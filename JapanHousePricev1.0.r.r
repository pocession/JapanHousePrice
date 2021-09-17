# Loading essential packages -----------------------------------------------------------------------------------------------------
library(rstudioapi)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(corrplot)
library(scales)
library(randomForest)
# Loading data into R -----------------------------------------------------------------------------------------------------
current_path = rstudioapi::getActiveDocumentContext()$path
dir <- setwd(dirname(current_path ))
files <- list.files(file.path(dir, "Raw"), pattern="*.csv")

# No is not a variable
Raw <- read.csv(file.path(dir,"Raw",files[37]), stringsAsFactors = F)
Raw <- Raw[,2:ncol(Raw)]

# Numeric data wrangling -----------------------------------------------------------------------------------------------------
# There are some works needs to be done before we start the analysis
# Let's check numeric data first and perform wrangling
# Year
Raw <- Raw %>%
  separate(Transaction.period, c("quarter.1", "quarter.2", "Year"), sep = " ")

# Let's do some wrangling to make some non-numeirc variables be numeric
# Area.m.2, Nearest.station.Distance.minute., Total.floor.area.m.2., Year.of.construction, Year, should be numeric variable
# City code is not a numeric variable
Raw$Area.m.2. <- as.numeric(Raw$Area.m.2.)
Raw$Total.floor.area.m.2.<- as.numeric(Raw$Total.floor.area.m.2.)
Raw$Year.of.construction <- as.numeric(Raw$Year.of.construction)
Raw$Year <- as.numeric(Raw$Year)

# Nearest.station.Distance.minute.: needs more work to be a numeric variable
unique(Raw$Nearest.station.Distance.minute.)

index1 <- Raw$Nearest.station.Distance.minute. == "30-60minutes"
index2 <- Raw$Nearest.station.Distance.minute. == "1H-1H30"
index3 <- Raw$Nearest.station.Distance.minute. == "1H30-2H"
index4 <- Raw$Nearest.station.Distance.minute. == "2H-"
Raw$Nearest.station.Distance.minute.[index1] <- 60
Raw$Nearest.station.Distance.minute.[index2] <- 90
Raw$Nearest.station.Distance.minute.[index3] <- 120
Raw$Nearest.station.Distance.minute.[index4] <- 150 # This is a fake number, assign to house where it takes more than 120 minutes walking to nearest station  
Raw$Nearest.station.Distance.minute. <- as.numeric(Raw$Nearest.station.Distance.minute.)

unique(Raw$Frontage)

index5 <- Raw$Frontage == "50.0m or longer"
Raw$Frontage [index5] <- 50
Raw$Frontage <- as.numeric(Raw$Frontage)
# City.Town.Ward.Village.code is not a numeric variable
Raw$City.Town.Ward.Village.code <- as.factor(Raw$City.Town.Ward.Village.code)

numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
numericVarNames
cat('There are', length(numericVars), 'numeric variables')

# Dealing with missing values-----------------------------------------------------------------------------------------------------
# Check which variable contains missing values. missing values could be NA or "" (Blank).
blankcol <- which(sapply(Raw,function(x) any(x== "")))
NAcol <- which(colSums(is.na(Raw)) > 0)
missingcol<-union(names(blankcol),names(NAcol))
cat('There are', length(missingcol), 'variables containing missing values')
missingcol

# Complete the caculation of Transaction.price.Unit.price.m.2.
# And obtain how many NAs are left, should be 1216
Raw$Transaction.price.Unit.price.m.2. <- Raw$Transaction.price.total./Raw$Area.m.2.
length(which(is.na(Raw$Transaction.price.Unit.price.m.2.)))

# Check if NAs of the two variables are now all 1216
length(which(is.na(Raw$Transaction.price.Unit.price.m.2.))) & length(which(is.na(Raw$Area.m.2.)))

# Assign NAs to Region, Land Shape, USe, Purpose.of.Use, Building.structure, Layout
Raw$Region[which(Raw$Region == "")] <- NA
Raw$Land.shape[which(Raw$Land.shape == "")] <- NA
Raw$Use[which(Raw$Use == "")] <- NA
Raw$Purpose.of.Use[which(Raw$Purpose.of.Use == "")] <- NA
Raw$Building.structure[which(Raw$Building.structure == "")] <- NA
Raw$Layout[which(Raw$Layout == "")] <- NA

# Check whether NAs in Nearest.station.Distance.minute. are caused by missing values in Nearest.station.Distance.Name.
Raw %>% 
  filter(is.na(Nearest.station.Distance.minute.)) %>%
  group_by(Nearest.station.Name) %>%
  summarise(count = n())

Raw$Nearest.station.Name[which(is.na(Raw$Nearest.station.Distance.minute.) & Raw$Nearest.station.Name == "")] <- "No_station"

# Check again
Raw %>% 
  filter(is.na(Nearest.station.Distance.minute.)) %>%
  group_by(Nearest.station.Name) %>%
  summarise(count = n())

# Check if number of NAs in Frontage.road.Breadth.m.equals the number of "" in Frontage.road.Classification and "No facing road" in Frontage.road.Direction
length(which(is.na(Raw$Frontage.road.Breadth.m.))) & length(which(Raw$Frontage.road.Classification=="")) & length(which(Raw$Frontage.road.Direction=="No facing road"))

# Replace the NAs in Frontage.road.Breadth.m. with 0
Raw$Frontage.road.Breadth.m.[is.na(Raw$Frontage.road.Breadth.m.)] <- 0

# Assign Nas to Frontage.road.Classification and Frontage.road.Direction
Raw$Frontage.road.Classification[which(Raw$Frontage.road.Classification == "")] <- NA
Raw$Frontage.road.Direction[which(Raw$Frontage.road.Direction == "")] <- NA

# Check if number of NAs in Maximus.Building.Coverage.Ratio... and in Maximus.Floor.area.Ratio... are equal
length(which(is.na(Raw$Maximus.Building.Coverage.Ratio...))) & length(which(is.na(Raw$Maximus.Floor.area.Ratio...)))

# Check the NAs in Year.of.construction
Raw %>%
  filter(is.na(Year.of.construction)) %>%
  group_by(Type) %>%
  summarise(count = n())

# 1935 is a fake number to those house properties built before WWII.
Raw$Year.of.construction[which(Raw$Type == "Pre-owned Condominiums, etc.")] <- 1935
Raw$Year.of.construction[which(Raw$Type == "Residential Land(Land and Building)")] <- 1935
Raw$Year.of.construction <- as.numeric(Raw$Year.of.construction)

# Check the NAs in Year.of.construction again, all NAs should be obtained only in land properties
Raw %>%
  filter(is.na(Year.of.construction)) %>%
  group_by(Type) %>%
  summarise(count = n())

# Assigns NAs to Renovation and Transactional.factors
Raw$Renovation[which(Raw$Renovation == "")] <- NA
Raw$Transactional.factors[which(Raw$Transactional.factors == "")] <- NA

# Factorize data---------------------------------------------------------------------------------------------------------
# Factorize non-ordinal variables first
Factors <- c("Type","Region","City.Town.Ward.Village.code","Prefecture","City.Town.Ward.Village","Area","Nearest.station.Name",
             "Land.shape","Building.structure","Use","Purpose.of.Use","Frontage.road.Direction","Frontage.road.Classification",
             "City.Planning","Renovation")
Raw[Factors]<-lapply(Raw[Factors],factor)

# Factorize ordinal variables
Raw$quarter.1 <- factor(Raw$quarter.1,order = TRUE, levels = c("1st", "2nd", "3rd", "4th"))
Raw$Layout <- factor(Raw$Layout, order = TRUE, levels = c("1R","1K","1DK","1LDK","2K","2K+S","2DK","2DK+S","2LDK","2LDK+S",
                                                          "3K","3DK","3LDK","3LDK+S","4DK","4LDK","5DK","5LDK","6DK"))


str(Raw)
# Drop some variables ----------------------------------------------------------------------------------------------------
# Some variables are not required for the further analysis.
drops <- c("Transaction.price.total.","quarter.2","Renovation","Transactional.factors")
Raw <- Raw[ , !(names(Raw) %in% drops)]

numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(Raw, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

# Separate train and test data -----------------------------------------------------------------------------------------------------
# We only focus on those real estate used for house
Raw <- Raw %>% 
  filter(Use == "House")

## 75% of the sample size
smp_size <- floor(0.75 * nrow(Raw))

## set the seed to make your partition reproducible
set.seed(120)
train_ind <- sample(seq_len(nrow(Raw)), size = smp_size)

train <- Raw[train_ind, ]
test <- Raw[-train_ind, ]

all <- Raw

# Save wrangled, train and test data into csv files
write.csv(train,file.path(dir,"Wrangled","train.csv"))
write.csv(test,file.path(dir,"Wrangled","test.csv"))
write.csv(Raw,file.path(dir,"Wrangled","wrangled.csv"))



# Identifying numeric varialbes correlated with dependent variables ------------------------------------------------------
# Visualize the transaction price-----------------------------------------------------------------------------------------
ggsave(file.path(dir,"Result","Transaction.price.Unit.price.m2.png"))
ggplot(data=train[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=Transaction.price.Unit.price.m.2.)) +
  geom_histogram(fill="blue", binwidth = 10000) 
dev.off()

# Identify the correlation between Transaction.price.Unit.price.m.2.and numeric variables--------------------------------- 
summary_Transaction.price.Unit.price.m2. <- summary(all$Transaction.price.Unit.price.m.2.)
summary_Transaction.price.Unit.price.m2.

numericVars <- which(sapply(all, is.numeric))
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

# Sort on decreasing correlations with Transaction.price.Unit.price.m.2.
cor_sorted <- as.matrix(sort(cor_numVar[,'Transaction.price.Unit.price.m.2.'], decreasing = TRUE))
# Select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

png(file.path(dir,"Result","CorrelationvarNum.png"))
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
dev.off()

# Identify the correlation between Transaction.price.Unit.price.m.2. and all variables by Random Forest----------------------
RF<-na.action(all)
set.seed(2018)
quick_RF <- randomForest(x=RF[1:5000,-c(6,7,11,17)], y=RF$Transaction.price.Unit.price.m.2.[1:5000], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")