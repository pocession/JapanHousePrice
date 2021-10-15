# 4.1 Loading essential packages -----------------------------------------------------------------------------------------------------
library(rstudioapi)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(corrplot)
library(scales)
library(randomForest)
library(psych)
library(caret)
library(glmnet)
# 4.2 Loading data into R -----------------------------------------------------------------------------------------------------
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

# We don't analyze data containing no house.
Raw <- Raw %>%
  filter(!(Type %in% c("Agricultural Land","Forest Land","Residential Land(Land Only)")))

# Assign "No_information" to NAs.
Raw$Region[which(Raw$Region == "")] <- "No_information"

# Identify data that have NAs in both Nearest.station.Distance.minute. and Nearest.station.Distance.Name.
# Exclude data that have nearest station but does not contain distance information
Raw %>% 
  filter(is.na(Nearest.station.Distance.minute.)) %>%
  group_by(Nearest.station.Name) %>%
  summarise(count = n())

Raw$Nearest.station.Name[which(is.na(Raw$Nearest.station.Distance.minute.) & Raw$Nearest.station.Name == "")] <- "No_station"
Raw$Nearest.station.Distance.minute.[which(Raw$Nearest.station.Name == "No_station")] <- 999
Raw <- Raw %>%
  filter(!is.na(Nearest.station.Distance.minute.))

# Check again
Raw %>% 
  filter(Nearest.station.Distance.minute. == 999) %>%
  group_by(Nearest.station.Name) %>%
  summarise(count = n())

# Complete the caculation of Transaction.price.Unit.price.m.2.
# And obtain how many NAs are left, should be 1216
Raw$Transaction.price.Unit.price.m.2. <- Raw$Transaction.price.total./Raw$Area.m.2.
length(which(is.na(Raw$Transaction.price.Unit.price.m.2.)))

# Check if the numbers of NAs in Transaction.price.Unit.price.m.2. and Area.m.2. equal to each other.
# Remove data that contains missing values in both variables
length(which(is.na(Raw$Transaction.price.Unit.price.m.2.))) & length(which(is.na(Raw$Area.m.2.)))
Raw <- Raw %>%
  filter(!is.na(Raw$Transaction.price.Unit.price.m.2.) & !is.na(Raw$Area.m.2.))

# Check if number of NAs in Frontage.road.Breadth.m.equals the number of "" in Frontage.road.Classification and "No facing road" in Frontage.road.Direction
length(which(is.na(Raw$Frontage.road.Breadth.m.))) & length(which(Raw$Frontage.road.Classification=="")) & length(which(Raw$Frontage.road.Direction=="No facing road"))

# Assign "No_information" to Frontage.road.Classification and Frontage.road.Direction
# Replace the NAs in Frontage.road.Breadth.m. with 0
Raw$Frontage.road.Classification[which(Raw$Frontage.road.Classification == "")] <- "No_information"
Raw$Frontage.road.Direction[which(Raw$Frontage.road.Direction == "")] <- "No_information"
Raw$Frontage.road.Breadth.m.[is.na(Raw$Frontage.road.Breadth.m.)] <- 0

# Assign "No_information" to those variables
Raw$Renovation[which(Raw$City.Planing == "")] <- "No_information"
Raw$Renovation[which(Raw$Renovation == "")] <- "No_information"
Raw$Transactional.factors[which(Raw$Transactional.factors == "")] <- "No_information"
Raw$City.Planning[which(Raw$City.Planning == "")] <- "No_information"
Raw$Layout[which(Raw$Layout == "")] <- "No_information"
Raw$Land.shape[which(Raw$Land.shape == "")] <- "No_information"
Raw$Building.structure[which(Raw$Building.structure == "")] <- "No_information"
Raw$Use[which(Raw$Use == "")] <- "No_information"
Raw$Purpose.of.Use[which(Raw$Purpose.of.Use == "")] <- "No_information"

# Visualize Frontage and unit price
p <- ggplot(Raw, aes(Frontage,Transaction.price.Unit.price.m.2.)) + geom_point() + ylim(0,1e+6)
# Use vars() to supply faceting variables:
p + facet_wrap(vars(Raw$Type))
ggsave(file.path(dir,"Result","Unit_price_Frontage.png"))
dev.off()

# Assign 0 to all NAs
Raw$Frontage[is.na(Raw$Frontage)] <- 0
p <- ggplot(Raw, aes(Frontage,Transaction.price.Unit.price.m.2.)) + geom_point() + ylim(0,1e+6)
# Use vars() to supply faceting variables:
p + facet_wrap(vars(Raw$Type))
ggsave(file.path(dir,"Result","Unit_price_Frontage2.png"))
dev.off()
# Visualize Total.floor.area.m.2. and unit price
p <- ggplot(Raw, aes(Total.floor.area.m.2.,Transaction.price.Unit.price.m.2.)) + geom_point() + ylim(0,1e+6)
# Use vars() to supply faceting variables:
p + facet_wrap(vars(Raw$Type))
ggsave(file.path(dir,"Result","Unit_price_Floor_Area.png"))
dev.off()

# Assign 0 to all NAs
Raw$Total.floor.area.m.2.[is.na(Raw$Total.floor.area.m.2.)] <- 0
p <- ggplot(Raw, aes(Total.floor.area.m.2.,Transaction.price.Unit.price.m.2.)) + geom_point() + ylim(0,1e+6)
# Use vars() to supply faceting variables:
p + facet_wrap(vars(Raw$Type))
ggsave(file.path(dir,"Result","Unit_price_Floor_Area2.png"))
dev.off()

# Check if number of NAs in Maximus.Building.Coverage.Ratio... and in Maximus.Floor.area.Ratio... are equal
length(which(is.na(Raw$Maximus.Building.Coverage.Ratio...))) & length(which(is.na(Raw$Maximus.Floor.area.Ratio...)))
# Assign 0 to these two variables.
Raw$Maximus.Building.Coverage.Ratio...[is.na(Raw$Maximus.Building.Coverage.Ratio...)] <- 0
Raw$Maximus.Floor.area.Ratio...[is.na(Raw$Maximus.Floor.area.Ratio...)] <- 0

# Check the NAs in Year.of.construction
Raw %>%
  filter(is.na(Year.of.construction)) %>%
  group_by(Type) %>%
  summarise(count = n())

# Remove the house without information of Year.of.construction

# 1935 is a fake number to those house properties built before WWII.
Raw$Year.of.construction[which(is.na(Raw$Year.of.construction))] <- 1935

# Check whether we have any variables containing missing values now
blankcol <- which(sapply(Raw,function(x) any(x== "")))
NAcol <- which(colSums(is.na(Raw)) > 0)
missingcol<-union(names(blankcol),names(NAcol))
cat('There are', length(missingcol), 'variables containing missing values')
missingcol

# Factorize data---------------------------------------------------------------------------------------------------------
# Factorize non-ordinal variables first
Factors <- c("Type","Region","City.Town.Ward.Village.code","Prefecture","City.Town.Ward.Village","Area","Nearest.station.Name",
             "Land.shape","Building.structure","Use","Purpose.of.Use","Frontage.road.Direction","Frontage.road.Classification",
             "City.Planning","Renovation")
Raw[Factors]<-lapply(Raw[Factors],factor)

# Factorize ordinal variables
Raw$quarter.1 <- factor(Raw$quarter.1,order = TRUE, levels = c("1st", "2nd", "3rd", "4th"))
Raw$Layout <- factor(Raw$Layout, order = TRUE, levels = c("No_information","1R","1K","1DK","1LDK","2K","2K+S","2DK","2DK+S","2LDK","2LDK+S",
                                                          "3K","3DK","3LDK","3LDK+S","4DK","4LDK","5DK","5LDK","6DK"))
str(Raw)
# Drop some variables ----------------------------------------------------------------------------------------------------
drops <- c("Transaction.price.total.","Area.m.2.","quarter.2","Renovation","Transactional.factors")
Raw <- Raw[ , !(names(Raw) %in% drops)]

numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(Raw, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

# Visualize the transaction price-----------------------------------------------------------------------------------------
# We only consier house price
all <- Raw %>%
  filter(grepl("House",Use))

ggplot(data=all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=Transaction.price.Unit.price.m.2.)) +
  geom_histogram(fill="blue", binwidth = 10000)
ggsave(file.path(dir,"Result","Transaction.price.Unit.price.m2.png"))
dev.off()

summary_Transaction.price.Unit.price.m2. <- summary(all$Transaction.price.Unit.price.m.2.)
summary_Transaction.price.Unit.price.m2.

# Identify the correlation between Transaction.price.Unit.price.m.2.and numeric variables--------------------------------- 
numericVars <- which(sapply(all, is.numeric))
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

# Sort on decreasing correlations with Transaction.price.Unit.price.m.2.
cor_sorted <- as.matrix(sort(cor_numVar[,'Transaction.price.Unit.price.m.2.'], decreasing = TRUE))
# Display all correlation
Cor <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0)))
cor_numVar <- cor_numVar[Cor, Cor]

png(file.path(dir,"Result","CorrelationvarNum.png"))
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
dev.off()

# Drop Maximus.Building.Coverage.Ratio...
all <- all[,-22]

# Drop unused levels
all$Use <- factor(all$Use)

# Identify the correlation between Transaction.price.Unit.price.m.2. and all variables by Random Forest----------------------
RF<- all %>%
  filter(!is.na(Transaction.price.Unit.price.m.2.))
set.seed(2018)

# randomForest can not handle variables containing more than 53 level
# select the top 50 Areas
Frequency_Area <- all %>%
  group_by(Area) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

RF <- all %>%
  group_by(Area) %>%
  filter(n()>55)

RF$Area <- factor(RF$Area)
RF$Nearest.station.Name <- factor(RF$Nearest.station.Name)
RF<-as.data.frame(RF)
# Run the random Forest
quick_RF <- randomForest(x=RF[,-c(10)], y=RF[,10], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")
ggsave(file.path(dir,"Result","randomForest.png"))
dev.off()

# Feature variables ----------------------------------------------------------------------------------------------------
# Transform to year.of.Constuction to Age
all$Age <- as.numeric(all$Year - all$Year.of.construction)

# There are some house sold earlier than it's construction so that the age is negative
# I think they are new houses so I change the negative age to 0
# Assign "No_information" to those variables
all$Age[which(all$Age < 0)] <- 0

cor_age_price <- cor(all$Transaction.price.Unit.price.m.2., all$Age)
Price_age <- ggplot(data=all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=Age, y=Transaction.price.Unit.price.m.2.))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 5000000, by=1000000), labels = comma) +
  annotate("text",label = cor_age_price,
    x = 30, y = 1000000, size = 8, colour = "red")
Price_age
dev.off()
all <- all %>%
  filter(Transaction.price.Unit.price.m.2.< 1000000)
cor_age_price2 <- cor(all$Transaction.price.Unit.price.m.2., all$Age)
Price_age2 <- ggplot(data=all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=Age, y=Transaction.price.Unit.price.m.2.))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 5000000, by=1000000), labels = comma) +
  annotate("text",label = cor_age_price2,
           x = 30, y = 500000, size = 8, colour = "red")
Price_age2
ggsave(file.path(dir,"Result","Unit_price_age2.png"))
dev.off()

# New
all$IsNew <- ifelse(all$Year==all$Year.of.construction, 1, 0)
table(all$IsNew)

ggplot(all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=as.factor(IsNew), y=Transaction.price.Unit.price.m.2.)) +
  geom_bar(stat='summary', fun = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=60000, linetype="dashed")
ggsave(file.path(dir,"Result","Unit_price_IsNew.png"))
dev.off()

# Binning Station
Station <- ggplot(all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=reorder(Nearest.station.Name, Transaction.price.Unit.price.m.2., FUN=median), y=Transaction.price.Unit.price.m.2.)) +
  geom_bar(stat="summary", fun = "median", fill='blue') + labs(x='Nearest Station', y='Median Unit price') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5)) +
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=2) +
  geom_hline(yintercept=c(25000,75000,125000,175000), linetype="dashed", color = "red") 
Station
ggsave(file.path(dir,"Result","Unit_price_station.png"))
dev.off()

# Regroup Nearest.station.Name based on their median price
all <- all %>%
  group_by(Nearest.station.Name) %>%
  mutate(Stationgroup = case_when(median(Transaction.price.Unit.price.m.2.) >= 175000 ~ '5',
                                  median(Transaction.price.Unit.price.m.2.) >= 125000 & median(Transaction.price.Unit.price.m.2.) < 175000 ~ '4',
                                  median(Transaction.price.Unit.price.m.2.) >= 75000  & median(Transaction.price.Unit.price.m.2.) < 125000 ~ '3',
                                  median(Transaction.price.Unit.price.m.2.) >= 25000  & median(Transaction.price.Unit.price.m.2.) < 75000 ~ '2',
                                  median(Transaction.price.Unit.price.m.2.) >= 0      & median(Transaction.price.Unit.price.m.2.) < 25000 ~ '1'))

# Check again
Station2 <- ggplot(all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=reorder(Stationgroup, Transaction.price.Unit.price.m.2., FUN=median), y=Transaction.price.Unit.price.m.2.)) +
  geom_bar(stat="summary", fun = "median", fill='blue') + labs(x='Nearest Station', y='Median Unit price') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) +
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=5) +
  geom_hline(yintercept=c(25000,75000,125000,175000), linetype="dashed", color = "red") 
Station2
ggsave(file.path(dir,"Result","Unit_price_station2.png"))
dev.off()

all %>% 
  filter(Stationgroup == 5) %>% 
  group_by(Nearest.station.Name) %>% 
  summarise(count=n())

# Binning Area
Area <- ggplot(all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=reorder(Area, Transaction.price.Unit.price.m.2., FUN=median), y=Transaction.price.Unit.price.m.2.)) +
  geom_bar(stat="summary", fun = "median", fill='blue') + labs(x='Area', y='Median Unit price') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2)) +
  scale_y_continuous(breaks= seq(0, 500000, by=100000), labels = comma) +
  geom_hline(yintercept=c(100000,200000,300000,400000), linetype="dashed", color = "red") 
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=2) 
Area
ggsave(file.path(dir,"Result","Unit_price_area.png"))
dev.off()

all <- all %>%
  group_by(Area) %>%
  mutate(Areagroup = case_when(median(Transaction.price.Unit.price.m.2.) >= 400000 ~ '5',
                                  median(Transaction.price.Unit.price.m.2.) >= 300000 & median(Transaction.price.Unit.price.m.2.) < 400000 ~ '4',
                                  median(Transaction.price.Unit.price.m.2.) >= 200000  & median(Transaction.price.Unit.price.m.2.) < 300000 ~ '3',
                                  median(Transaction.price.Unit.price.m.2.) >= 100000  & median(Transaction.price.Unit.price.m.2.) < 200000 ~ '2',
                                  median(Transaction.price.Unit.price.m.2.) >= 0      & median(Transaction.price.Unit.price.m.2.) < 100000 ~ '1'))
# Check again
Area2 <- ggplot(all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=reorder(Areagroup, Transaction.price.Unit.price.m.2., FUN=median), y=Transaction.price.Unit.price.m.2.)) +
  geom_bar(stat="summary", fun = "median", fill='blue') + labs(x='Area', y='Median Unit price') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  scale_y_continuous(breaks= seq(0, 500000, by=100000), labels = comma) +
  geom_hline(yintercept=c(100000,200000,300000,400000), linetype="dashed", color = "red") 
geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=10) 
Area2
ggsave(file.path(dir,"Result","Unit_price_area2.png"))
dev.off()

all %>% 
  filter(Areagroup == 5) %>% 
  group_by(Area) %>% 
  summarise(count=n())

# Frontage
cor_frontage_price <- cor(all$Transaction.price.Unit.price.m.2., all$Frontage)
Price_frontage <- ggplot(data=all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=Frontage, y=Transaction.price.Unit.price.m.2.))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 5000000, by=1000000), labels = comma) +
  annotate("text",label = cor_frontage_price,
           x = 30, y = 1000000, size = 8, colour = "red")
Price_frontage
ggsave(file.path(dir,"Result","Unit_price_frontage.png"))
dev.off()
# Prepare data for modeling --------------------------------------------------------------------------------------------------------
# Separate train and test data -----------------------------------------------------------------------------------------------------
# 75% of the sample size
smp_size <- floor(0.75 * nrow(all))

## set the seed to make your partition reproducible
set.seed(108)
train_ind <- sample(seq_len(nrow(all)), size = smp_size)

train <- all[train_ind, ]
test <- all[-train_ind, ]
test$Transaction.price.Unit.price.m.2. <- NA

all <- rbind(train, test)

# Save all, train and test data into csv files
write.csv(train,file.path(dir,"Wrangled","train.csv"))
write.csv(test,file.path(dir,"Wrangled","test.csv"))
write.csv(all,file.path(dir,"Wrangled","all.csv"))
# Drop unused variables
dropVars <- c('Year.of.construction', 'Year', 'Nearest.station.Name', 'Area','Prefecture', 'Use')

all <- all[,!(names(all) %in% dropVars)]
all$Stationgroup <- as.numeric(all$Stationgroup)
all$Areagroup <- as.numeric(all$Areagroup)


# Check how many numeric and factor variables we have now
# Remove non-numeric variables
numericVars <- which(sapply(all, is.numeric))
numericVarNames <- names(numericVars)
numericVarNames <- numericVarNames[!(numericVarNames %in% c('Transaction.price.Unit.price.m.2.','IsNew','Stationgroup','Areagroup'))]
DFnumeric <- all[, names(all) %in% numericVarNames]
DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'Transaction.price.Unit.price.m.2.']
cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables.')

# log transform the numberic variables if their distributions are skewed
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

# one-hot coding
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

# check if some values are absent in the test set
ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$Transaction.price.Unit.price.m.2.),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])

# check if some values are absent in the train set
ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$Transaction.price.Unit.price.m.2.),]),])==0)
colnames(DFdummies[ZerocolTrain])

DFdummies <- DFdummies[,-ZerocolTrain] #removing predictor

fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$Transaction.price.Unit.price.m.2.),]),])<10)
colnames(DFdummies[fewOnes])

DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe 

# Check the skewness of responsive variable
skew(all$Transaction.price.Unit.price.m.2.)

png(file.path(dir,"Result","skewness_unit_price.png"))
qqnorm(all$Transaction.price.Unit.price.m.2.)
qqline(all$Transaction.price.Unit.price.m.2.)
dev.off()

all$Transaction.price.Unit.price.m.2. <- log(all$Transaction.price.Unit.price.m.2.) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$Transaction.price.Unit.price.m.2.)
png(file.path(dir,"Result","skewness_unit_price.2.png"))
qqnorm(all$Transaction.price.Unit.price.m.2.)
qqline(all$Transaction.price.Unit.price.m.2.)
dev.off()

# Composing train and test dataset
train1 <- combined[!is.na(all$Transaction.price.Unit.price.m.2.),]
test1 <- combined[is.na(all$Transaction.price.Unit.price.m.2.),]

# Modeling, lasso -----------------------------------------------------------------------------------------------------------
set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
lasso_mod <- train(x=train1, y=all$Transaction.price.Unit.price.m.2.[!is.na(all$Transaction.price.Unit.price.m.2.)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune
min(lasso_mod$results$RMSE)
max(lasso_mod$results$Rsquared)

LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)

lassoVarImp <- varImp(lasso_mod,scale=T)
lassoImportance <- lassoVarImp$importance
imp_lasso <- data.frame(Variables = row.names(lassoImportance), MSE = lassoImportance[,1])

ggplot(imp_lasso[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")
ggsave(file.path(dir,"Result","mse_lasso.png"))
dev.off()


# Modeling, ridge -----------------------------------------------------------------------------------------------------------
set.seed(27042018)
my_control_ridge <-trainControl(method="cv", number=5)
ridgeGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
ridge_mod <- train(x=train1, y=all$Transaction.price.Unit.price.m.2.[!is.na(all$Transaction.price.Unit.price.m.2.)], method='glmnet', trControl= my_control_ridge, tuneGrid=ridgeGrid) 
ridge_mod$bestTune
min(ridge_mod$results$RMSE)
max(ridge_mod$results$Rsquared)

ridgePred <- predict(ridge_mod, test1)
predictions_ridge <- exp(ridgePred) #need to reverse the log to the real values
head(predictions_ridge)
