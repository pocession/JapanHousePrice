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
library(mice)
library(ranger)
library(gridExtra)
# 4 Loading and exploring data ------
## 4.2 Loading data into R -----------------------------------------------------------------------------------------------------
period <- "20053_20212_e"
dir <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
files <- list.files(file.path(dir, "Raw"), pattern="*.csv")
Raw <- read.csv(file.path(dir,"Raw",files[14]), stringsAsFactors = F)
cat("There are", ncol(Raw), "variables")

## 4.3 Data wrangling ------
### 4.3.1 Numeric variables ######
str(Raw)
# Check the distribution of the dependent variable, Transaction.price.total.
# summary(log10(Raw$Transaction.price.total.))
# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(x=log10(Transaction.price.total.))) +
#   geom_histogram(fill="blue", binwidth = 0.05) +
#   scale_x_continuous(breaks= seq(0, 11, by=1), labels = comma)
# ggsave(file.path(dir,"Result","Price.png"))
# dev.off()

# Year
Raw <- Raw %>%
  separate(Transaction.period, c("quarter.1", "quarter.2", "Year"), sep = " ")

# Area.m.2, Nearest.station.Distance.minute., Total.floor.area.m.2., Year.of.construction, Year, should be numeric variable
# City code is not a numeric variable
Raw$Area.m.2. <- as.numeric(Raw$Area.m.2.)
Raw$Total.floor.area.m.2.<- as.numeric(Raw$Total.floor.area.m.2.)
Raw$Year.of.construction <- as.numeric(Raw$Year.of.construction)
Raw$Year <- as.numeric(Raw$Year)

# Nearest.station.Distance.minute.: needs more work to be a numeric variable
unique(Raw$Nearest.station.Distance.minute.)

Raw %>%
  filter(Nearest.station.Distance.minute. == "2H-") %>%
  group_by(Type) %>%
  summarise(n=n())

index1 <- Raw$Nearest.station.Distance.minute. == "30-60minutes"
index2 <- Raw$Nearest.station.Distance.minute. == "1H-1H30"
index3 <- Raw$Nearest.station.Distance.minute. == "1H30-2H"
index4 <- Raw$Nearest.station.Distance.minute. == "2H-"
index5 <- Raw$Nearest.station.Distance.minute. == ""
Raw$Nearest.station.Distance.minute.[index1] <- 45
Raw$Nearest.station.Distance.minute.[index2] <- 75
Raw$Nearest.station.Distance.minute.[index3] <- 105
Raw$Nearest.station.Distance.minute.[index4] <- 135 
Raw$Nearest.station.Distance.minute.[index5] <- 165
Raw$Nearest.station.Distance.minute. <- as.numeric(Raw$Nearest.station.Distance.minute.)

# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(x=as.factor(Nearest.station.Distance.minute.), y=log10(Transaction.price.total.)))+
#   geom_boxplot(col='blue') + labs(x='Nearest.station.Distance.minute.') +
#   scale_y_continuous(breaks= seq(0, 11, by=1), labels = comma)
# ggsave(file.path(dir,"Result","Price_station_distance.png"))
# dev.off()

unique(Raw$Frontage)
index6 <- Raw$Frontage == "50.0m or longer"
Raw$Frontage [index6] <- 50
Raw$Frontage <- as.numeric(Raw$Frontage)

# City.Town.Ward.Village.code is not a numeric variable
Raw$City.Town.Ward.Village.code <- as.factor(Raw$City.Town.Ward.Village.code)

numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
numericVarNames
cat('There are', length(numericVars), 'numeric variables')

### 4.3.2 A glimpse of numeric variables ######
# We don't analyze data containing no house.
Raw <- Raw %>%
  filter(!(Type %in% c("Agricultural Land","Forest Land","Residential Land(Land Only)")))

# Have an overveiw about which numeric variable is important
# all_numVar <- Raw[, numericVars]
# cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
# # sort on decreasing correlations with SalePrice
# cor_sorted <- as.matrix(sort(cor_numVar[,'Transaction.price.total.'], decreasing = TRUE))
# CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))
# cor_numVar <- cor_numVar[CorHigh, CorHigh]
# png(file.path(dir,"Result","CorrelationvarNum.png"))
# corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
# dev.off()

# Check the relationship between "Total.floor.area.m.2." and "Transaction.price.total.".
# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(x=Total.floor.area.m.2., y=log10(Transaction.price.total.)))+
#   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
#   labs(x = "Total.floor.area.m.2", y = "log(Transaction.price.total.)") +
#   ylim(0,10)
# ggsave(file.path(dir,"Result","Price_floor_area0.png"))
# dev.off()

# 5 Dealing with missing values ------
# Check which variable contains missing values. missing values could be NA or "" (Blank).
blankcol <- which(sapply(Raw,function(x) any(x== "")))
NAcol <- which(colSums(is.na(Raw)) > 0)
missingcol<-union(names(blankcol),names(NAcol))
cat('There are', length(missingcol), 'variables containing missing values')
missingcol
# png(file.path(dir,"Result","Missing.png"))
# md.pattern(Raw, rotate.names = TRUE)
# dev.off()

# Assign "No_information" to NAs.
Raw$Region[which(Raw$Region == "")] <- "No_information"

# Identify data that have NAs in both Nearest.station.Distance.minute. and Nearest.station.Distance.Name.
# Note NAs in Nearest.station.Distance.minute. has been transformed to 165
# Exclude data that have nearest station but does not contain distance information
Raw %>% 
  filter(Nearest.station.Distance.minute. == 165) %>%
  group_by(Nearest.station.Name) %>%
  summarise(count = n())

Raw$Nearest.station.Name[which(Raw$Nearest.station.Distance.minute. == 165 & Raw$Nearest.station.Name == "")] <- "No_station"

# Check again
Raw %>% 
  filter(Nearest.station.Distance.minute. == 165) %>%
  group_by(Nearest.station.Name) %>%
  summarise(count = n())

# Check if number of NAs in Frontage.road.Breadth.m.equals the number of "" in Frontage.road.Classification and "No facing road" in Frontage.road.Direction
length(which(is.na(Raw$Frontage.road.Breadth.m.))) & length(which(Raw$Frontage.road.Classification=="")) & length(which(Raw$Frontage.road.Direction=="No facing road"))

# Assign "No_information" to Frontage.road.Classification and Frontage.road.Direction
# Replace the NAs in Frontage.road.Breadth.m. with 0
Raw$Frontage.road.Classification[which(Raw$Frontage.road.Classification == "")] <- "No_information"
Raw$Frontage.road.Direction[which(Raw$Frontage.road.Direction == "")] <- "No_information"
Raw$Frontage.road.Breadth.m.[is.na(Raw$Frontage.road.Breadth.m.)] <- 0

# Frontage
Raw %>%
  filter(is.na(Frontage)) %>%
  group_by(Frontage.road.Breadth.m.) %>%
  summarise(n=n())

# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(x=Frontage, y=log10(Transaction.price.total.)))+
#   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
#   labs(x = "Frontage", y = "log(Transaction.price.total.)") +
#   ylim(0,10)
# ggsave(file.path(dir,"Result","Price_frontage.png"))
# dev.off()

# Total.floor.area.m.2.
Raw$Total.floor.area.m.2. <- ifelse(is.na(Raw$Total.floor.area.m.2.), Raw$Area.m.2., Raw$Total.floor.area.m.2.)

# check
length(which(is.na(Raw$Total.floor.area.m.2.))) & length(which(is.na(Raw$Area.m.2.)))

# Omit data containing missing value in both Area and floor area
Raw <- Raw %>%
  filter(!is.na(Total.floor.area.m.2.) & !is.na(Area.m.2.))

# visualize the price and floor area again
# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(x=Total.floor.area.m.2., y=log10(Transaction.price.total.)))+
#   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
#   labs(x = "Total.floor.area.m.2.", y = "log(Transaction.price.total.)") +
#   ylim(0,10)
# ggsave(file.path(dir,"Result","Price_floor_area1.png"))
# dev.off()

# Assign "No_information" to those variables
Raw$Area[which(Raw$Area == "")] <- "No_information"
Raw$Renovation[which(Raw$City.Planing == "")] <- "No_information"
Raw$Renovation[which(Raw$Renovation == "")] <- "No_information"
Raw$Transactional.factors[which(Raw$Transactional.factors == "")] <- "No_information"
Raw$City.Planning[which(Raw$City.Planning == "")] <- "No_information"
Raw$Layout[which(Raw$Layout == "")] <- "No_information"
Raw$Land.shape[which(Raw$Land.shape == "")] <- "No_information"
Raw$Building.structure[which(Raw$Building.structure == "")] <- "No_information"
Raw$Use[which(Raw$Use == "")] <- "No_information"
Raw$Purpose.of.Use[which(Raw$Purpose.of.Use == "")] <- "No_information"

# Check if number of NAs in Maximus.Building.Coverage.Ratio... and in Maximus.Floor.area.Ratio... are equal
length(which(is.na(Raw$Maximus.Building.Coverage.Ratio...))) & length(which(is.na(Raw$Maximus.Floor.area.Ratio...)))

# Visualize Maximus.Building.Coverage.Ratio... and price
# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(Maximus.Building.Coverage.Ratio..., y=log10(Transaction.price.total.)))+
#   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
#   labs(x = "Max Building coverage ratio", y = "log(Transaction.price.total.)") +
#   ylim(0,10)
# ggsave(file.path(dir,"Result","Price_maxi_building_coverage.png"))
# dev.off()

# Visualize Maximus.Floor.area.Ratio... and price
# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(Maximus.Floor.area.Ratio..., y=log10(Transaction.price.total.)))+
#   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
#   labs(x = "Maximus.Floor.area.Ratio...", y = "log(Transaction.price.total.)") +
#   ylim(0,10)
# ggsave(file.path(dir,"Result","Price_maxi_floor_coverage.png"))
# dev.off()

# Check the NAs in Year.of.construction
Raw %>%
  filter(is.na(Year.of.construction)) %>%
  group_by(Type) %>%
  summarise(count = n())

# Visualize Year.of.construction and price
# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(Year.of.construction, y=log10(Transaction.price.total.)))+
#   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
#   labs(x = "Year.of.construction", y = "log(Transaction.price.total.)") +
#   ylim(0,10)
# ggsave(file.path(dir,"Result","Price_year_construction.png"))
# dev.off()
# Omit data contain missing values
Raw <- Raw %>%
  filter(!is.na(Year.of.construction))


# Drop some variables
drops <- c("Transaction.price.Unit.price.m.2.","Maximus.Building.Coverage.Ratio...","Maximus.Floor.area.Ratio...","Frontage","Renovation","Transactional.factors","quarter.2")
Raw <- Raw[ , !(names(Raw) %in% drops)]

# Check whether we have any variables containing missing values now
blankcol <- which(sapply(Raw,function(x) any(x== "")))
NAcol <- which(colSums(is.na(Raw)) > 0)
missingcol<-union(names(blankcol),names(NAcol))
cat('There are', length(missingcol), 'variables containing missing values')
missingcol

# Separate train and test data -----------------------------------------------------------------------------------------------------
# 75% of the sample size
smp_size <- floor(0.75 * nrow(Raw))

## set the seed to make your partition reproducible
set.seed(108)
train_ind <- sample(seq_len(nrow(Raw)), size = smp_size)

train <- Raw[train_ind, ]
test <- Raw[-train_ind, ]

# write.csv(Raw,file.path(dir, "Wrangled",paste("Kanagawa",period,".csv")))
# write.csv(train,file.path(dir, "Wrangled",paste("Kanagawa",period,"train.csv")))
# write.csv(test,file.path(dir, "Wrangled",paste("Kanagawa",period,"test.csv")))
# 6 Identify and visualize important variables (features) ------
## 6.1 Factorize character variables ######

# Re-assign the train dataset to Raw
# Drop the No and Prefecture variable
Raw <- train
Raw <- select(Raw, -c("No","Prefecture"))
Factors <- c("Type","Region","City.Town.Ward.Village.code","City.Town.Ward.Village","Area","Nearest.station.Name",
             "Layout","Land.shape","Building.structure","Use","Purpose.of.Use","Frontage.road.Direction",
             "Frontage.road.Classification","City.Planning")

Raw[Factors]<-lapply(Raw[Factors],factor)

### 6.2 Identify important variables by random forest ######
# RF<-as.data.frame(Raw)
# # Run the random Forest
# quick_RF <- ranger(Transaction.price.total. ~ ., data = RF, num.trees=100,importance='permutation')
# imp_RF <- importance(quick_RF)
# imp_DF <- data.frame(Variables = names(imp_RF), Importance = imp_RF)
# imp_DF$Variance <- 100 * (imp_DF$Importance / sum(imp_DF$Importance))
# imp_DF <- imp_DF[order(imp_DF$Variance, decreasing = TRUE),]
# 
# ggplot(imp_DF, aes(x=reorder(Variables, Importance), y=Variance, fill=Variance)) +
#   geom_bar(stat = 'identity') +
#   labs(x = 'Variables', y= '% increase if variable is randomly permutated') +
#   coord_flip() + theme(legend.position="none")
# ggsave(file.path(dir,"Result","randomForest.png"))
# dev.off()

### 6.3 Visualization ######
#### Total.floor.area.m.2., Area.m.2.
cor_floor_area_matrix <- Raw %>%
  select(Area.m.2.,Total.floor.area.m.2.,Transaction.price.total.)
cor_floor_area <- cor(cor_floor_area_matrix, use="pairwise.complete.obs")

# Visualize price, floor area and area
# as same as before

#### Year.of.Construction, Year ######
# p_year_construction <- ggplot(data=Raw, aes(x=Year.of.construction)) +
#   geom_histogram(stat='count')
# p_year <- ggplot(data=Raw, aes(x=as.factor(Year))) +
#   geom_histogram(stat='count')
# grid.arrange(p_year_construction,p_year, nrow=2)
# g <- arrangeGrob(p_year_construction,p_year, nrow=2)
# ggsave(file.path(dir,"Result","Year.png"),g)
# dev.off()

#### Building.structure ######
# p_structure<- ggplot(Raw[!is.na(Raw$Transaction.price.total.),], aes(x=reorder(Building.structure, log10(Transaction.price.total.), FUN=median), y=log10(Transaction.price.total.))) +
#   geom_bar(stat="summary", fun = "median", fill='blue') + labs(x='Building structure', y='log10 median price') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10)) +
#   ylim(0,10) +
#   geom_label(stat = "count", aes(label = ..count.., y = ..count../1000000), size=3)
# p_structure2 <- ggplot(data=Raw, aes(x=reorder(Building.structure, log10(Transaction.price.total.), FUN=median))) +
#      geom_histogram(stat='count')
# grid.arrange(p_structure,p_structure2, nrow=2)
# g <- arrangeGrob(p_structure,p_structure2, nrow=2)
# ggsave(file.path(dir,"Result","Structure.png"),g)
# dev.off()

#### Layout ######
# p_layout<- ggplot(Raw[!is.na(Raw$Transaction.price.total.),], aes(x=reorder(Layout, log10(Transaction.price.total.), FUN=median), y=log10(Transaction.price.total.))) +
#   geom_bar(stat="summary", fun = "median", fill='blue') + labs(x='Layout', y='log10 median price') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10)) +
#   ylim(0,10) +
#   geom_label(stat = "count", aes(label = ..count.., y = ..count../1000000), size=2)
# p_layout2 <- ggplot(data=Raw, aes(x=reorder(Layout, log10(Transaction.price.total.), FUN=median))) + 
#      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10)) +
#      geom_histogram(stat='count')
# grid.arrange(p_layout,p_layout2, nrow=2)
# g <- arrangeGrob(p_layout,p_layout2, nrow=2, nrow=2)
# ggsave(file.path(dir,"Result","Layout.png"),g)
# dev.off()

# 7 Feature engineering ------
## 7.1 BigHouse ######
Raw$BigHouse <- ifelse(Raw$Total.floor.area.m.2. >= 1000, 1, 0)

small_house <- Raw %>%
  filter(BigHouse == 0) %>%
  select(Transaction.price.total.)

big_house <- Raw %>%
  filter(BigHouse == 1) %>%
  select(Transaction.price.total.)

t_test_floor <- t.test(log10(small_house),log10(big_house))
t_test_floor

# ggplot(Raw, aes(x=as.factor(BigHouse), y=log10(Transaction.price.total.))) +
#   geom_bar(stat='summary', fun = "mean", fill='blue') +
#   geom_label(stat='count', aes(label=..count..,y=..count../1000000), size=6) +
#   # divided by 1000000 is just to make sure the label is on the bottom
#   theme_grey(base_size = 18) +
#   ylim(0,10)+ 
#   geom_hline(yintercept=7.41, linetype="dashed")
#   # dashed line is mean price
# ggsave(file.path(dir,"Result","BigHouse.png"))
# dev.off()

## 7.2 Age, IsNew ######
# Age
Raw$Age <- Raw$Year - Raw$Year.of.construction
Raw$Age <- ifelse(Raw$Age < 0, 0, Raw$Age)
cor_year_matrix <- Raw %>%
  select(Year,Year.of.construction, Age,Transaction.price.total.)
cor_year <- cor(cor_year_matrix, use="pairwise.complete.obs")

# 
# IsNew
Raw$IsNew <- ifelse(Raw$Age == 0, 1, 0)
Raw$IsNew <- as.numeric(Raw$IsNew)

old_house <- Raw %>%
  filter(IsNew == 0) %>%
  select(Transaction.price.total.)

new_house <- Raw %>%
  filter(IsNew == 1) %>%
  select(Transaction.price.total.)

t_test_new <- t.test(log10(old_house),log10(new_house))
t_test_new
# 
# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(Age, y=log10(Transaction.price.total.)))+
#   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
#   labs(x = "Age", y = "log(Transaction.price.total.)") +
#   ylim(0,10)
# ggsave(file.path(dir,"Result","Price_Age.png"))
# dev.off()
# 
# ggplot(Raw, aes(x=as.factor(IsNew), y=log10(Transaction.price.total.))) +
#   geom_bar(stat='summary', fun = "mean", fill='blue') +
#   geom_label(stat='count', aes(label=..count..,y=..count../1000000), size=6) +
#   # divided by 1000000 is just to make sure the label is on the bottom
#   theme_grey(base_size = 18) +
#   ylim(0,10)+ 
#   geom_hline(yintercept=7.34, linetype="dashed")
# # dashed line is mean price
# ggsave(file.path(dir,"Result","IsNew.png"))
# dev.off()

## 7.3 Building.structure ######
# Regroup the building structure
# Regroup Nearest.station.Name based on their median price
Raw$StructureQuality <- NA
Raw <- Raw %>%
  group_by(Building.structure) %>%
  mutate(StructureQuality = case_when(Building.structure %in% c("S, B", "SRC", "S, LS", "SRC, RC", "W, LS") ~ 1,
                                      Building.structure %in% c("RC", "No_information", "RC, B", "S, W") ~ 2,
                                      Building.structure %in% c("W") ~ 3,
                                      Building.structure %in% c("B", "RC, W", "LS", "RC, LS", "S", "RC, S", "SRC, S") ~ 4
                                  )) %>%
  ungroup()

cor_structure_matrix <- Raw %>%
  ungroup()
  select(StructureQuality,Transaction.price.total.)
cor_structure <- cor(Raw$StructureQuality, Raw$Transaction.price.total., use="pairwise.complete.obs")

# ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(StructureQuality, y=log10(Transaction.price.total.)))+
#   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
#   labs(x = "StructureQuality", y = "log(Transaction.price.total.)") +
#   ylim(0,10)
# ggsave(file.path(dir,"Result","Price_Structure_quality.png"))
# dev.off()

Raw$StationDistance <- ifelse(Raw$Nearest.station.Distance.minute. < 45, 1, 0)

far_house <- Raw %>%
   filter(StationDistance == 0) %>%
   select(Transaction.price.total.)

 close_house <- Raw %>%
   filter(StationDistance == 1) %>%
   select(Transaction.price.total.)

 t_test_station <- t.test(log10(far_house),log10(close_house))
 t_test_station

 ggplot(Raw, aes(x=as.factor(StationDistance), y=log10(Transaction.price.total.))) +
   geom_bar(stat='summary', fun = "mean", fill='blue') +
   geom_label(stat='count', aes(label=..count..,y=..count../1000000), size=6) +
   # divided by 1000000 is just to make sure the label is on the bottom
   theme_grey(base_size = 18) +
   ylim(0,10)+
   geom_hline(yintercept=7.32, linetype="dashed")
 # dashed line is mean price
 ggsave(file.path(dir,"Result","StationDistance.png"))
 dev.off()

# Run the random Forest again
# RF<-as.data.frame(Raw)
# quick_RF <- ranger(Transaction.price.total. ~ ., data = RF, num.trees=100,importance='permutation')
# imp_RF <- importance(quick_RF)
# imp_DF <- data.frame(Variables = names(imp_RF), Importance = imp_RF)
# imp_DF$Variance <- 100 * (imp_DF$Importance / sum(imp_DF$Importance))
# imp_DF <- imp_DF[order(imp_DF$Variance, decreasing = TRUE),]
# 
# ggplot(imp_DF, aes(x=reorder(Variables, Importance), y=Variance, fill=Variance)) +
#   geom_bar(stat = 'identity') +
#   labs(x = 'Variables', y= '% increase if variable is randomly permutated') +
#   coord_flip() + theme(legend.position="none")

# 8 Modeling ------
## 8.1 Drop unused variables ######
# I remove numeric variables that are already re-engineered. 
# I also remove variables that contaings too many levels and contribute less variance.
# I test the increase of R2 and decrease in RMSE when I add this variable to modeling.
# Add 'Area', 'Nearest.station.Name' can increae the R2 by 0.1-0.2% and decrease the RMSE by 0.002%.
# Add 'Layout' can increase the R2 by 4% and slightly increase the RMSE by 0.001 - 0.003. 
# In opposite, Add 'Frontage.road.Classification' largely decrease the R2 by 10% and increase the RMSE by 0.11.

dropVars2 <- c('Year.of.construction', 'Year', 'Area.m.2.', 'Building.structure', 'Frontage.road.Classification', 'Region',
                'City.Town.Ward.Village', 'quarter.1', 'Use', 'Purpose.of.Use')

all <- Raw[,!(names(Raw) %in% dropVars2)]

## 8.2 Removing outliers ######
all <- all %>%
  filter(Transaction.price.total. > 3000000)

## 8.3 log transform numeric variables if their distribution are skewed ######
# Check how many numeric and factor variables we have now
# Remove non-numeric variables
numericVars <- which(sapply(all, is.numeric))
numericVarNames <- names(numericVars)
numericVarNames <- numericVarNames[!(numericVarNames %in% c('Transaction.price.total.','IsNew','BigHouse','StructureQuality','StationDistance'))]
DFnumeric <- all[, names(all) %in% numericVarNames]
DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'Transaction.price.total.']
cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables.')

# log transform the numberic variables if their distributions are skewed

for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i]+1)
  }
}

# Center the numeric data
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

## 8.4 Perform one-hot coding for factor variables ######
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

# # check if some values are absent in the test set
# ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$Transaction.price.total.),])+1):nrow(all),])==0)
# colnames(DFdummies[ZerocolTest])
# 
# # check if some values are absent in the train set
# ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$Transaction.price.total.),]),])==0)
# colnames(DFdummies[ZerocolTrain])
# 
# DFdummies <- DFdummies[,-ZerocolTrain] #removing predictor

# removing levels with few or no observations in train or test 
fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$Transaction.price.total.),]),])<20)
colnames(DFdummies[fewOnes])

DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe 

## 8.5 Check the skewness and perform natural log transformation of responsive variable ######
skew(all$Transaction.price.total.)

# png(file.path(dir,"Result","skewness_price.png"))
# qqnorm(all$Transaction.price.total.)
# qqline(all$Transaction.price.total.)
# dev.off()

# default is the natural logarithm, "+1" is not necessary as there are no 0's
all$Transaction.price.total. <- log(all$Transaction.price.total.)
skew(all$Transaction.price.total.)
# png(file.path(dir,"Result","skewness_price.2.png"))
# qqnorm(all$Transaction.price.total.)
# qqline(all$Transaction.price.total.)
# dev.off()

## 8.6 Composing train and test dataset ######
# train1 <- combined[!is.na(all$Transaction.price.total.),]
# test1 <- combined[is.na(all$Transaction.price.total.),]

# 9 Modeling ------
## 9.1 lasso ######
set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
lasso_mod <- train(x=combined, y=all$Transaction.price.total., method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune
min(lasso_mod$results$RMSE)
max(lasso_mod$results$Rsquared)


## 9.2 Modeling, ridge ######
# The ridge modeling generates similar results as lasso.
set.seed(27042018)
my_control_ridge <-trainControl(method="cv", number=5)
ridgeGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
ridge_mod <- train(x=combined, y=all$Transaction.price.total., method='glmnet', trControl= my_control_ridge, tuneGrid=ridgeGrid) 
ridge_mod$bestTune
min(ridge_mod$results$RMSE)
max(ridge_mod$results$Rsquared)
