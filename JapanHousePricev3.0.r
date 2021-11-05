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

#### Building.structure
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

# 7 Feature engineering ------
## 7.1 BigHouse
Raw$BigHouse <- ifelse(Raw$Total.floor.area.m.2. >= 1000, 1, 0)

small_house <- Raw %>%
  filter(BigHouse == 0) %>%
  select(Total.floor.area.m.2.)

big_house <- Raw %>%
  filter(BigHouse == 1) %>%
  select(Total.floor.area.m.2.)

t_test <- t.test(log10(small_house),log10(big_house))

ggplot(Raw, aes(x=as.factor(BigHouse), y=log10(Transaction.price.total.))) +
  geom_bar(stat='summary', fun = "median", fill='blue') +
  geom_label(stat='count', aes(label=..count..,y=..count../1000000), size=6) +
  theme_grey(base_size = 18) +
  ylim(0,10) # divided by 1000000 is just to make sure the label is on the bottom
ggsave(file.path(dir,"Result","BigHouse.png"))
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
