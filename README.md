### This project is cloned and modified from 
### https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/

# Version 1.0
# 1. Summary
I start this project by focusing on understanding Japanese realestate market and preacticing my data analysis skills. The EDA and modeling are documented in each section.

In this version, only data of Kanagawa prefecture is used for modeling.  

* The data is collected from [Land General information system](https://www.land.mlit.go.jp/webland/servlet/MainServlet). 
* Describe modeling methods used in this project.

# 2. Files
* Due to the large size of data files, data are not uploaded to the repository. You can download the data from [Land General information system](https://www.land.mlit.go.jp/webland/servlet/MainServlet).
* All plots in [Result](Result/).

# 3. Introduction
# 4. Loading and exploring data
## 4.1. Loading required libraries

```{r, message=FALSE, warning=FALSE}
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(corrplot)
library(scales)
library(randomForest)
```

## 4.2. Loading csv data into R

```{r}
# Loading data into R -----------------------------------------------------------------------------------------------------
current_path = rstudioapi::getActiveDocumentContext()$path
dir <- setwd(dirname(current_path ))
files <- list.files(file.path(dir, "Raw"), pattern="*.csv")

# No is not a variable
Raw <- read.csv(file.path(dir,"Raw",files[37]), stringsAsFactors = F)
Raw <- Raw[,2:ncol(Raw)]
```
## 4.3. Data wrangling
We need to wrangle data before performing the analysis. Let's wrangle numeric and character data separately. 
### 4.3.1 Numeric data
Let's wrangle the numeric data first. In the final, we should have 10 numeirc variables.
```{r}
# Data wrangling numeric -----------------------------------------------------------------------------------------------------
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

# Frontage: only needs to replace one value
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
```
We should have 11 numeric variables. The list is below.
```{r}
[1] "Nearest.station.Distance.minute."   "Transaction.price.total."           "Area.m.2."                         
 [4] "Transaction.price.Unit.price.m.2."  "Frontage"                           "Total.floor.area.m.2."             
 [7] "Year.of.construction"               "Frontage.road.Breadth.m."           "Maximus.Building.Coverage.Ratio..."
[10] "Maximus.Floor.area.Ratio..."        "Year"    

There are 11 numeric variables
```
### 4.3.2 Dealing with missing values
Now we can find that missing values in variables are shown in NA or "" (Blank) in this dataset. Some missing values in numeric variables are associated with specific values in character variables. This suggests that those variables are in a group and should be handled together. Let's start to identify NAs in numeric variables and deal with their associated values in character variables. We should have 21 variables containing missing values, which are listed below.  

We will only discuss those real estates containing houses. We first exclude data belong to land properties and deal variables one by one.

* Region: Only a small fraction of NAs are related to Pre-owned Condominiums. Assign "no_information" to them. 
* Nearest.station.Name: this variable is related to Nearest.station.Distance.minute.. So let's identify data that contain missing values in both Nearest.station.Name and Nearest.station.Distance.minute.. We then assign "No_station" and a dummy number 999 to those data. Lastly, we exclude data that contains station names but without any distance information.
* Transaction.price.Unit.price.m.2.: this variable is our dependent variable (Y). NAs in Transaction.price.Unit.price.m.2. have two meanings: either the value Area.m2. is lacking or the calculation is not performed. Let's complete the calculation first. Then we check whether the number of NAs left in Transaction.price.Unit.price.m.2. and in Area.m.2 equal to each other. Those data should be excluded in the analysis.
* Frontage.road.Direction: this variable is related to other two variables: Frontage.road.Classification and Frontage.road.Breadth.m.. If the data does not contain any facing road, then NAs are also shown in other two variables. Let's check whether number of the NAs in Frontage.road.Breadth.m. matches the number of "No facing road" in Frontage.road.Direction as well as the number of "" (Blank) in Frontage.road.Classification.. In the final step, we replace the NAs in Frontage.road.Breadth.m. with 0.
* Frontage: this variable is totally related to Type. Data belong to land+house contains the frontage information and only a small fraction contains missing values. In the other hand, data belong to Pre-owned Condominiums do not contain any frontage information. Let's assign 0 to all missing values.

Before assigning 0:
![Unit_price_Frontage](/Result/Unit_price_Frontage.png?raw=true)  

After assigning 0:
![Unit_price_Frontage2](/Result/Unit_price_Frontage2.png?raw=true)

* Total.floor.area.m.2.: similar to Frontage, we first visualize the relationship between unit price and this variable. This variable is negatvely (but weakly) correlated to unit price. Since only a small fraction contains NAs, we assign 0 to all NAs.  

Before assigning 0:
![Unit_price_Floor_Area](/Result/Unit_price_Floor_Area.png?raw=true)  

After assigning 0:
![Unit_price_Floor_Area2](/Result/Unit_price_Floor_Area2.png?raw=true)
* Year.of.construction: Houses built before world war II do not contain information of year of built. We assign a dummy number 1935 to all NAs. 
* Layout, Land.shape, building.structure, Use, Purpose.of.Use, City.Planning, Renovation, and Transactional.factors: Assign "No_information" to missing values in these three variables.

We should not have any variables containing missing values now.
```{r}
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

# 1935 is a fake number to those house properties built before WWII.
Raw$Year.of.construction[which(is.na(Raw$Year.of.construction))] <- 1935

# Check whether we have any variables containing missing values now
blankcol <- which(sapply(Raw,function(x) any(x== "")))
NAcol <- which(colSums(is.na(Raw)) > 0)
missingcol<-union(names(blankcol),names(NAcol))
cat('There are', length(missingcol), 'variables containing missing values')
missingcol
```

### 4.3.3 Factorize data
Next, let's factorize variables. For some variables, if the values have ordinality, we should make it as ordinal factors. In the Layout variable, Open Floor is very rare and is only used in shops or offices. We will not analyze those data anyway.

```{r}
# Factorize non-ordinal variables first
Factors <- c("Type","Region","City.Town.Ward.Village.code","Prefecture","City.Town.Ward.Village","Area","Nearest.station.Name",
             "Land.shape","Building.structure","Use","Purpose.of.Use","Frontage.road.Direction","Frontage.road.Classification",
             "City.Planning","Renovation")
Raw[Factors]<-lapply(Raw[Factors],factor)

# Factorize ordinal variables
Raw$quarter.1 <- factor(Raw$quarter.1,order = TRUE, levels = c("1st", "2nd", "3rd", "4th"))
Raw$Layout <- factor(Raw$Layout, order = TRUE, levels = c("No_information","1R","1K","1DK","1LDK","2K","2K+S","2DK","2DK+S","2LDK","2LDK+S",
                                                          "3K","3DK","3LDK","3LDK+S","4DK","4LDK","5DK","5LDK","6DK"))
```  
Now our data looks like this. We should keep in mind that those variables containing too many levels (<53) may need to be further transformed during modeling. 
```{r}
str(Raw)

'data.frame':	12066 obs. of  25 variables:
 $ Type                              : chr  "Pre-owned Condominiums, etc." "Pre-owned Condominiums, etc." "Pre-owned Condominiums, etc." "Pre-owned Condominiums, etc." ...
 $ Region                            : chr  "No_information" "No_information" "No_information" "No_information" ...
 $ City.Town.Ward.Village.code       : Factor w/ 17 levels "37201","37202",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Prefecture                        : chr  "Kagawa Prefecture" "Kagawa Prefecture" "Kagawa Prefecture" "Kagawa Prefecture" ...
 $ City.Town.Ward.Village            : chr  "Takamatsu City" "Takamatsu City" "Takamatsu City" "Takamatsu City" ...
 $ Area                              : chr  "Akanecho" "Akanecho" "Akanecho" "Akanecho" ...
 $ Nearest.station.Name              : chr  "Showacho (Kagawa)" "Showacho (Kagawa)" "Showacho (Kagawa)" "Showacho (Kagawa)" ...
 $ Nearest.station.Distance.minute.  : num  10 14 13 13 10 10 12 13 10 14 ...
 $ Layout                            : chr  "3DK" "3LDK" "3LDK" "2DK" ...
 $ Transaction.price.Unit.price.m.2. : num  100000 118571 108333 160000 114545 ...
 $ Land.shape                        : chr  "No_information" "No_information" "No_information" "No_information" ...
 $ Frontage                          : num  0 0 0 0 0 0 11.5 0 0 0 ...
 $ Total.floor.area.m.2.             : num  0 0 0 0 0 0 95 0 0 0 ...
 $ Year.of.construction              : num  1990 1995 1990 1990 1990 ...
 $ Building.structure                : chr  "SRC" "RC" "SRC" "SRC" ...
 $ Use                               : chr  "House" "House" "House" "House" ...
 $ Purpose.of.Use                    : chr  "House" "House" "House" "House" ...
 $ Frontage.road.Direction           : chr  "No_information" "No_information" "No_information" "No_information" ...
 $ Frontage.road.Classification      : chr  "No_information" "No_information" "No_information" "No_information" ...
 $ Frontage.road.Breadth.m.          : num  0 0 0 0 0 0 4.5 0 0 0 ...
 $ City.Planning                     : chr  "Quasi-industrial Zone" "Quasi-industrial Zone" "Quasi-industrial Zone" "Quasi-industrial Zone" ...
 $ Maximus.Building.Coverage.Ratio...: num  60 60 60 60 60 60 60 60 60 60 ...
 $ Maximus.Floor.area.Ratio...       : num  200 200 200 200 200 200 200 200 200 200 ...
 $ quarter.1                         : chr  "4th" "4th" "3rd" "1st" ...
 $ Year                              : num  2020 2020 2020 2019 2018 ...
```
### 4.3.4 Drop some variables
Finally, let's drop some variables. We will use Transaction.price.Unit.price.m.2. as dependent variable, so both Transaction.price.total. and Area.m.2. are not required. quarter.2 is not necessary. Renovation and Transactional.factors are also not required since these two variables contain too many missing values and could not provide any useful information.  

```{r}
# Drop some variables ----------------------------------------------------------------------------------------------------
# Some variables are not required for the further analysis.
drops <- c("Transaction.price.total.","Area.m.2.","quarter.2","Renovation","Transactional.factors")
Raw <- Raw[ , !(names(Raw) %in% drops)]

numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(Raw, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

There are 9 numeric variables, and 16 categoric variables.
```

The data wrangling part ends here. Be reminded that we still need to do one-hot encoding for non-ordinal factor variables. We will handle this in modeling section.

# 5. Exploring variables
## 5.1. The depenent variable, Transaction.price.Unit.price.m2. (Unit price)
Transaction.price.Unit.price.m2. is our dependent variable, or Y. For simplicity, we will call it as unit price from now. Let's check the distribution first. Apparently the distribution is right skewed and it is because only a few real estates are transacted with high prices. Those data are so0called outliers. Let's keep the data now but will remove those data before modeling.

```{r}
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

Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
  10   29412   69767   92448  129032 2470588
```   

![Transaction.price.Unit.price.m2](/Result/Transaction.price.Unit.price.m2.png?raw=true)

## 5.2. The correlation between numeric variables and the unit price
We then check the unit price is affected by which numeric variables most. Apparently, the unit price is positively correlated with the year that the house was built, with a correlation coefficient of 0.4. Newer houses are more expensive, very reasonable. We also find Maximus.Building.Coverage.Ratio... and Maximus.Floor.area.Ratio... are mutually correlated. We remove Maximus.Building.Coverage.Ratio... because it is less correlated with unit price.

```{r}
# Identify the correlation between Transaction.price.Unit.price.m.2.and numeric variables--------------------------------- 
numericVars <- which(sapply(all, is.numeric))
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

# Sort on decreasing correlations with Transaction.price.Unit.price.m.2.
cor_sorted <- as.matrix(sort(cor_numVar[,'Transaction.price.Unit.price.m.2.'], decreasing = TRUE))
# Display all correlation
Cor <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.)))
cor_numVar <- cor_numVar[Cor, Cor]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
png(file.path(dir,"Result","CorrelationvarNum.png"))
dev.off()
```

![CorrelationvarNum](/Result/CorrelationvarNum.png?raw=true)
### 5.2.1. The correlation between categorical (factor) variables and the unit price.
We now check the correlation between categorical variables (factor variables) and the unit price with a method called random forest. However, random forest cannot handle variables with NAs, either factor variables with more than 53 levels. We already deal with the missing values before. Now We have to reduce the levels of Nearest.station.Name and Area. I calculate the trading frequencies of each station and area. I then  transform the names of station and area as their trading frequency. As a result, we can see the unit price is still most correlated with Year.of.construciton.

```{r}
# Identify the correlation between Transaction.price.Unit.price.m.2. and all variables by Random Forest----------------------
RF<- all %>%
  filter(!is.na(Transaction.price.Unit.price.m.2.))
set.seed(2018)

# randomForest can not handle variables containing more than 53 level
# Change the Nearest.station.Name as trading frequency of each station
Frequency_Station <- all %>%
  group_by(Nearest.station.Name) %>%
  summarise(n=n()) %>%
  mutate(frequency=n/sum(n)) %>% 
  select(Nearest.station.Name,frequency)
  
Frequency_Area <- all %>%
  group_by(Area) %>%
  summarise(n=n()) %>%
  mutate(frequency=n/sum(n)) %>%
  select(Area,frequency)

RF <- RF %>%
  left_join(Frequency_Station,by=c("Nearest.station.Name")) %>%
  left_join(Frequency_Area,by="Area")

colnames(RF)[c(25,26)] <- c("Nearest.station.Name.frequency","Area.frequency")

# Run the random Forest
quick_RF <- randomForest(x=RF[,-c(6,7,10)], y=RF[,10], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")
ggsave(file.path(dir,"Result","randomForest.png"))
dev.off()
```

![randomForest](/Result/randomForest.png?raw=true)

# 6. The last wrangling process before modeling
# Visualization of important variables
# Feature engineering
# Preparing data for modeling
# Modeling