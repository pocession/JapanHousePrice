### This project is cloned and modified from 
### https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/

# Version 0.1
# 1. Summary
I start this project by focusing on understanding Japanese realestate market and preacticing my data analysis skills. The EDA and modeling are documented in each section.

In this version, only data of Kanagawa prefecture is used for modeling.  

* The data is collected from [Land General information system](https://www.land.mlit.go.jp/webland/servlet/MainServlet). 
* Describe modeling methods used in this project.

# 2. Files
* Due to the large size of data files, data are not uploaded to the repository. You can download the data from [Land General information system](https://www.land.mlit.go.jp/webland/servlet/MainServlet).
* All R scripts in [Script](Script/).
* All plots in [Result](Result/).

# 3. Introduction
# 4. Loading and exploring data
## 4.1. Loading required libraries

```{r, message=FALSE, warning=FALSE}
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
```

## 4.2. Loading csv data into R

```{r}
# Get the files names
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
# Loading essential packages -----------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(corrplot)
library(scales)

# Loading data into R -----------------------------------------------------------------------------------------------------
dir <- "E:/Dropbox (OIST)/Ishikawa Unit/Tsunghan/JapanHousePrice"

# Get the files names
files <- list.files(file.path(dir, "Raw"), pattern="*.csv")

# No is not a variable
Raw <- read.csv(file.path(dir,"Raw",files[37]), stringsAsFactors = F)
Raw <- Raw[,2:ncol(Raw)]

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
We can find that missing values in variables are shown in NA or "" (Blank) in this dataset. We can also notice that some missing values in numeric variables are associated with specific values in character variables. This suggests that those variables are in a group and should handled together. Let's start to identify NAs in numeric variables and deal with their associated values in character variables. We should have 21 variables containing missing values, which are listed below.
```{r}
Nblankcol <- which(sapply(Raw,function(x) any(x== "")))
NAcol <- which(colSums(is.na(Raw)) > 0)
missingcol<-union(names(blankcol),names(NAcol))
cat('There are', length(missingcol), 'variables containing missing values')

There are 21 variables containing missing values

[1] "Region"                             "Nearest.station.Name"               "Layout"                            
 [4] "Land.shape"                         "Building.structure"                 "Use"                               
 [7] "Purpose.of.Use"                     "Frontage.road.Direction"            "Frontage.road.Classification"      
[10] "City.Planning"                      "Renovation"                         "Transactional.factors"             
[13] "Nearest.station.Distance.minute."   "Area.m.2."                          "Transaction.price.Unit.price.m.2." 
[16] "Frontage"                           "Total.floor.area.m.2."              "Year.of.construction"              
[19] "Frontage.road.Breadth.m."           "Maximus.Building.Coverage.Ratio..." "Maximus.Floor.area.Ratio..."     
```
Transaction.price.Unit.price.m.2. will be our dependent variable (Y). Let's handle it first. NAs in Transaction.price.Unit.price.m.2. have two meanings: either the value Area.m2. is lacking or the calculation is not performed. Let's complete the calculation first. Then we check whether the number of NAs left in Transaction.price.Unit.price.m.2. and in Area.m.2 equal to each other.

```{r}
# Complete the caculation of Transaction.price.Unit.price.m.2.
# And obtain how many NAs are left, should be 1216
Raw$Transaction.price.Unit.price.m.2. <- Raw$Transaction.price.total./Raw$Area.m.2.
length(which(is.na(Raw$Transaction.price.Unit.price.m.2.)))

# Check if NAs of the two variables are now all 1216
length(which(is.na(Raw$Transaction.price.Unit.price.m.2.))) & length(which(is.na(Raw$Area.m.2.)))

True
```
Due to the information of Area as well as the Transaction.price.Unit.price.m.2. are lacking, those 1216 data will not be be used in our analysis.

Assign NAs to the following variables.

```{r}
# Assign NAs to Region, Land Shape, USe, Purpose.of.Use
Raw$Region[which(Raw$Region == "")] <- NA
Raw$Land.shape[which(Raw$Land.shape == "")] <- NA
Raw$Use[which(Raw$Use == "")] <- NA
Raw$Purpose.of.Use[which(Raw$Purpose.of.Use == "")] <- NA
```

NAs in Frontage.road.Breadth.m. means this real estate does not have any facing road, which is also reflected in another two variables: Frontage.road.Direction and Frontage.road.Classification.. Let's check whether number of the NAs in Frontage.road.Breadth.m. matches the number of "No facing road" in Frontage.road.Direction as well as the number of "" (Blank) in Frontage.road.Classification.. In the final step, we replace the NAs in Frontage.road.Breadth.m. with 0.

```{r}
# Check if number of NAs in Frontage.road.Breadth.m.equals the number of "" in Frontage.road.Classification and "No facing road" in Frontage.road.Direction
length(which(is.na(Raw$Frontage.road.Breadth.m.))) & length(which(Raw$Frontage.road.Classification=="")) & length(which(Raw$Frontage.road.Direction=="No facing road"))

# Replace the NAs in Frontage.road.Breadth.m. with 0
Raw$Frontage.road.Breadth.m.[is.na(Raw$Frontage.road.Breadth.m.)] <- 0

# Assign Nas to Frontage.road.Classification and Frontage.road.Direction
Raw$Frontage.road.Classification[which(Raw$Frontage.road.Classification == "")] <- NA
Raw$Frontage.road.Direction[which(Raw$Frontage.road.Direction == "")] <- NA

# Check if number of NAs in Maximus.Building.Coverage.Ratio... and in Maximus.Floor.area.Ratio... are equal
length(which(is.na(Raw$Maximus.Building.Coverage.Ratio...))) & length(which(is.na(Raw$Maximus.Floor.area.Ratio...)))

True
```

The variable Year.of.construction contains many NAs. But if we check it carefully, we can find out most of NAs come from the land property. If a house property (e.g. Pre-owned Condominiums) does not have the information of years of construction, it means the it was built before World War II (Before 1945). Let's assign 1935 to those properties.

```{r}
# Check the NAs in Year.of.construction
Raw %>%
  filter(is.na(Year.of.construction)) %>%
  group_by(Type) %>%
  summarise(count = n())

# A tibble: 5 x 2
  Type                                count
  <chr>                               <int>
1 Agricultural Land                    4724
2 Forest Land                          1090
3 Pre-owned Condominiums, etc.          115
4 Residential Land(Land and Building)  1447
5 Residential Land(Land Only)         13859

# 1935 is a fake number to those house properties built before WWII.
Raw$Year.of.construction[which(Raw$Type == "Pre-owned Condominiums, etc.")] <- 1935
Raw$Year.of.construction[which(Raw$Type == "Residential Land(Land and Building)")] <- 1935

# Check the NAs in Year.of.construction again, all NAs should be obtained only in land properties
Raw %>%
  filter(is.na(Year.of.construction)) %>%
  group_by(Type) %>%
  summarise(count = n())

  # A tibble: 3 x 2
  Type                        count
  <chr>                       <int>
1 Agricultural Land            4724
2 Forest Land                  1090
3 Residential Land(Land Only) 13859
```
### 4.3.3 Split data into train and test dataset
Then I split the data into train and test dataset. I use 75% of sample as train dataset. The other 25% are left for test dataset. I save both datasets as csv file.

```{r}
## 75% of the sample size
smp_size <- floor(0.75 * nrow(Raw))

## set the seed to make your partition reproducible
set.seed(120)
train_ind <- sample(seq_len(nrow(Raw)), size = smp_size)

train <- Raw[train_ind, ]
test <- Raw[-train_ind, ]

# Save train and test data into csv files
write.csv(train,file.path(dir,"Raw","train.csv"))
write.csv(test,file.path(dir,"Raw","test.csv"))
```

# 5. Exploring some of the most important variables
## 5.1. The depenent variable, Transaction.price.total.
Apparently the distribution of price is right skewed. It is because only a few real estates are transacted with high prices. Those data may be outliers. Let's keep the data now but will remove before modeling.

```{r}
# Visualize the transaction price
ggsave(file.path(dir,"Result","Transaction.price.total.png"))
ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(x=Transaction.price.total.)) +
  geom_histogram(fill="blue", binwidth = 1000000) 
#dev.off()
```
![Transaction.price.total.](/Result/Transaction.price.total.png?raw=true)

```{r}
# Have a summary of Transaction.price.total.
summary_TRansaction.price.total <- summary(Raw$Transaction.price.total.)
summary_TRansaction.price.total

##   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##   1400   5300000  11000000  14193646  21000000 340000000 
```
## 5.2. The most important numeric variable
I need to process the chacater variables before analyzing them. Let's start to check the numeric variable first.
### 5.2.1. Correlation with Transaction.price.total.
Let's see how many numeric variables first.
```{r}
# Check the numeric variables
numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

There are 6 numeric variables
```
We have 6 numeric variables in our Raw dataset. Now let's check the correlation beteew Transaction.price.total. and each numeric variable.

```{r}
# Check the numeric variables
numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

all_numVar <- Raw[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

# Sort on decreasing correlations with Transaction.price.total.
cor_sorted <- as.matrix(sort(cor_numVar[,'Transaction.price.total.'], decreasing = TRUE))
cor_sorted

# 
# 
#                                           [,1]
# Transaction.price.total.            1.00000000
# Frontage.road.Breadth.m.            0.18504503
# Maximus.Floor.area.Ratio...        -0.05139797
# City.Town.Ward.Village.code        -0.07525850
# Maximus.Building.Coverage.Ratio... -0.08390417
```
There is only numeric variable, Frontage.road.Breadth.m., with a low correlation 

# 6. Missing data, label encoding and factorizing variables
# Visualization of important variables
# Feature engineering
# Preparing data for modeling
# Modeling