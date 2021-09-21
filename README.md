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

Let's deal variables one by one.
* Region: In fact, Region and Type are related. Many NAs in Regions are related to forest land and agricultural land. Only a small fraction of NAs are related to Pre-owned Condominiums. This data will not be used in our analysis. For the remained NAs, assign "no_information" to them. 
* Nearest.station.Name: this variable is related to Nearest.station.Distance.minute.. So let's identify data that contain missing values in both Nearest.station.Name and Nearest.station.Distance.minute.. We then assign "No_station" and a dummy number 999 to those data. Lastly, we exclude data that contains station names but without any distance information.
* Layout, Land.shape, building.structure, Use, Purpose.of.Use: assign "No_information" to NAs.
* Transaction.price.Unit.price.m.2.: this variable is our dependent variable (Y). NAs in Transaction.price.Unit.price.m.2. have two meanings: either the value Area.m2. is lacking or the calculation is not performed. Let's complete the calculation first. Then we check whether the number of NAs left in Transaction.price.Unit.price.m.2. and in Area.m.2 equal to each other. Those data should be excluded in the analysis.
* Frontage.road.Direction: this variable is related to other two variables: Frontage.road.Classification and Frontage.road.Breadth.m.. If the data does not contain any facing road, then NAs are also shown in other two variables. Let's check whether number of the NAs in Frontage.road.Breadth.m. matches the number of "No facing road" in Frontage.road.Direction as well as the number of "" (Blank) in Frontage.road.Classification.. In the final step, we replace the NAs in Frontage.road.Breadth.m. with 0.
* City.Planing, Renovation, and Transactional.factors: Assign "No_information" to missing values in these three variables.
* Frontage: this variable is related to Type and Frontage.road.Direction. Data belong to land properties do not contain frontage data. We assign a dummy number 0 to data belong to land properties. Then we exclude data belong to land+buildings but without the frontage information.


```{r}

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
Raw$Year.of.construction[which(Raw$Type == "Pre-owned Condominiums, etc." & is.na(Raw$Year.of.construction))] <- 1935
Raw$Year.of.construction[which(Raw$Type == "Residential Land(Land and Building )" & is.na(Raw$Year.of.construction))] <- 1935
Raw$Year.of.construction <- as.numeric(Raw$Year.of.construction)

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

Lastly, let's assign NAs to the last two variables, Revonation and Transactional.factors. In fact, Transactional.factors is more or less like a note. We will not use this variable during analysis.
```{r}
# Assigns NAs to Renovation and Transactional.factors
Raw$Renovation[which(Raw$Renovation == "")] <- NA
Raw$Transactional.factors[which(Raw$Transactional.factors == "")] <- NA

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
Raw$Layout <- factor(Raw$Layout, order = TRUE, levels = c("1R","1K","1DK","1LDK","2K","2K+S","2DK","2DK+S","2LDK","2LDK+S",
                                                          "3K","3DK","3LDK","3LDK+S","4DK","4LDK","5DK","5LDK","6DK"))

str(Raw)

'data.frame':	32048 obs. of  30 variables:
 $ Type                              : Factor w/ 5 levels "Agricultural Land",..: 5 3 3 3 3 3 3 5 4 3 ...
 $ Region                            : Factor w/ 4 levels "Commercial Area",..: 4 NA NA NA NA NA NA 4 4 NA ...
 $ City.Town.Ward.Village.code       : Factor w/ 17 levels "37201","37202",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Prefecture                        : Factor w/ 1 level "Kagawa Prefecture": 1 1 1 1 1 1 1 1 1 1 ...
 $ City.Town.Ward.Village            : Factor w/ 17 levels "Ayagawa Town,Ayauta County",..: 14 14 14 14 14 14 14 14 14 14 ...
 $ Area                              : Factor w/ 572 levels "(No Address)",..: 3 3 3 3 3 3 3 3 3 3 ...
 $ Nearest.station.Name              : Factor w/ 101 levels "Ayagawa","Busshozan",..: 82 82 82 82 82 82 82 82 82 82 ...
 $ Nearest.station.Distance.minute.  : num  15 10 14 13 13 10 10 14 12 13 ...
 $ Transaction.price.total.          : num  2.6e+07 5.5e+06 8.3e+06 6.5e+06 6.4e+06 6.3e+06 1.3e+07 1.2e+07 8.5e+06 7.5e+06 ...
 $ Layout                            : Ord.factor w/ 19 levels "1R"<"1K"<"1DK"<..: NA 12 13 13 7 7 13 NA NA 13 ...
 $ Area.m.2.                         : num  520 55 70 60 40 55 85 160 140 60 ...
 $ Transaction.price.Unit.price.m.2. : num  50000 100000 118571 108333 160000 ...
 $ Land.shape                        : Factor w/ 9 levels "&quot;Flag-shaped&quot; etc.",..: 4 NA NA NA NA NA NA 3 8 NA ...
 $ Frontage                          : num  15.5 NA NA NA NA NA NA 11 11.5 NA ...
 $ Total.floor.area.m.2.             : num  NA NA NA NA NA NA NA NA 95 NA ...
 $ Year.of.construction              : num  NA 1935 1935 1935 1935 ...
 $ Building.structure                : Factor w/ 15 levels "B","LS","RC",..: NA 11 3 11 11 11 11 NA 2 11 ...
 $ Use                               : Factor w/ 103 levels "Factory","Factory, Office",..: NA 16 16 16 16 16 16 NA 16 16 ...
 $ Purpose.of.Use                    : Factor w/ 6 levels "Factory","House",..: NA 2 2 2 2 2 2 NA 2 2 ...
 $ Frontage.road.Direction           : Factor w/ 9 levels "East","No facing road",..: 1 NA NA NA NA NA NA 1 6 NA ...
 $ Frontage.road.Classification      : Factor w/ 14 levels "Access Road",..: 3 NA NA NA NA NA NA 3 3 NA ...
 $ Frontage.road.Breadth.m.          : num  4.5 0 0 0 0 0 0 12 4.5 0 ...
 $ City.Planning                     : Factor w/ 17 levels "","Category I Exclusively Low-story Residential Zone",..: 15 15 15 15 15 15 15 15 15 15 ...
 $ Maximus.Building.Coverage.Ratio...: int  60 60 60 60 60 60 60 60 60 60 ...
 $ Maximus.Floor.area.Ratio...       : int  200 200 200 200 200 200 200 200 200 200 ...
 $ quarter.1                         : Ord.factor w/ 4 levels "1st"<"2nd"<"3rd"<..: 4 4 4 3 1 4 4 3 3 3 ...
 $ quarter.2                         : chr  "quarter" "quarter" "quarter" "quarter" ...
 $ Year                              : num  2020 2020 2020 2020 2019 ...
 $ Renovation                        : Factor w/ 2 levels "Done","Not yet": NA 2 2 1 2 2 1 NA NA 2 ...
 $ Transactional.factors             : chr  NA NA NA NA ...
```
### 4.3.5 Drop some variables
Finally, let's drop some variables. We will use Transaction.price.Unit.price.m.2. as dependent variable, so Transaction.price.total. is not required. quarter.2 is not necessary. Renovation and Transactional.factors are also note required since most values in these two varialbes are NAs. 

```{r}
# Drop some variables ----------------------------------------------------------------------------------------------------
# Some variables are not required for the further analysis.
drops <- c("Transaction.price.total.","quarter.2","Renovation","Transactional.factors")
Raw <- Raw[ , !(names(Raw) %in% drops)]
```

```{r}
numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(Raw, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

There are 10 numeric variables, and 16 categoric variables.
```

The data wrangling part ends here. Be reminded that we still need to do one-hot encoding for non-ordinal factor variables. We will handle this in modeling section.

# 5. Exploring variables
## 5.1. The depenent variable, Transaction.price.Unit.price.m2. (Unit price)
Transaction.price.Unit.price.m2. is our dependent variable, or Y. For simplicity, we will call it as unit price from now. Let's check the distribution first. Apparently the distribution is right skewed and it is because only a few real estates are transacted with high prices. Those data are so0called outliers. Let's keep the data now but will remove those data before modeling.

```{r}
# Visualize the transaction price-----------------------------------------------------------------------------------------
# We only consier house price
all <- Raw %>%
  filter(Use == "House")

ggsave(file.path(dir,"Result","Transaction.price.Unit.price.m2.png"))
ggplot(data=all[!is.na(all$Transaction.price.Unit.price.m.2.),], aes(x=Transaction.price.Unit.price.m.2.)) +
  geom_histogram(fill="blue", binwidth = 10000) 
dev.off()
```
![Transaction.price.Unit.price.m2](/Result/Transaction.price.Unit.price.m2.png?raw=true)

```{r}
# Have a summary of Transaction.price.Unit.price.m2.
summary_Transaction.price.Unit.price.m2. <- summary(train$Transaction.price.Unit.price.m.2.)
summary_Transaction.price.Unit.price.m2.

## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   10   30556   72727   92688  130000 1507692      13
```
## 5.2. The correlation between numeric variables and the unit price
We then check the unit price is affected by which numeric variables most. Apparently, the unit price is positively correlated with the year that the house was built, with a correlation coefficient of 0.5. Newer houses are more expensive, very reasonable. Interestingly, the size of the house has a negative correlation with the unit price (-0.37). It seems people prefer the smaller houses. We also find many variables are correlated, such as Maximus.Building.Coverage.Ratio... and Maximus.Floor.area.Ratio.... We will remove one of those correlated variables before modeling.

```{r}
numericVars <- which(sapply(all, is.numeric))
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

# Sort on decreasing correlations with Transaction.price.Unit.price.m.2.
cor_sorted <- as.matrix(sort(cor_numVar[,'Transaction.price.Unit.price.m.2.'], decreasing = TRUE))
# Display all correlation
Cor <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.)))
cor_numVar <- cor_numVar[Cor, Cor]

png(file.path(dir,"Result","CorrelationvarNum.png"))
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
dev.off()
```

![CorrelationvarNum](/Result/CorrelationvarNum.png?raw=true)
### 5.2.1. The correlation between categorical (factor) variables and the unit price.
We move to check the correlation between categorical variables (factor variables) and the unit price with a method called random forest. However, random forest cannot handle variables with NAs, either factor variables with more than 53 levels. We choose the top 53 frequent stations and areas for this analysis.Finally, we need to exclude NAs from the data. We "impute" those NAs by using random forest, and yes, perform random forest again to draw the important variables from this imputed data.

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