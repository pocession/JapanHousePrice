### This project is cloned and modified from 
### https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/
### To increase the readibility, I remove all the R codes from the readme file in this version. I annotate the corresponding section number in R script file instead.
# Version 3.0
# 1. Hightlight of this project
* The R-square of the current model is 62%.
* The minimal RMSE of the current model is 0.68, suggesting there is a 68% error of predicted price when compared to the real price. It needs to be furhter improved.
* This model can be applied to all districts in Kanagawa. 
* This model is scalable, means it could be adjusted to predict house prices across whole Japan.  
# 2. Introduction
* I start this project to understand realestate market in Japan and to practice my data analysis skills. The idea behind this analysis is documented in each section of the README file. To increase the readibility, I remove most of R codes from the README file. The annotation of R codes can be in each section of the R file.
* To shorted the running time, the plotting commands in the R file are hidden. 
* In this version, only data of Kanagawa prefecture is used for modeling.  
# 3. Data and Files
* The data is originally collected from [Land General information system](https://www.land.mlit.go.jp/webland/servlet/MainServlet). Due to the large size of data files, data are not uploaded to the Github repository.
* The R files are in [Script](Script/).
* All plots are in [Result](Result/).

# 4. Loading and exploring data
## 4.1. Loading required libraries
Packages used for this project are listed below.
```{r}
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
```

## 4.2. Loading csv data into R
In this version, I only use data from Kanagawa prefecture. There should be total 29 variables in the raw data.

## 4.3. Data wrangling
Before performing the analysis, the data needs to be wrangled. I first wrangle numeric and then character variables. 
### 4.3.1 Numeric variables
Let's wrangle the numeric data first. I find some variables are actually numeric but become character after data loading. Some variables need to be further processed and transformed to numeric.
```{r}
str(Raw)

 'data.frame':	32048 obs. of  29 variables:
 $ No                                : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Type                              : chr  "Residential Land(Land Only)" "Pre-owned Condominiums, etc." "Pre-owned Condominiums, etc." "Pre-owned Condominiums, etc." ...
 $ Region                            : chr  "Residential Area" "" "" "" ...
 $ City.Town.Ward.Village.code       : int  37201 37201 37201 37201 37201 37201 37201 37201 37201 37201 ...
 $ Prefecture                        : chr  "Kagawa Prefecture" "Kagawa Prefecture" "Kagawa Prefecture" "Kagawa Prefecture" ...
 $ City.Town.Ward.Village            : chr  "Takamatsu City" "Takamatsu City" "Takamatsu City" "Takamatsu City" ...
 $ Area                              : chr  "Akanecho" "Akanecho" "Akanecho" "Akanecho" ...
 $ Nearest.station.Name              : chr  "Showacho (Kagawa)" "Showacho (Kagawa)" "Showacho (Kagawa)" "Showacho (Kagawa)" ...
 $ Nearest.station.Distance.minute.  : chr  "15" "10" "14" "13" ...
 $ Transaction.price.total.          : num  2.6e+07 5.5e+06 8.3e+06 6.5e+06 6.4e+06 6.3e+06 1.3e+07 1.2e+07 8.5e+06 7.5e+06 ...
 $ Layout                            : chr  "" "3DK" "3LDK" "3LDK" ...
 $ Area.m.2.                         : chr  "520" "55" "70" "60" ...
 $ Transaction.price.Unit.price.m.2. : int  50000 NA NA NA NA NA NA 75000 NA NA ...
 $ Land.shape                        : chr  "Semi-rectangular Shaped" "" "" "" ...
 $ Frontage                          : chr  "15.5" "" "" "" ...
 $ Total.floor.area.m.2.             : chr  "" "" "" "" ...
 $ Year.of.construction              : chr  "" "1990" "1995" "1990" ...
 $ Building.structure                : chr  "" "SRC" "RC" "SRC" ...
 $ Use                               : chr  "" "House" "House" "House" ...
 $ Purpose.of.Use                    : chr  "" "House" "House" "House" ...
 $ Frontage.road.Direction           : chr  "East" "" "" "" ...
 $ Frontage.road.Classification      : chr  "City Road" "" "" "" ...
 $ Frontage.road.Breadth.m.          : num  4.5 NA NA NA NA NA NA 12 4.5 NA ...
 $ City.Planning                     : chr  "Quasi-industrial Zone" "Quasi-industrial Zone" "Quasi-industrial Zone" "Quasi-industrial Zone" ...
 $ Maximus.Building.Coverage.Ratio...: int  60 60 60 60 60 60 60 60 60 60 ...
 $ Maximus.Floor.area.Ratio...       : int  200 200 200 200 200 200 200 200 200 200 ...
 $ Transaction.period                : chr  "4th quarter 2020" "4th quarter 2020" "4th quarter 2020" "3rd quarter 2020" ...
 $ Renovation                        : chr  "" "Not yet" "Not yet" "Done" ...
 $ Transactional.factors             : chr  "" "" "" "" ...
```
#### "Transaction.price.total.": This is our dependent varialbe as well as the most important variable. I notice that there are some outliers. Since I will do many processings later, those outliers may be removed. I will check whether there are still outliers left before modeling. The distribution is left-skewed. I will correct the skewness before modeling, too.
```{r}
summary(log10(Raw$Transaction.price.total.))
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 2.000   6.447   6.845   6.752   7.176   9.799 
```
![Price](/Result/Price.png?raw=true)  
#### "Year": The year variable should be divided into year and quarters.
#### "Area.m.2", "Nearest.station.Distance.minute.", "Total.floor.area.m.2.", "Year.of.construction", "Year": After reading the data into dataframe, these variables may not be numeric. I use 'as.numeric()' to make sure these variables are numeric.
#### "Nearest.station.Distance.minute."
* This variable contains both numbers and characters and needs to be further processed. I first have a look at the data.
```{r}
uniqure(Raw$Nearest.station.Distance.minute.)
[1] "15"           "10"           "14"           "13"           "12"           "29"           "30-60minutes"
[8] "26"           "18"           "19"           "17"           "16"           "20"           "1H30-2H"     
[15] "1H-1H30"      ""             "25"           "21"           "23"           "24"           "28"          
[22] "6"            "4"            "9"            "7"            "2"            "8"            "5"           
[29] "11"           "22"           "3"            "1"            "0"            "27"           "2H-"  
```

* There are some data not labelled as minutes. I first transform those data following the below rules. For house with distance longer than 2h and for houses lacking the distance information, I apply two dummy numbers. 
```{r}
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
``` 
* After plotting the relationship between price and distance of station, I find Nearest.station.Distance.minute. is much like a factor variable, as the data could be clearly separate into three groups: 0min, 0-45min, and >45min. I will handle this at the final step.

#### "Frontage": I replace "50.0m or longer" as 50. There are a lot of missing values here, mostly are lands. I will deal with this in the later section.
#### "City.Town.Ward.Village.code" is not a numeric variable. I transform it as a factor variable.

There should be 12 numeric variables now. Keep in mind that "No" is just an ID, not a real variable.  
```{r}
numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
numericVarNames
cat('There are', length(numericVars), 'numeric variables')

 [1] "No"                                 "Nearest.station.Distance.minute."   "Transaction.price.total."          
 [4] "Area.m.2."                          "Transaction.price.Unit.price.m.2."  "Frontage"                          
 [7] "Total.floor.area.m.2."              "Year.of.construction"               "Frontage.road.Breadth.m."          
[10] "Maximus.Building.Coverage.Ratio..." "Maximus.Floor.area.Ratio..."        "Year"

There are 12 numeric variables
```
### 4.3.2 A glimpse of numeric variables

The importance of each numeric variable is revealed as the following figure.
![CorrelationvarNum](/Result/CorrelationvarNum.png?raw=true)

#### "Total.floor.area.m.2.": This variable is the only numeric variable with a high correlation with our dependent variable. This makes sense. Houses with more floor area are expensive.

![Price_floor_area](/Result/Price_floor_area.png?raw=true)

## 5 Dealing with missing values
Now I move on to deal with the missing values. I find that missing values are shown in NA or "" (Blank) in this dataset. Some missing values in numeric variables are associated with specific values in character variables. This suggests that those variables are in a group and should be handled together. I frist start to identify NAs in numeric variables and deal with their associated values in character variables. There should be 20 variables containing missing values, which are listed below.  

```{r}
blankcol <- which(sapply(Raw,function(x) any(x== "")))
NAcol <- which(colSums(is.na(Raw)) > 0)
missingcol<-union(names(blankcol),names(NAcol))
cat('There are', length(missingcol), 'variables containing missing values')
missingcol

There are 20 variables containing missing values

 [1] "Region"                             "Nearest.station.Name"               "Layout"                            
 [4] "Land.shape"                         "Building.structure"                 "Use"                               
 [7] "Purpose.of.Use"                     "Frontage.road.Direction"            "Frontage.road.Classification"      
[10] "City.Planning"                      "Renovation"                         "Transactional.factors"             
[13] "Area.m.2."                          "Transaction.price.Unit.price.m.2."  "Frontage"                          
[16] "Total.floor.area.m.2."              "Year.of.construction"               "Frontage.road.Breadth.m."          
[19] "Maximus.Building.Coverage.Ratio..." "Maximus.Floor.area.Ratio..."       
```
I will only discuss those real estates containing houses. I first exclude data belong to land properties. For character variables, I will assign "No_information" to missing values. For numeric variables, I will assign either 0 or a dummy number to NAs, depending on the data properties. 

#### "Region": Only a small fraction of NAs are related to Pre-owned Condominiums. Assign "No_information" to them. 
#### "Nearest.station.Name": this variable is related to "Nearest.station.Distance.minute.". So let's identify data that contain missing values in "Nearest.station.Name" and "Nearest.station.Distance.minute.". Note NAs in "Nearest.station.Distance.minute." have been transformed to 165 in previous section. We then assign "No_station" to those data. Lastly, we exclude data that contains station names but without any distance information.
#### "Transaction.price.Unit.price.m.2.": As there are already  "Transaction.price.Unit.price.total." and "Area.m.2.", this variable is redundant. I will exclude this variable from the data set later.
#### "Frontage.road.Direction": this variable is related to other two variables: "Frontage.road.Classification" and "Frontage.road.Breadth.m.". If the data does not contain any facing road, then NAs are also shown in other two variables. Let's check whether number of the NAs in "Frontage.road.Breadth.m." matches the number of "No facing road" in "Frontage.road.Direction" as well as the number of "" (Blank) in "Frontage.road.Classification.". In the final step, we replace the NAs in Frontage.road.Breadth.m. with 0.
#### Frontage: this variable means how long the front of a house is connected to the road. Therefore, if there is no road connected to this house, the frontage will be zero. I frist assign 0 to NAs where the "Frontage.road.Breadth.m."　is also 0. I still have 1037 houses connected to a road but without any Frontage information. As the house price has a very small positive correlation with the frontage, I decide to give a negative value to those houses lacking frontage information. 


Before assigning 0:
![Price_Frontage0](/Result/Price_Frontage0.png?raw=true)  

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
We now check the correlation between categorical variables (factor variables) and the unit price with a method called random forest. However, random forest cannot handle variables with NAs, either factor variables with more than 53 levels. We already deal with the missing values before. Now We have to reduce the levels of Nearest.station.Name and Area. I try to select the top 36 Areas where the house are traded most frequently. This will also reduce the nearest stations to 51. 

```{r}
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
```
As a result, we can see the unit price is still largely affected by its built year. But we also identify that the unit price is affected by its nearest stations and areas. We will do more feature engineering for these two variables before modeling. 
![randomForest](/Result/randomForest.png?raw=true)
# 6. Feature engineering
Hu ~~ now we still need "transform" or "re-engineering" some variables before modeling. We will take care those variables here: Year, Year.of.Constuction, Area, and Nearest.station.Name.
* Age: I am now creating a new variable "Age" based on "Year" and "Year.of.Construction". As you can see, there is a negative correlation between house age and its price. (Correlation = -0.400)  
![Unit_price_age](/Result/Unit_price_age.png?raw=true)

  There are some extremely expensive houses. By taking these outliers, the correlation increase by 4.7%. (Correlation = -0.447)  
![Unit_price_age2](/Result/Unit_price_age2.png?raw=true)  
* IsNew: there are more than 1000 houses built and sold at the same year. I hypothesize those new houses could be sold at a higher price. After visualizing the unit price, my hypothesis is right. Therefore, I create another variable called IsNew to represent new houses.  
![Unit_price_IsNew](/Result/Unit_price_IsNew.png?raw=true)  
* Nearest.station.Name.: we can see nearest stations could be further separated based on their median unit price. I do not want over bin the stations. So let's separate names into 5 groups. The first group contains only Kawaramachi station.
![Unit_price_station](/Result/Unit_price_station.png?raw=true)
![Unit_price_station2](/Result/Unit_price_station2.png?raw=true)  
* Area: there are more than 500 areas in our data. Similar as stations, we will categorize areas into five groups based on their median unit price. The first group contains only three three areas.
![Unit_price_area](/Result/Unit_price_area.png?raw=true)
![Unit_price_area2](/Result/Unit_price_area2.png?raw=true)  
* Frontage: the frontage has a negative correlation with the unit price. We won't perform any engineering here.
![Unit_price_frontage](/Result/Unit_price_frontage.png?raw=true)
```{r}
# Feature variables ----------------------------------------------------------------------------------------------------
# Transform to year.of.Constuction to Age
all$Age <- all$Year - all$Year.of.construction
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
```
# 7. Preparing data for modeling
## 7.1. Drop variables and correct the variable categories
We now need to drop some variables in our data.  
* Nearest.station.Name and Area: will be replaced by Stationgroup and Areagroup during modeling, respectively.
* Year, Year.of.construction: will be replaced by Age.
* Prefecture: in this current version, we will only analyze the house price in Kanagawa. So this variable will be droped.
* Stationgroup and Areagroup: should be in numeric variables.  
## 7.2. 
# Modeling