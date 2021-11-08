### This project is cloned and modified from 
### https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/
### To increase the readibility, I remove all the R codes from the readme file in this version. I annotate the corresponding section number in R script file instead.
# Version 3.0
# 1. Hightlight of this project
* The R-square of the current model is 78%.
* The minimal RMSE of the current model is 0.3364, suggesting there is a 33.64% error of predicted price when compared to the real price. It's better than the previous version but still needs to be furhter improved.
* This model can be applied to all areas in Kanagawa. 
* This model cannot be used to predict house with trading price less than 3,000,000 yen.
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
library(mice)
library(gridExtra)
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

![Price_floor_area0](/Result/Price_floor_area0.png?raw=true)

# 5 Dealing with missing values
Missing values are shown in NA or "" (Blank) in this dataset. Some missing values in numeric variables are associated with specific values in character variables. This suggests that those variables are in a group and should be handled together. I frist start to identify NAs in numeric variables and deal with their associated values in character variables. There should be 20 variables containing missing values, which are listed below.  

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

Let's also check the distribution of missing values among each variables. As we can see, "Transaction.price.Unit.price.m.2." contains a lot of missing values. "Frontage.road.Breadth.m.", "Frontage" and "Total.floor.area.m.2." also contains a lot of missing values. We will start to deal with each variables and their missing values.
![Missing](/Result/Missing.png?raw=true)  
I will only discuss those real estates containing houses. I first exclude data belong to land properties. For character variables, I will assign "No_information" to missing values. For numeric variables, I will assign either 0 or a dummy number to NAs, depending on the data properties. 

* #### "Region": Only a small fraction of NAs are related to Pre-owned Condominiums. Assign "No_information" to them. 

* #### "Nearest.station.Name": this variable is related to "Nearest.station.Distance.minute.". So let's identify data that contain missing values in "Nearest.station.Name" and "Nearest.station.Distance.minute.". Note NAs in "Nearest.station.Distance.minute." have been transformed to 165 in previous section. We then assign "No_station" to those data. Lastly, we exclude data that contains station names but without any distance information.

* #### "Transaction.price.Unit.price.m.2.": As there are already  "Transaction.price.Unit.price.total." and "Area.m.2.", this variable is redundant. I will exclude this variable from the data set later.  

* #### "Frontage.road.Direction": this variable is related to other two variables: "Frontage.road.Classification" and "Frontage.road.Breadth.m.". If the data does not contain any facing road, then NAs are also shown in other two variables. Let's check whether number of the NAs in "Frontage.road.Breadth.m." matches the number of "No facing road" in "Frontage.road.Direction" as well as the number of "" (Blank) in "Frontage.road.Classification.". In the final step, we replace the NAs in Frontage.road.Breadth.m. with 0.  

* #### Frontage: This variable means how long the front of a house is connected to the road. There are total 3053 data containing missing values. The missing value may be because no road connected to this house. There are total 2016 data falling in this category and 1037 NAs are still remained. As I have no idea why these 1037 data are missing, it is not safe to impute or delete those data. As the correlation of this variable with the price is very low, I decide not to include this variable when building model in this version. At least, there are already three variables related to the road:  "Frontage.road.Breadth.m.", "Frontage.road.Direction", and "Frontage.road.Classification".  

Price and Frontage:
![Price_Frontage](/Result/Price_Frontage.png?raw=true)  

* #### "Total.floor.area.m.2.": This is the most important predictor. For apartment and house containing only one floor, the floor area equals to it area. Therefore, I just copy the value of "Area" to this variable and omit those data containing missing values. Now the correlation is more clear.I also notice that there are some outliers. I will deal with those data later.

Price and floor area (new):
![Price_floor_area1](/Result/Price_floor_area1.png?raw=true)  

* #### "Maximus.Building.Coverage.Ratio..." and "Maximus.Floor.area.Ratio...": The maximum coverage ratio and floor ratio is regulated by laws. I could not make any imputation about those missing values based on other variables. In fact, given the low relationship between price and these two variables, it is not necessary to keep these two variables. Therefore, I decide to dropout these two variables. 

Price and maximum buidling coverage ratio:
![Price_maxi_building_coverage](/Result/Price_maxi_building_coverage.png?raw=true) 
Price and maximum floor coverage ratio:
![Price_maxi_floor_coverage](/Result/Price_maxi_floor_coverage.png?raw=true)  

* #### "Year.of.construction": There is no way to impute the year when the house was built. I omit data containg missing values in this variable.  

Price and Year.of.construction:
![Price_year_construction](/Result/Price_year_construction.png?raw=true) 

* #### "Area", "Layout", "Land.shape", "building.structure", "Use, Purpose.of.Use", "City.Planning": Assign "No_information" to missing values in these three variables.  
* #### "Renovation", and "Transactional.factors": These two variables contain too many NAs and not useful. I remove these two variables in modeling. 

We should not have any variables containing missing values now.
```{r}
# Check which variable contains missing values. missing values could be NA or "" (Blank).
blankcol <- which(sapply(Raw,function(x) any(x== "")))
NAcol <- which(colSums(is.na(Raw)) > 0)
missingcol<-union(names(blankcol),names(NAcol))
cat('There are', length(missingcol), 'variables containing missing values')
missingcol

There are 0 variables containing missing values
```

I separate test and train data at this step. I will let the test dataset untouched and only use the train dataset in the following stage. Finally, I save wrangled, train and test dataset.  

# 6 Indetify and visualize important variables  
## 6.1 Factorize data
I drop the "No" and "Prefecture" variables first. In a version discussing the nationwide price, the "Prefecture" variable will be used. I then factorize all character variables.
## 6.2 Identify the important variables by random forest
Before performing any further feature engineering, I use random forest method to know which variable is important and deserves more efforts to be engineered. "Year.of.construction", "Type", "Building structure", "City.Town.Ward.Village.code", and "City.Planning" are important features (variables). Interestingly, remove "Use" seems to increase the accuracy of model.    

Random forest result:
![randomForest](/Result/randomForest.png?raw=true) 

## 6.3 Visualization
* ### "Total.floor.area.m.2.", "Area.m.2.": I already know both are highly correlated with price but "Total.floor.area.m.2." is more correlated with price (p value of floor area and area is 0.62 and 0.47, respectively). Since both variables are also co-dependent (correlation = 0.7), I will keep "Total.floor.area.m.2." and remove the other. I also identify some outliers and will remove them before modeling. And I notice that fewer house with total floor area larger than 1000 mm2 and also have higher price. I will create another variable to represent those big and expensive houses.
![Price_floor_area1](/Result/Price_floor_area1.png?raw=true)

* ### "Year.of.Construction", "Year": The price is highly correlated with the year of construction and is also little positively correlated with the year of sold. Therefore, I decide to create another variable representing the house of age. I also decide to create a variable to show the house is new or old when it is sold (Age < 1).   
![Year](/Result/Year.png?raw=true)

* ### The most important factor variable: "Building.structure": I see there are some houses with rare structrues. I decide to create a variable to represent common and rare structure. I also see there is a trend that house with certain structrues are more expensive. I will create another new variable to represent this trend (e.g., w (stands for woods) are more expensive than RC and SRC). 
![Structure](/Result/Structure.png?raw=true)  

# 7 Feature engineering
## 7.1 BigHouse
I first create a variable to represent house with floor area larger than 1000 mm2. And the mean price of big house is significantly than it of small house.

```{r}
t_test_floor <- t.test(log10(small_house),log10(big_house))
t_test_floor

	Welch Two Sample t-test

data:  log10(small_house) and log10(big_house)
t = -87.024, df = 745.64, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -1.078297 -1.030721
sample estimates:
mean of x mean of y 
 7.412883  8.467392
```  
![BigHouse](/Result/BigHouse.png?raw=true)  

## 7.2 Age
I create another variable "Age" to represent both "Year" and "Year.of.Constuction". The correlation between "Age", "Year", and "Year.of.Construction" is -0.102, 0.001, and 0.102, respectively. Hence, "Age" variable has alomost the same correlation as "Year.of.Construction". I also create "IsNew" variable to represent the new house.  

```{r}
t_test_new <- t.test(log10(old_house),log10(new_house))
t_test_new

Welch Two Sample t-test

data:  log10(old_house) and log10(new_house)
t = -196.81, df = 168333, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.2529317 -0.2479436
sample estimates:
mean of x mean of y 
 7.340772  7.591210 
```  
![Price_Age](/Result/Price_age.png?raw=true)
![IsNew](/Result/IsNew.png?raw=true) 

## 7.3 StructureQuality
I create a new variable "Structure Quality" to replace the "Building.structure" variable. The correlation between structure and price now is much clear (correlation = 0.106).

![Price_Structure_quality](/Result/Price_Structure_quality.png?raw=true)

## 7.4 StationDistance
I know there is a significant decrease of price when the closet station is more than 45 mins. Hence, I create a variable "StationDistance" to represent this.

![StationDistance](/Result/StationDistance.png?raw=true)  

# 8 Modeling
## 8.1 Drop highly correlated and unnecessary variables
I drop variables that are metioned before. I also drop some variables that contribute less variation, according to the results of random forest analysis. For factor variables, if the importance is more than 1%, I try to delete it and run the model. Now the combination of those following variables can create models with largest R-squared value and minimum RMSE. Here are some notes.

* Add 'Area', 'Nearest.station.Name' can increae the R2 by 0.1-0.2% and decrease the RMSE by 0.002%.
* Add 'Layout' can increase the R2 by 4% and slightly increase the RMSE by 0.001 - 0.003. 
* In opposite, Add 'Frontage.road.Classification' largely decrease the R2 by 10% and increase the RMSE by 0.11.

```{r}
dropVars2 <- c('Year.of.construction', 'Year', 'Area.m.2.', 'Building.structure', 'Frontage.road.Classification', 'Region', 'City.Town.Ward.Village', 'quarter.1', 'Use', 'Purpose.of.Use')

all <- Raw[,!(names(Raw) %in% dropVars2)]
```
## 8.2 Removing outliers 
In a previous version, I remove houese less than 10,000 yen. However, the data distribution is still moderately negative-skewed. Hence, I decide to remove house with price less than 3,000,0000 yen. In fact, it is very rare to buy houses less than 10,000,000 yen in Japan. I will try to exclude houses less than 10,000,000 in a later version.

## 8.3 Examine the skewness of numeric variables and make log transformation  
The skewness of numeric variables needs to be further tested. If the skewness of a variable is larger than 0.8, it will be log tranformed and centered.

## 8.4 Perform one-hot encoding for factor variables
For factor variables (but not ordinal variables), the values need to be transformed into 1 and 0. The model.matrix funciton is used for one-hot encoding.

```{r}
DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

[1] 166606   1804
```  

## 8.5 Check the skewness and perform natural log transformation of responsive variable  
For the the dependent variables, the skewness is 22.67. There it needs to be log tranform to reduce its skewness. I perform the natural log transformation and the skewness reduces to -0.1897. It is much better.

![skewness_price](/Result/skewness_price.png?raw=true)  
![skewness_price.2](/Result/skewness_price.2.png?raw=true)  

# 9 Modeling
## 9.1 Lasso
I use losso regression to build model. The lamda valus is set from 0.001 to 0.1 and increases by 0.0005.  I set he crossvalidation as 5. The final result of this model, R-squared is 0.7827 and RMSE-score is 0.3364. This suggests the model can explain 78% of the data variance and the error of predicted value to real value is 33.64%.

```{r}
lasso_mod$bestTune
alpha lambda
    1  0.001
min(lasso_mod$results$RMSE)
[1] 0.3364902
max(lasso_mod$results$Rsquared)
[1] 0.7826619
```

## 9.2 Ridge
The ridge modeling generates similar results as lasso.



