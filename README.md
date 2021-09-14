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
# 4.1. Loading required libraries

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

# 4.2. Loading csv data into R

```{r}
# Get the files names
files <- list.files(file.path(dir, "Raw"), pattern="*.csv")

# No is not a variable
Raw <- read.csv(file.path(dir,"Raw",files[37]), stringsAsFactors = F)
Raw <- Raw[,2:ncol(Raw)]
```
# 4.3. Data wrangling
Let's wrangle the numeric data first. In the final, we should have 10 numeirc variables.
```{r}
# Data wrangling -----------------------------------------------------------------------------------------------------
# There are some works needs to be done before we start the analysis
# Let's check numeric data first and perform wrangling
# Year
Raw <- Raw %>%
  separate(Transaction.period, c("quarter.1", "quarter.2", "Year"), sep = " ")

# Check the numeric variables
# Let's do some wrangling to make some non-numeirc variables be numeric
# Area.m.2, Nearest.station.Distance.minute., Total.floor.area.m.2., Year.of.construction, Year, should be numeric variable
# City code is not a numeric variable
Raw$Area.m.2. <- as.numeric(Raw$Area.m.2.)
Raw$Total.floor.area.m.2.<- as.numeric(Raw$Total.floor.area.m.2.)
Raw$Year.of.construction <- as.numeric(Raw$Year.of.construction)
Raw$Year <- as.numeric(Raw$Year)

# Nearest.station.Distance.minute. needs more work to be numeric
unique(Raw$Nearest.station.Distance.minute.)

# Japanese era to Western era
index1 <- Raw$Nearest.station.Distance.minute. == "30-60minutes"
index2 <- Raw$Nearest.station.Distance.minute. == "1H-1H30"
index3 <- Raw$Nearest.station.Distance.minute. == "1H30-2H"
index4 <- Raw$Nearest.station.Distance.minute. == "2H-"
Raw$Nearest.station.Distance.minute.[index1] <- 60
Raw$Nearest.station.Distance.minute.[index2] <- 90
Raw$Nearest.station.Distance.minute.[index3] <- 120
Raw$Nearest.station.Distance.minute.[index4] <- 150 # This is a fake number, assign to house where it takes more than 120 minutes walking to nearest station  

# Check again
unique(Raw$Nearest.station.Distance.minute.)
Raw$Nearest.station.Distance.minute. <- as.numeric(Raw$Nearest.station.Distance.minute.)

# City.Town.Ward.Village.code is not a numeric variable
Raw$City.Town.Ward.Village.code <- as.factor(Raw$City.Town.Ward.Village.code)

numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
```

We should have 10 numeric variables. The list is below.
```{r}
names(numericVars)
 [1] "Nearest.station.Distance.minute."   "Transaction.price.total."           "Area.m.2."                         
 [4] "Transaction.price.Unit.price.m.2."  "Total.floor.area.m.2."              "Year.of.construction"              
 [7] "Frontage.road.Breadth.m."           "Maximus.Building.Coverage.Ratio..." "Maximus.Floor.area.Ratio..."       
[10] "Year"                   
```

```{r}
numericVars <- which(sapply(Raw, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

There are 10 numeric variables
```
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