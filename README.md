### This project is cloned and modified from https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/

# Version 0.1
# 1. Summary
I start this project by focusing on understanding Japanese realestate market and preacticing my data analysis skills. The EDA and modeling are documented in each section.

In this version, only data of Kanagawa prefecture is used for modeling.  

* The data is collected from [Land General information system](https://www.land.mlit.go.jp/webland/servlet/MainServlet). 
* Describe modeling methods used in this project.

# 2. Files
* All data are in [Raw](Raw/).
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

Raw <- read.csv(file.path(dir,"Raw",files[37]), stringsAsFactors = F)

# A quick wrangling
Raw <- Raw %>%
  separate(Transaction.period, c("quarter.1", "quarter.2", "Year"), sep = " ")

# We only focus on those real estate used for house
Raw <- Raw %>% 
  filter(Use == "House")
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

write.csv(train,file.path(dir,"Raw","train.csv"))
write.csv(test,file.path(dir,"Raw","test.csv"))
```

# 5. Exploring some of the most important variables
## 5.1. The depenent variable, Transaction.price.total.
Apparently the distribution of price is right skewed. It is because only a few real estates with extreme pries are transacted. Those data may be outliers and may need to be removed during analysis. Let's keep the data now but will remove before modeling.  
[Transaction.price.total.](/Result/Transaction.price.total.png?raw=true)
# Missing data, label encoding and factorizing variables
# Visualization of important variables
# Feature engineering
# Preparing data for modeling
# Modeling