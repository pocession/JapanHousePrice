### This project is cloned and modified from https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/

# Summary
I start this project by focusing on understanding Japanese realestate market and preacticing my data analysis skill. The EDA and modeling are documented in each section.

* Describe modeling methods used in this project.

# Introduction
# Loading and exploring data
Loading required libraries and read csv data into R.

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

Below, I am reading the the csv as dataframes into R.

```{r}
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
```

Then I split the data into train and test dataset. I save both datasets as csv file.

```{r}
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
```

Then read the train dataset.
```{r}
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
```

## Train data
## Test data

* Describe how to split data into train and test data set.

# Exploring some of the most important variables
# Missing data, label encoding and factorizing variables
# Visualization of important variables
# Feature engineering
# Preparing data for modeling
# Modeling