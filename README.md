### This project is cloned and modified from https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/

# Version 0.1
# Summary
I start this project by focusing on understanding Japanese realestate market and preacticing my data analysis skill. The EDA and modeling are documented in each section.

In this version, only data of Kanagawa prefecture is used for modeling.  

* Describe modeling methods used in this project.

# Files
* All data are in [Raw](Raw/).
* All R scripts in [Script](Script/).

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

Below, I am reading the the csv as dataframes into R and do some quick wrangling.

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

# Exploring some of the most important variables
# Missing data, label encoding and factorizing variables
# Visualization of important variables
# Feature engineering
# Preparing data for modeling
# Modeling