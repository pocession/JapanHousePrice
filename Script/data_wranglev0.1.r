library(dplyr)
library(tidyverse)

dir <- "E:/Dropbox (OIST)/Ishikawa Unit/Tsunghan/JapanHousePrice"

# Get the files names
files <- list.files(file.path(dir, "Raw"), pattern="*.csv")

Raw <- read.csv(file.path(dir,"Raw",files[37]), stringsAsFactors = F)

# A quick wrangling
Raw <- Raw %>%
  separate(Transaction.period, c("quarter.1", "quarter.2", "Year"), sep = " ")

# We only focus on those real estate used for house
Raw <- Raw %>% 
  filter(Use == "House")

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

