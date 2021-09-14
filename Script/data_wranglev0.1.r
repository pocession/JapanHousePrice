library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(corrplot)

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

# Data wrangling character -----------------------------------------------------------------------------------------------------
# Separate train and test data -----------------------------------------------------------------------------------------------------
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

# Visualize the transaction price
ggsave(file.path(dir,"Result","Transaction.price.total.png"))
ggplot(data=Raw[!is.na(Raw$Transaction.price.total.),], aes(x=Transaction.price.total.)) +
  geom_histogram(fill="blue", binwidth = 1000000) 
#dev.off()

# Have a summary of Transaction.price.total.
summary_TRansaction.price.total <- summary(Raw$Transaction.price.total.)
summary_TRansaction.price.total

all_numVar <- Raw[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

# Sort on decreasing correlations with Transaction.price.total.
cor_sorted <- as.matrix(sort(cor_numVar[,'Transaction.price.total.'], decreasing = TRUE))
cor_sorted

# The variable selected to be shown here are based their correlation with Transaction.price.total.
# I want to show all variables, so I set the filter at a very low value. 
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

png(file.path(dir,"Result","Correlation.png"))
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
#dev.off()


