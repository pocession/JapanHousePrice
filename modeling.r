# Loading libraries ------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(corrplot)
library(scales)
library(randomForest)

# Read train and test data -----------------------------------------------------------------------------------------------
dir <- "E:/Dropbox (OIST)/Ishikawa Unit/Tsunghan/JapanHousePrice"
# dir <- "C:/Users/Tsunghan/Dropbox (OIST)/Ishikawa Unit/Tsunghan/JapanHousePrice/"
# Get the files names
all <- read.csv(file.path(dir,"Wrangled","wrangled.csv"))
train <- read.csv(file.path(dir,"Wrangled","train.csv"))
test <-  read.csv(file.path(dir,"Wrangled","test.csv"))

train <- train[2:ncol(train)]
test <- test[2:ncol(test)]

# Visualize the transaction price
ggsave(file.path(dir,"Result","Transaction.price.Unit.price.m2.png"))
ggplot(data=train[!is.na(train$Transaction.price.Unit.price.m.2.),], aes(x=Transaction.price.Unit.price.m.2.)) +
  geom_histogram(fill="blue", binwidth = 10000) 
#dev.off()

# Have a summary of Transaction.price.Unit.price.m2.
summary_Transaction.price.Unit.price.m2. <- summary(train$Transaction.price.Unit.price.m.2.)
summary_Transaction.price.Unit.price.m2.

numericVars <- which(sapply(train, is.numeric))
all_numVar <- train[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

# Sort on decreasing correlations with Transaction.price.Unit.price.m.2.
cor_sorted <- as.matrix(sort(cor_numVar[,'Transaction.price.Unit.price.m.2.'], decreasing = TRUE))
# Select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

# The variable selected to be shown here are based their correlation with Transaction.price.total.
# I want to show all variables, so I set the filter at a very low value. 
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

png(file.path(dir,"Result","Correlation.png"))
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
#dev.off()