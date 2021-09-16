# Loading libraries ------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(corrplot)
library(scales)

# Read train and test data -----------------------------------------------------------------------------------------------
# dir <- "E:/Dropbox (OIST)/Ishikawa Unit/Tsunghan/JapanHousePrice"
dir <- "C:/Users/Tsunghan/Dropbox (OIST)/Ishikawa Unit/Tsunghan/JapanHousePrice/"
# Get the files names
train <- read.csv(file.path(dir,"Raw","train.csv"), stringsAsFactors = F)
test <-  read.csv(file.path(dir,"Raw","test.csv"), stringsAsFactors = F)

# Visualize the transaction price
ggsave(file.path(dir,"Result","Transaction.price.Unit.price.m2.png"))
ggplot(data=train[!is.na(train$Transaction.price.Unit.price.m.2.),], aes(x=Transaction.price.Unit.price.m.2.)) +
  geom_histogram(fill="blue", binwidth = 10000) 
#dev.off()

# Have a summary of Transaction.price.Unit.price.m2.
summary_Transaction.price.Unit.price.m2. <- summary(train$Transaction.price.Unit.price.m.2.)
summary_Transaction.price.Unit.price.m2.

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