library(dplyr)
library(tidyverse)

dir <- "E:/Dropbox (OIST)/Ishikawa Unit/Tsunghan/JapanHousePrice"

# Get the files names
files <- list.files(file.path(dir, "Raw"), pattern="*.csv")

# First apply read.csv, then rbind
Raw = do.call(rbind, lapply(file.path(dir, "Raw", files), function(x) read.csv(x, stringsAsFactors = FALSE)))

Raw <- Raw %>%
  separate(Transaction.period, c("quarter.1", "quarter.2", "Year"), sep = " ")

apartment <- Raw %>%
  filter(Type == "Pre-owned Condominiums, etc.")

write.csv(Raw,file.path(dir,"Raw","all.csv"))
write.csv(apartment,file.path(dir,"Wrangled","apartment.csv"))