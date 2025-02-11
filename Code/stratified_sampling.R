# In this script we create the training test and validation sets using stratified sampling
library(dplyr)
library(pracma)
library(rsample)

set.seed(123)  # For reproducibility

data <- readRDS("dataset_NAs_fixed.rds")

# Perform stratified split (e.g., 80% training, 20% testing)
split <- initial_split(data, prop = 0.85, strata = bacteremia)

# Extract training and testing datasets
train_data <- training(split)
test_data <- testing(split)

# Check proportions
table(train_data$bacteremia) / nrow(train_data)
table(test_data$bacteremia) / nrow(test_data)

# We do a further split to get a validation dataset 
split2 <- initial_split(train_data, prop = 0.85, strata = bacteremia)

# Extract training and validation datasets
train_data <- training(split2)
val_data <- testing(split2)

# Check proportions
table(train_data$bacteremia) / nrow(train_data)
table(val_data$bacteremia) / nrow(val_data)

# save the three datasets 
write.csv(train_data, "~/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/project/train_test_val_data/train_data.csv", row.names = F)
write.csv(test_data, "~/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/project/train_test_val_data/test_data.csv", row.names = F)
write.csv(val_data, "~/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/project/train_test_val_data/val_data.csv", row.names = F)
check <- read.csv("train_test_val_data/train_data.csv")

