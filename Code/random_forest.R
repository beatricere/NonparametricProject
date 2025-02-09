rm(list = ls())

# Load necessary libraries
library(randomForest)
library(caret)
library(pROC)
library(glmnet)

# Load the data
data <- readRDS("C:\\Users\\jclaz\\Downloads\\dataset_NAs_fixed.rds")

# Remove the 'id' column from the dataset
data_clean <- data[, !names(data) %in% c("id, group")]

# Prepare the data: Remove any rows with missing values for the target variable (bacteremia)
data_clean <- na.omit(data_clean)

# Convert the 'bacteremia' variable into a factor for classification
data_clean$bacteremia <- as.factor(data_clean$bacteremia)

# Split the data into training and test sets with stratified sampling
set.seed(09122024)
train_index <- createDataPartition(data_clean$bacteremia, p = 0.8, list = FALSE)
train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

# Check the proportions of bacteremia in both train and test sets
cat("Training set proportions:\n")
print(prop.table(table(train_data$bacteremia)))

cat("Test set proportions:\n")
print(prop.table(table(test_data$bacteremia)))

# Train a Random Forest model to predict bacteremia using all variables
rf_model <- randomForest(bacteremia ~ ., data = train_data, importance = TRUE)

# Print model summary
print(rf_model)

# Plot feature importance
varImpPlot(rf_model)

# Predict on the test set
predictions <- predict(rf_model, test_data)

# Confusion Matrix
conf_matrix <- confusionMatrix(predictions, test_data$bacteremia)

# Print the confusion matrix
print(conf_matrix)

# Metrics: Accuracy, Precision, Recall, F1 Score
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value'] # Precision for the positive class
recall <- conf_matrix$byClass['Sensitivity']     # Recall for the positive class
f1_score <- 2 * ((precision * recall) / (precision + recall))  # F1 Score

# Print the metrics
cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1 Score: ", f1_score, "\n")

attach(data)
# Build the matrix of predictors
x <- model.matrix(bacteremia~wbc+neu+lym+mono+eos+baso+crp+gbil+plt+ldh)[,-1]
# Build the vector of response
y <- bacteremia

# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- 10^seq(5,-3,length=100)
fit.lasso <- glmnet(x,y,family = "binomial", lambda = lambda.grid) # default: alpha=1 -> lasso 
# [note: if alpha=0 -> ridge regression]
par(mfrow=c(1,1))
plot(fit.lasso, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,family="binomial", lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

optlam.lasso <- cv.lasso$lambda.1se
optlam.lasso

plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)
abline(v=log(optlam.lasso), lty=1)

# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso

coef.lasso <- predict(fit.lasso, s=optlam.lasso, type = 'coefficients')
coef.lasso





# Assuming 'age' is a column in the dataset representing the age of the patients
data_clean$age_group <- cut(data_clean$age,
                            breaks = c(18, 30, 40, 50, 60, 70, 80, 90, 100),
                            labels = c("18-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"),
                            right = FALSE)

# Check the distribution of the new age groups
cat("Age group distribution:\n")
print(table(data_clean$age_group))

# Split the data by age groups
age_groups <- levels(data_clean$age_group)
age_group_models <- list()

# Create custom age groupings: 18-30, 30-60, 60+
data_clean$age_group_custom <- cut(data_clean$age,
                                   breaks = c(18, 30, 60, 100),
                                   labels = c("18-30", "30-60", "60+"),
                                   right = FALSE)

# Check the distribution of the custom age groups
cat("Custom age group distribution:\n")
print(table(data_clean$age_group_custom))

# Split the data by the custom age groups
custom_age_groups <- levels(data_clean$age_group_custom)
custom_age_group_models <- list()

# Visualizzare l'importanza delle caratteristiche per ogni modello Random Forest
for (age_group in age_groups) {
  # Subset the data for the current age group
  group_data <- subset(data_clean, age_group == age_group)
  
  # Split into training and test sets for each age group
  train_index <- createDataPartition(group_data$bacteremia, p = 0.8, list = FALSE)
  train_data <- group_data[train_index, ]
  test_data <- group_data[-train_index, ]
  
  # Train a Random Forest model for this age group
  rf_model <- randomForest(bacteremia ~ ., data = train_data, importance = TRUE)
  
  # Store the model
  age_group_models[[age_group]] <- rf_model
  
  # Predict on the test set
  predictions <- predict(rf_model, test_data)
  
  # Print confusion matrix for this model
  cat("\nConfusion Matrix for Age Group", age_group, ":\n")
  conf_matrix <- confusionMatrix(predictions, test_data$bacteremia)
  print(conf_matrix)
  
  # Plot feature importance for this model
  cat("\nFeature Importance for Age Group", age_group, ":\n")
  varImpPlot(rf_model, main = paste("Feature Importance - Age Group", age_group))
}

# Esegui lo stesso processo per i gruppi di età personalizzati (18-30, 30-60, 60+)
for (age_group in custom_age_groups) {
  # Subset the data for the current custom age group
  group_data <- subset(data_clean, age_group_custom == age_group)
  
  # Split into training and test sets for each custom age group
  train_index <- createDataPartition(group_data$bacteremia, p = 0.8, list = FALSE)
  train_data <- group_data[train_index, ]
  test_data <- group_data[-train_index, ]
  
  # Train a Random Forest model for this custom age group
  rf_model <- randomForest(bacteremia ~ ., data = train_data, importance = TRUE)
  
  # Store the model
  custom_age_group_models[[age_group]] <- rf_model
  
  # Predict on the test set
  predictions <- predict(rf_model, test_data)
  
  # Print confusion matrix for this model
  cat("\nConfusion Matrix for Custom Age Group", age_group, ":\n")
  conf_matrix <- confusionMatrix(predictions, test_data$bacteremia)
  print(conf_matrix)
  
  # Plot feature importance for this model
  cat("\nFeature Importance for Custom Age Group", age_group, ":\n")
  varImpPlot(rf_model, main = paste("Feature Importance - Custom Age Group", age_group))
}