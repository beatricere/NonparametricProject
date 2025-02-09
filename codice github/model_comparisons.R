# SCRIPT FOR CONFORMAL PREDICTION
library(pracma)
library(rsample)
library(mgcv)
library(pROC)
library(leaps)
library(e1071)

bact <- readRDS("/Users/giuliadesanctis/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/project/dataset_NAs_fixed.RDS")
train <- read.csv("/Users/giuliadesanctis/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/project/train_test_val_data/train_data.csv")
test <- read.csv("/Users/giuliadesanctis/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/project/train_test_val_data/test_data.csv")
val <- read.csv("/Users/giuliadesanctis/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/project/train_test_val_data/val_data.csv")

alpha <- 0.1 

set.seed(42)

# Perform stratified split (e.g., 70% training, 30% calibration)
split <- initial_split(train, prop = 0.7, strata = bacteremia)

# Extract training and testing datasets
training.set <- training(split) # Proper Training Set
cal.set <- testing(split) # Calibration set


# FORWARD AND BACKWARD SEARCH ---------------------------------------------

regfit.fwd <- regsubsets(bacteremia~., data=training.set, method = "forward", nvmax = 48)
summary(regfit.fwd)

quartz()
par(mfrow=c(1,3))
plot(summary(regfit.fwd)$rsq,xlab="Number of Variables",ylab="R-squared",type="b")
plot(summary(regfit.fwd)$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b")
plot(summary(regfit.fwd)$rss,xlab="Number of Variables",ylab="RSS",type="b")

coef(regfit.fwd,10)

regfit.bwd <- regsubsets(bacteremia~., data=training.set, method = "backward", nvmax = 48)
summary(regfit.bwd)

quartz()
par(mfrow=c(1,3))
plot(summary(regfit.bwd)$rsq,xlab="Number of Variables",ylab="R-squared",type="b")
plot(summary(regfit.bwd)$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b")
plot(summary(regfit.bwd)$rss,xlab="Number of Variables",ylab="RSS",type="b")

coef(regfit.bwd,10)

# Fit the models 
logistic_fwd <- glm(bacteremia ~ age+mono+eos+sodium+mg+bun+ap+glu+crp+monor, training.set, family = 'binomial')
logistic_bwd <- glm(bacteremia ~ age+mono+sodium+mg+bun+ap+glu+crp+eosr+monor , training.set, family = 'binomial')

summary(logistic_fwd)
summary(logistic_bwd)

# Predict probabilities
pred_probs <- predict(logistic_fwd, newdata = val, type = 'response')

roc_obj <- roc(val$bacteremia, pred_probs)
plot(roc_obj, main ="ROC curve - Forward Selection")
roc_obj


# LOGISTIC CLASSIFIER DSAE SELECTED VARIABLES -----------------------------------------------------

# STEP 1: train 
logistic_0.7 <- glm(bacteremia ~ age+rdw+potass+phos+crea+bun+hs+gbil+tp+alb+ap+ggt+ck+glu, 
                    training.set, family = 'binomial')
logistic_0.8 <- glm(bacteremia ~ potass+phos+crea+bun+hs+tp+ap+ggt+ck+glu, training.set, family = 'binomial')
logistic_0.9 <- glm(bacteremia ~ phos+bun+ap+ggt+glu, training.set, family = 'binomial')

summary(logistic_0.9)

shitty_log <- glm(bacteremia ~ gbil+crp+plt+lymr+monor, training.set, family = 'binomial')
summary(shitty_log)


# GAM ---------------------------------------------------------------------

# Train a GAM model for binary classification

# GAM with variables chosen from DSAE delta = 0.7
gam_model_0.7 <- gam(bacteremia ~ s(age)+s(rdw)+s(potass)+ s(phos)+s(crea)+s(bun)
                     +s(hs)+s(gbil)+s(tp)+s(alb)+s(ap)+s(ggt)+s(ck)+s(glu),
                     family = binomial, 
                     data = training.set)

# Model Summary
summary(gam_model_0.7)

# GAM with variables chosen from DSAE delta = 0.8
gam_model_0.8 <- gam(bacteremia ~ s(potass)+ s(phos)+s(crea)+s(bun)+s(hs)+s(tp)+
                       s(ap)+s(ggt)+s(ck)+s(glu),
                 family = binomial, 
                 data = training.set)

# Model Summary
summary(gam_model_0.8)

# GAM with variables chosen from DSAE delta = 0.9
gam_model_0.9 <- gam(bacteremia ~ s(phos)+s(bun)+s(ap)+s(ggt)+s(glu), 
                     family = binomial, 
                     data = training.set)

# Model Summary
summary(gam_model_0.9)

# GAM with variables chosen from forward selection
gam_model_fwd <- gam(bacteremia ~ s(age)+s(mono)+s(eos)+s(sodium)+s(mg)+s(bun)+
                       s(ap)+s(glu)+s(crp)+s(monor),
                     family = binomial, data = training.set)
summary(gam_model_fwd)


# SVM ---------------------------------------------------------------------

svmfit <- svm(bacteremia~age+mono+sodium+mg+bun+ap+crp+eosr+lymr+monor, 
              data=training.set , kernel ='radial', cost =1000, scale =FALSE, probability = TRUE)
summary(svmfit)
svmfit$index


library(ROCR)
pred <- predict(svmfit, val, type = "prob", probability = TRUE)
roc_score=roc(val$bacteremia, pred) #AUC score
roc_score
plot(roc_score ,main ="ROC curve - SVM")

library(caret)
example <- confusionMatrix(data=predicted_value, reference = expected_value)

# CONFORMAL ---------------------------------------------------------------

training.fit <- gam_model_fwd

# STEP 2: compute calibration scores 

LAC <- function(X){
  P_true <- predict(training.fit, newdata=X, type='response')
  LAC.byclass <- as.matrix(cbind(P_true, 1 - P_true))
  return (LAC.byclass)
}

all.calibration.scores <- LAC(cal.set)

# Calibration scores computation based on the calibration set
n = dim(cal.set)[1]
calibration.scores <- numeric(n)

for (i in 1:n){
  y.correct <- as.numeric(cal.set$bacteremia[i]) + 1
  calibration.scores[i] <- all.calibration.scores[i, y.correct]
}

# STEP 3: compute the threshold as a quantile

alpha <- 0.1
n <- dim(all.calibration.scores)[1]

# Quantile level computation starting from alpha and n
quantile.level <- ceiling((n + 1) * (1 - alpha)) / n

# Threshold for the Conformal region based on alpha and calibration set
qhat.alpha <- quantile(calibration.scores, quantile.level)
qhat.alpha

# STEP 4: Inference time (on validation set / test set): 

n.val <- dim(val)[1]
val.LAC.scores <- LAC(val)
conformal.val.set <- val.LAC.scores <= qhat.alpha
conformal.val.set

# Verify the conformal region guarantee
# We need to pick the true label of the observations that were classified 
# as class 0 by the conformal classifier
class0.conf.pred.idxs <- as.numeric(which(conformal.val.set[,1] == TRUE))
class0.true.labels <- val[class0.conf.pred.idxs,]$bacteremia
class0.true.labels

class1.conf.pred.idxs <- as.numeric(which(conformal.val.set[,2] == TRUE))
class1.true.labels <- val[class1.conf.pred.idxs,]$bacteremia 
class1.true.labels

# Conformal region gurantee: is the coverage of the conformal sets greater or 
# equal than 1 - alpha?
numerator <- sum((class0.true.labels == FALSE)) + sum((class1.true.labels == TRUE))
denominator <- length(class0.true.labels) + length(class1.true.labels)

prob.ytrue.belongs.to.conformal.set <- numerator/denominator
prob.ytrue.belongs.to.conformal.set
prob.ytrue.belongs.to.conformal.set >= 1 - alpha

# Confusion matrix 
true.labels <- val$bacteremia  

# Predicted labels: classify as class 1 if conformal.val.set[,2] is TRUE, otherwise class 0
predicted.labels <- ifelse(conformal.val.set[,2] == TRUE, 1, 0)

# Print the confusion matrix
confusion.matrix <- table(Predicted = predicted.labels, Actual = true.labels)
print(confusion.matrix)

# ROC CURVE AND CONFUSION MATRIX ---------------------------------------------------------------
library(pROC)

quartz()

#model <- shitty_log
#pred_probs <- predict(model, newdata = test, type = 'response')
#roc_score=roc(test$bacteremia, pred_probs) #AUC score
#roc_score
#plot(roc_score ,main ="ROC curve test set - original method")

model <- gam_model_0.9
pred_probs <- predict(model, newdata = val, type = 'response')
roc_score=roc(val$bacteremia, pred_probs) #AUC score
roc_score
plot(roc_score ,main ="ROC curve test set - GAM with Forward Selection")

# Convert probabilities to binary predictions (threshold 0.5)
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(factor(pred_class, levels = c(0, 1)), factor(val$bacteremia, levels = c(0, 1)))
print(conf_matrix)

# Calculate metrics
TN <- conf_matrix[1, 1]  # True Negative
FP <- conf_matrix[1, 2]  # False Positive
FN <- conf_matrix[2, 1]  # False Negative
TP <- conf_matrix[2, 2]  # True Positive

# Accuracy
accuracy <- (TP + TN) / sum(conf_matrix)
accuracy

# Precision (Positive Predictive Value)
precision <- TP / (TP + FP)
precision

# Recall (Sensitivity, True Positive Rate)
recall <- TP / (TP + FN)
recall

# F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")
