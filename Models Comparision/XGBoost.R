library(xgboost)
library(caret)

# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]


model <- xgboost(data = as.matrix(training[,-c(14)]), label = training$target,
                 objective = "binary:logistic", nrounds = 100)


prediction <- predict(model, as.matrix(testing[,-c(14)]))

# Convert prediction and testing$target to factors with the same levels
levels <- c("0", "1")
prediction <- factor(ifelse(prediction > 0.5, 1, 0), levels = levels)
testing$target <- factor(testing$target, levels = levels)

conf_mat <- confusionMatrix(prediction, testing$target)
conf_mat

Accuracy <- conf_mat$overall['Accuracy']
Accuracy

f1_score <- conf_mat$byClass['F1']
f1_score

recall <- conf_mat$byClass['Recall']
recall

precision <- conf_mat$byClass['Precision']
precision





# Using Hyperparameter Tuning
# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

# Define parameter grid for hyperparameter tuning
param_grid <- expand.grid(
  nrounds = 50,
  max_depth = 2:4,
  eta = c(0.01, 0.1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 0.5
)

# Train model with hyperparameter tuning
xgb_model <- train(
  x = as.matrix(training[,-c(14)]),
  y = training$target,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = param_grid,
  objective = "binary:logistic"
)

# Make predictions on testing set
predictions <- predict(xgb_model, as.matrix(testing[,-c(14)]))

# Convert prediction and testing$target to factors with the same levels
levels <- c("0", "1")
predictions <- factor(ifelse(predictions > 0.5, 1, 0), levels = levels)
testing$target <- factor(testing$target, levels = levels)

# Evaluate model performance using confusion matrix
conf_mat <- confusionMatrix(predictions, testing$target)
conf_mat

Accuracy <- conf_mat$overall['Accuracy']
Accuracy

f1_score <- conf_mat$byClass['F1']
f1_score

recall <- conf_mat$byClass['Recall']
recall

precision <- conf_mat$byClass['Precision']
precision




# Using Cross Validation
# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

# Define the parameters for XGBoost
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss"
)

# Perform cross-validation with 5 folds
cv <- xgb.cv(
  params = params,
  data = as.matrix(training[,-c(14)]),
  label = training$target,
  nfold = 5,
  nrounds = 100,
  verbose = TRUE
)

# Get the best number of rounds based on the minimum log loss
best_round <- which.min(cv$evaluation_log$test_logloss_mean)

# Train the final model with the best number of rounds
model <- xgboost(
  params = params,
  data = as.matrix(training[,-c(14)]),
  label = training$target,
  nrounds = best_round
)

# Make predictions on the test set
prediction <- predict(model, as.matrix(testing[,-c(14)]))

# Convert prediction and testing$target to factors with the same levels
levels <- c("0", "1")
prediction <- factor(ifelse(prediction > 0.5, 1, 0), levels = levels)
testing$target <- factor(testing$target, levels = levels)

# Compute the confusion matrix
conf_mat <- confusionMatrix(prediction, testing$target)
conf_mat

Accuracy <- conf_mat$overall['Accuracy']
Accuracy

f1_score <- conf_mat$byClass['F1']
f1_score

recall <- conf_mat$byClass['Recall']
recall

precision <- conf_mat$byClass['Precision']
precision