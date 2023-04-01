library(rpart)
library(caret)

# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

model <- rpart(target ~., data = training, method = "class")

prediction <- predict(model, testing, type = "class")

# Convert prediction and testing$target to factors
prediction <- factor(prediction)
testing$target <- factor(testing$target)

# Ensure that the factor levels are consistent
levels(prediction) <- levels(testing$target)

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

# Convert target variable to a factor with two levels
training$target <- factor(training$target, levels = c(0, 1))

# Define parameter grid for tuning
param_grid <- expand.grid(
  cp = seq(0.01, 0.5, by = 0.01)
)

cv <- trainControl(method = "cv", number = 10)

# Train model with cross-validation and hyperparameter tuning
rpart_model <- train(
  x = training[, -14], # exclude target column
  y = training$target,
  method = "rpart",
  trControl = cv,
  tuneGrid = param_grid
)

# Make predictions on test data
prediction <- predict(rpart_model, testing[, -14], type = "raw")

# Convert prediction and testing$target to factors
prediction <- factor(prediction)
testing$target <- factor(testing$target)

# Ensure that the factor levels are consistent
levels(prediction) <- levels(testing$target)

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




# Using Cross Validation
# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Convert target column to factor
data$target <- as.factor(data$target)

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

# Set up 5-fold cross-validation
folds <- createFolds(data$target, k = 5, list = TRUE)

# Define the parameter grid to search over
param_grid <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))

# Perform 5-fold cross-validation using the parameter grid
rpart_model <- train(
  x = data[, -14], # exclude target column
  y = data$target,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, index = folds),
  tuneGrid = param_grid
)

# Make predictions on the testing set
prediction <- predict(rpart_model, testing[, -14], type = "raw")

# Evaluate the model's performance
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