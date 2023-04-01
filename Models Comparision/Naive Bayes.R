library(caret)
library(e1071)

# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

data$target <- factor(data$target, levels = c(0,1))

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

model <- naiveBayes(target ~., data = training)

prediction <- predict(model, testing)
prediction

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





# Using Hyperparameter Tuning and cross-validation
# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

data$target <- factor(data$target, levels = c(0,1))

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

# Define the hyperparameters to tune over
tune_grid <- expand.grid(
  fL = seq(0, 1, by = 0.1),
  usekernel = c(TRUE, FALSE),
  adjust = c(TRUE, FALSE)
)

cv <- trainControl(method = "cv", number = 10)

# Train the model using cross-validation and hyperparameter tuning
nb_model <- train(
  x = training[, -14],
  y = training$target,
  method = "nb",
  trControl = cv,
  tuneGrid = tune_grid
)

# Make predictions on the test set
prediction <- predict(nb_model, testing[, -14])

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

