library(caret)
library(e1071)

# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

training[["target"]] = factor(training[["target"]])

model <- svm(target ~., data = training, kernel = "linear", cost = 10)

prediction <- predict(model, newdata = testing)

conf_mat <- confusionMatrix(table(prediction, testing$target))
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

# define parameter grid
param_grid <- expand.grid(C = c(0.1, 1, 10, 100),
                          gamma = c(0.01, 0.1, 1, 10))

# performing grid search
tune_result <- tune(svm,
                    target ~.,
                    data = training,
                    kernel = "radial",
                    ranges = param_grid,
                    tunecontrol = tune.control(sampling = "cross",
                                               cross = 5))

model <- tune_result$best.model

prediction <- predict(model, newdata = testing)

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




# Using Cross-Validation 
# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

training[["target"]] = factor(training[["target"]])

# Create valid variable names for factor levels in target
#levels(training$target) <- make.names(levels(training$target))

# Train SVM model using linear kernel and 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)
svm_model <- train(target ~ ., data = training, method = "svmLinear", trControl = ctrl, tuneLength = 5)

# Make predictions on testing set
predictions <- predict(svm_model, newdata = testing)

testing$target <- factor(testing$target, levels = c("0", "1"))

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