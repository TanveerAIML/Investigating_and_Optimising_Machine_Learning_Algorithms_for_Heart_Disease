library(caret)
library(e1071)

# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

model <- lm(target ~., data = training)

prediction <- predict(model, testing)

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






# Using Cross Validation
# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

# Create cross-validation folds
folds <- createFolds(y = training$target, k = 5, list = TRUE, returnTrain = TRUE)

# Train linear regression model using cross-validation
model <- train(target ~ ., data = training, method = "lm",
               trControl = trainControl(method = "cv", number = 10, index = folds))

# Make predictions on test data
prediction <- predict(model, testing)

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
