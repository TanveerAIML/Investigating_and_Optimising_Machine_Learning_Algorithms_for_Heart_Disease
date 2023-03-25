library(caret)
library(e1071)

# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")


# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]


# Confusion Matrix
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
# Confusion Matrix
training[["target"]] = factor(training[["target"]])

model <- tune_result$best.model

testing[["target"]] <- factor(testing[["target"]])
levels(testing$target) <- levels(training$target)
prediction <- predict(model, newdata = testing)

confusionMatrix(prediction, testing$target)





# Adding Patient's Details

#new_data <- data.frame(age = 58, 
#                      sex = 0, 
#                     cp = 0, 
#                    trestbps = 100, 
#                   chol = 248, 
#                  fbs = 0, 
#                 restecg = 0, 
#                thalach = 122, 
#               exang = 0, 
#              oldpeak = 4.0, 
#             slope = 1, 
#            ca = 0, 
#           thal = 2)

#prediction <- predict(model, new_data)

#if (new_prediction > 0.50) {
# print("True")
#cat(new_prediction)
#} else {
# print("False")
#cat(new_prediction)
#}