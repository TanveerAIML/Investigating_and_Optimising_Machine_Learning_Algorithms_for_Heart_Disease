# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Old\\heart.csv", sep = ",")

# checking for outliers
#boxplot(data)

library(caret)
set.seed(123)

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

library(e1071)

model <- svm(target ~., data = training, kernel = "linear", cost = 10)

old_prediction <- predict(model, testing)
old_prediction


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

best_model <- tune_result$best.model

new_data <- data.frame(age = 58, 
                       sex = 0, 
                       cp = 0, 
                       trestbps = 100, 
                       chol = 248, 
                       fbs = 0, 
                       restecg = 0, 
                       thalach = 122, 
                       exang = 0, 
                       oldpeak = 4.0, 
                       slope = 1, 
                       ca = 0, 
                       thal = 2)

new_prediction <- predict(best_model, new_data)

if (new_prediction > 0.50) {
  print("True")
  cat(new_prediction)
} else {
  print("False")
  cat(new_prediction)
}

# Checking Accuracy
training[["target"]] = factor(training[["target"]])

model <- svm(target ~., data = training, kernel = "linear", cost = 10)

prediction <- predict(model, newdata = testing)

confusionMatrix(table(prediction, testing$target))
