library(xgboost)
library(caret)

# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Old\\heart.csv", sep = ",")

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

model <- xgboost(data = as.matrix(training[,-c(14)]), label = training$target,
                 objective = "binary:logistic", nrounds = 100)

prediction <- predict(model, as.matrix(testing[,-c(14)]))

# Convert these to categorical variables for factorize them
data$target = factor(data$target, levels = c(0,1), labels = c("False", "True"))

training[["target"]] = factor(training[["target"]])


confusionMatrix(prediction, testing$target)

new_data <- data.frame(age = 58, 
                       sex = 0, 
                       cp = 0, 
                       trestbps = 100, 
                       chol = 248, 
                       fbs = 0, 
                       restecg = 0, 
                       thalach = 122, 
                       exang = 0, 
                       oldpeak = 1.0, 
                       slope = 1, 
                       ca = 0, 
                       thal = 2)

new_prediction <- predict(model, as.matrix(new_data))


if (new_prediction > 0.50) {
  print("True")
  cat(new_prediction)
} else {
  print("False")
  cat(new_prediction)
}
