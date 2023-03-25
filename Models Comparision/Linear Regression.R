# Read Data
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Old\\heart.csv", sep = ",")

# checking for outliers
boxplot(data)

library(caret)
set.seed(123)

# Split data into 70% training and 30% test data sets
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]


library(e1071)
model <- train(target ~., data = training, method = 'lm',trControl = trainControl(method = 'cv', number = 10))


prediction <- predict(model, testing[,-14], kernel = "linear", cost = 10)
prediction

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

new_prediction <- predict(model, new_data)

if (new_prediction > 0.50) {
  print("True")
  cat(new_prediction)
} else {
  print("False")
  cat(new_prediction)
}
