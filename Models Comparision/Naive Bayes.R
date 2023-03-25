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
