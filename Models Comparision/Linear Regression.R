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





















# Add Patient's Details
#new_data <- data.frame(age = 58, 
 #                      sex = 0, 
  #                     cp = 0, 
   #                    trestbps = 100, 
    #                   chol = 248, 
     #                  fbs = 0, 
      #                 restecg = 0, 
       #                thalach = 122, 
        #               exang = 0, 
         #              oldpeak = 1.0, 
          #             slope = 1, 
           #            ca = 0, 
            #           thal = 2)

#new_prediction <- predict(model, new_data)

#if (new_prediction > 0.50) {
 # print("True")
  #cat(new_prediction)
#} else {
 # print("False")
  #cat(new_prediction)
#}