# Adding Relevant library
library(shiny)
library(htmlwidgets)
library(ggplot2)
library(caret)
library(e1071)
library(xgboost)
library(rsconnect)
library(testthat)


# Loading Data"
data <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")
data1 <- read.csv("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\www\\heart.csv", sep = ",")


# Setting target variables categorical
data1$target = factor(data1$target, levels = c(0,1), labels = c("False", "True"))

# Split data into 70% training Set and 30% testing Set
train_index <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training <- data[train_index,]
testing <- data[-train_index,]

# For accuracy
train_set <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
train <- data[train_set,]
test <- data[-train_set,]

train_acc <- createDataPartition(y = data$target, p = 0.7, list = FALSE)
training_acc <- data[train_acc,]
testing_acc <- data[-train_acc,]

# Building Support Vector Machine Model
model_svm <- svm(target ~., data = training, kernel = "linear", cost = 10)

# For accuracy
train[["target"]] = factor(train[["target"]])
model_svm_acc <- svm(target ~., data = train, kernel = "linear", cost = 10)

# Building Improved model of Support Vector machine
# define parameter grid
#param_grid <- expand.grid(C = c(0.1, 1, 10, 100),
 #                         gamma = c(0.01, 0.1, 1, 10))

# performing grid search
#tune_result <- tune(svm,
 #                   target ~.,
  #                  data = training,
   #                 kernel = "radial",
    #                ranges = param_grid,
     #               tunecontrol = tune.control(sampling = "cross", cross = 5))

# Best model 
#best_model_svm <- tune_result$best.model


# Building model XGBoost
model_xg <- xgboost(data = as.matrix(training[,-c(14)]), label = training$target,
                    objective = "binary:logistic", nrounds = 100)

# For Accuracy
model_xg_acc <- xgboost(data = as.matrix(training_acc[,-c(14)]), label = training_acc$target,
                        objective = "binary:logistic", nrounds = 100)

prediction_acc <- predict(model_xg_acc, as.matrix(testing_acc[,-c(14)]))
levels <- c("0", "1")
prediction_acc <- factor(ifelse(prediction_acc > 0.5, 1, 0), levels = levels)

# Building model linear regression
model_lm <- train(target ~., data = training, method = "lm", trControl = trainControl(method = 'cv', number = 10))


# Define server logic required to draw a histogram
function(input, output) {

  # Creating a reactive object to store input data
  input_data <- reactive({
    data.frame(
      age = input$age,
      sex = ifelse(input$sex == "Male", 1, 0),
      cp = switch(input$cp,
                  "Typical Angina" = 0,
                  "Atypical Angina" = 1,
                  "Non-Anginal pain" = 2,
                  "Asymptotic" = 3),
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = ifelse(input$fbs == "False", 0, 1),
      restecg = switch(input$restecg,
                       "Normal" = 0,
                       "ST-T wave abnormality" = 1,
                       "Left ventricular hyperthrophy" = 2),
      thalach = input$thalach,
      exang = ifelse(input$exang == "False", 0, 1),
      oldpeak = input$oldpeak,
      slope = switch(input$slope,
                     "upsloping" = 0,
                     "flat" = 1,
                     "downsloping" = 2),
      ca = input$ca,
      thal = switch(input$thal,
                    "No Defect" = 0,
                    "Normal Defect" = 1,
                    "Fixed Defect" = 2,
                    "Reversible Defect" = 3)
    )
  })
  
  # Predict health of the patient using old models
  output$prediction_text <- renderPrint({
    req(input$prediction_button)
    
    # Logic for SVM Model
    prediction_svm <- predict(model_svm, input_data())
    
    if(is.na(prediction_svm)){
      paste("Please input all values")
    }else{
      if(prediction_svm > 0.50){
        cat("SVM Model:",prediction_svm,"\nHigh Risk\nBased on the given details patient is at risk of heart disease\n\n")
      }else{
        cat("SVM Model:",prediction_svm,"\nLow Risk\nBased on the given details patient is not at risk of heart disease\n\n")
      }
    }
    
    # Logic for XGBoost
    prediction_xg <- predict(model_xg, as.matrix(input_data()))
    
    if(is.na(prediction_xg)){
      paste("Please input all values")
    }else{
      if(prediction_xg > 0.50){
        cat("XGBoost Model:",prediction_xg,"\nHigh Risk\nBased on the given details patient is at risk of heart disease\n\n")
      }else{
        cat("XGBoost Model:",prediction_xg,"\nLow Risk\nBased on the given details patient is not at risk of heart disease\n\n")
      }
    }
    
    # Logic for Linear Regression
    prediction_lm <- predict(model_lm, input_data(), kernel = "linear", cost = 10)
    
    if(is.na(prediction_lm)){
      paste("Please input all values")
    }else{
      if(prediction_lm > 0.50){
        cat("Linear Regression Model:",prediction_lm,"\nHigh Risk\nBased on the given details patient is at risk of heart disease\n\n")
      }else{
        cat("Linear Regression Model:",prediction_lm,"\nBased on the given details patient is not at risk of heart disease\n\n")
      }
    }
  })
  
  #output$best_prediction_text <- renderPrint({
   # req(input$prediction_button)
    
    # Logic for SVM Model
    #best_prediction_svm <- predict(best_model_svm, input_data())
    
    #if(is.na(best_prediction_svm)){
     # paste("Please input all values")
    #}else{
     # if(best_prediction_svm > 0.50){
      #  cat("SVM Model:",best_prediction_svm,"\nPatient is at risk of heart disease\n\n")
      #}else{
       # cat("SVM Model:",best_prediction_svm,"\nPatient is not at risk of heart disease\n\n")
      #}
    #}
  #})
  
#  output$svm_accuracy <- renderPrint({
    
#    predict_acc <- predict(model_svm_acc, newdata = test)
 #   print(confusionMatrix(table(predict_acc, test$target)))
  #})
  
#  output$xg_accuracy <- renderPrint({
    
    
 #   print(confusionMatrix(prediction_acc, testing_acc$target))
  #})
  
  
  # Tab 3
  output$age <- renderPrint({ input$age })
  
  output$sex <- renderPrint({ input$sex })
  
  output$cp <- renderPrint({ input$cp })
  
  output$trestbps <- renderPrint({ input$trestbps })
  
  output$chol <- renderPrint({ input$chol })
  
  output$fbs <- renderPrint({ input$fbs })
  
  output$restecg <- renderPrint({ input$restecg })
  
  output$thalach <- renderPrint({ input$thalach })
  
  output$exang <- renderPrint({ input$exang })
  
  output$oldpeak <- renderPrint({ input$oldpeak })
  
  output$slope <- renderPrint({ input$slope })
  
  output$ca <- renderPrint({ input$ca })
  
  output$thal <- renderPrint({ input$thal })
  
  
  # Tab 1 Home
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      data
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Age","Sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","target")
    ))
  
  
  # Tab 2 Analysis
  output$age_plot <- renderPlot({
    ggplot(data1, aes(age, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Age Distribution by Target")
  })
  
  output$sex_plot <- renderPlot({
    ggplot(data1, aes(sex, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Gender Distribution by Target")
  })
  
  output$cp_plot <- renderPlot({
    ggplot(data1, aes(cp, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Chest Pain Distribution by Target")
  })
  
  output$trestbps_plot <- renderPlot({
    ggplot(data1, aes(trestbps, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Resting Blood Pressure Distribution by Target")
  })
  
  output$chol_plot <- renderPlot({
    ggplot(data1, aes(chol, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Serum Cholestoral Distribution by Target")
  })
  
  output$fbs_plot <- renderPlot({
    ggplot(data1, aes(fbs, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Fasting Blood Sugar Distribution by Target")
  })
  
  output$restecg_plot <- renderPlot({
    ggplot(data1, aes(restecg, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Resting Electrocardiographic Results Distribution by Target")
  })
  
  output$thalach_plot <- renderPlot({
    ggplot(data1, aes(thalach, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Max. Heart Rate Achieved Distribution by Target")
  })
  
  output$exang_plot <- renderPlot({
    ggplot(data1, aes(exang, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Exercise Induced Angina Distribution by Target")
  })
  
  output$oldpeak_plot <- renderPlot({
    ggplot(data1, aes(oldpeak, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "St Depression Induced by Exercise Distribution by Target")
  })
  
  output$slope_plot <- renderPlot({
    ggplot(data1, aes(slope, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Exercise ST Segment Distribution by Target")
  })
  
  output$ca_plot <- renderPlot({
    ggplot(data1, aes(ca, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Number of Major Vessels Distribution by Target")
  })
  
  output$thal_plot <- renderPlot({
    ggplot(data1, aes(thal, colour = target)) +
      geom_freqpoly(binwidth = 1) +
      labs(title = "Display Thalassemia Distribution by Target")
  })

}




#testServer(server, {
 # session$
#})


