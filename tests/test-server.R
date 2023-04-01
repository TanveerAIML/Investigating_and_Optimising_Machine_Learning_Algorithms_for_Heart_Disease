library(shiny)
library(testthat)

source("C:\\Work Profession\\University Files\\Year 3\\Final Year Project\\Heart-Disease-Prediction\\server.R")

test_that("output$prediction_text updates correctly", {
  testServer(server, {
    # Set input values
    session$setInputs(age = 45)
    session$setInputs(sex = "Female")
    session$setInputs(cp = "Atypical Angina")
    session$setInputs(trestbps = 130)
    session$setInputs(chol = 250)
    session$setInputs(fbs = "False")
    session$setInputs(restecg = "ST-T wave abnormality")
    session$setInputs(thalach = 160)
    session$setInputs(exang = "False")
    session$setInputs(oldpeak = 1.5)
    session$setInputs(slope = "downsloping")
    session$setInputs(ca = 0)
    session$setInputs(thal = "Fixed Defect")
    
    # Trigger the prediction button
    session$setInputs(prediction_button = T)
    
    # Check the output$prediction_text value
    output <- session$getOutputs("prediction_text")
    
    # Define the expected output as a string
    expected_output <- paste(
      "SVM Model: <svm_prediction> \nPatient is not at risk of heart disease\n\n",
      "XGBoost Model: <xgboost_prediction> \nPatient is not at risk of heart disease\n\n",
      "Linear Regression Model: <lm_prediction> \nPatient is not at risk of heart disease\n\n",
      sep = ""
    )
    
    # Replace <svm_prediction>, <xgboost_prediction>, and <lm_prediction> with the expected predictions from the respective models
    expected_output <- gsub("<svm_prediction>", "<expected_svm_prediction>", expected_output)
    expected_output <- gsub("<xgboost_prediction>", "<expected_xgboost_prediction>", expected_output)
    expected_output <- gsub("<lm_prediction>", "<expected_lm_prediction>", expected_output)
    
    # Test the output value
    expect_equal(output$prediction_text, expected_output)
  })
})
