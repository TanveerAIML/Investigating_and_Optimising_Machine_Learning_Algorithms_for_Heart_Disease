# Adding Relevant Library
library(shiny)


# Define UI for application that draws a histogram
fluidPage(

  # Application title
  titlePanel(h1(strong(span("Heart", style = "color:red"), "Disease Prediction:"), span("Know your Disease Save your Life"), align = "center")),
  
  # Navbar
  navbarPage("Let's get Started",
             tabPanel(icon("home"),
                      
                      fluidRow(column(tags$img(src = "UK map.jpg", width="260px", height="260px"), width=2),
                               column(
                                 br(),
                                 p("Heart disease is the leading cause of death for men, women, and people of most racial and ethnic groups in the United Kingdom. 
                                              On the basis of statistics, more than 160,000 deaths occur each year with an average of 460 deaths each day or one every three minutes 
                                              in the UK", 
                                   a(href="https://www.bhf.org.uk/what-we-do/news-from-the-bhf/contact-the-press-office/facts-and-figures", strong("(British Heart Foundation, 2023)")),". 
                                              Heart disease refers to many conditions like coronary heart disease, heart arrhythmias, cerebrovascular disease, and other conditions. 
                                              In the UK, hospitals are receiving more than 100,000 admissions each year due to heart attack.", style = "text-align:justify; color:black; background-color:lavender; padding:15px; border-radius:10px"),
                                 br(),
                                 br(),
                                 p("The data used in this application are publicly available on the page of the", a(href="https://www.kaggle.com/datasets/johnsmith88/heart-disease-dataset", strong("Kaggle")),
                                   ". Kaggle is a community for data scientist and developers. People who are interested in machine learning or other kinds of modern development can join the community of over 
                                              1 million registered users and talk about development models, explore data sets, or network across 194 seperate countries around the world.", style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                 
                                 width=7),
                               column(tags$img(src = "Kaggle.jpg", width="400px", height="200px"),
                                      
                                      width=2)
                               
                      ),
                      hr(),
                      # Data Set Symbol
                      tags$style(".fa-database {color:#E87722}"),
                      h3(p(em("Dataset "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                      
                      # Calling Data
                      fluidRow(column(DT::dataTableOutput("RawData"),
                                      width = 12)),
                      
                      hr(),
                      
                      # Developed By 
                      p(em("Developed by"),br("Tanveer Dalal"),style="text-align:center; font-family: times")
                      
                      ),
             
             tabPanel("Analysis",
                      
                      
                      
                      fluidRow(column(4,
                              
                                      plotOutput("age_plot")  
                              ),
                              
                              column(4,
                                     
                                     plotOutput("sex_plot")
                                
                              ),
                              
                              column(4,
                                     
                                     plotOutput("fbs_plot")
                                     
                              ),
                      
                      ),
                      
                      fluidRow(column(8,
                                      
                                      plotOutput("trestbps_plot")
                                      
                              ),
                              
                              column(4,
                                     
                                     plotOutput("restecg_plot")
                                     
                              ),
                      ),
                      
                      fluidRow(column(12,
                                      
                                      plotOutput("chol_plot")
                                      
                              ),
                        
                      ),
                      
                      fluidRow(column(12,
                                      
                                      plotOutput("thalach_plot")
                                      
                              ),
                        
                      ),
                      
                      fluidRow(column(4,
                                      
                                      plotOutput("exang_plot")
                                      
                              ),
                              
                              column(4,
                                     
                                     plotOutput("slope_plot")
                                     
                              ),
                              
                              column(4,
                                     
                                     plotOutput("thal_plot")
                                     
                              ),
                        
                      ),
                      
                      fluidRow(column(4,
                                      
                                      plotOutput("oldpeak_plot")
                                      
                              ),
                              
                              column(4,
                                     
                                     plotOutput("cp_plot")
                                     
                              ),
                              
                              column(4,
                                     
                                     plotOutput("ca_plot")
                                     
                              ),
                        
                      )
                      
                      
             ),
             
             tabPanel("Prediction",
                      
                      fluidRow(
                        
                        column(2,
                               
                               # Add Patient's Age
                               sliderInput("age", label = h3("Age"), min = 29, max = 77, value = 40)
                               
                        ),
                        
                        column(2,
                               
                               # Add Patient's Gender
                               radioButtons("sex", label = h3("Gender"),
                                            choices = list("Female", "Male"))
                        ),
                        
                        column(2,
                               
                               # Add Patient's Chest Pain Type
                               selectInput("cp", label = h3("Chest Pain Type"),
                                           choices = list("Typical Angina", "Atypical Angina", "Non-Anginal pain", "Asymptotic"))
                        ),
                        
                        column(2,
                               
                               # Add Patient's Resting Blood Pressure
                               sliderInput("trestbps", label = h3("Resting Blood Pressure"),
                                           min = 94, max = 200, value = 110)
                        ),
                        
                        column(2,
                               
                               # Add Patient's Cholestoral
                               sliderInput("chol", label = h3("Serum Cholestoral"),
                                           min = 126, max = 564, value = 200)
                        ),
                        
                        column(2,
                               
                               # Add Patient's Fasting Blood Sugar
                               radioButtons("fbs", label = h3("Fasting Blood Sugar"),
                                            choices = list("False", "True"))
                        )
                      ),
                      
                      fluidRow(
                        
                        column(2,
                          
                               # Add Patient's Age
                               verbatimTextOutput("age")
                        ),
                        
                        column(2,
                               
                               # Add Patient's sex
                               verbatimTextOutput("sex")
                        ),
                        
                        column(2,
                               
                               # Add Patient's Chest Pain
                               verbatimTextOutput("cp")
                        ),
                        
                        column(2,
                               
                               # Add Patient's trestbps
                               verbatimTextOutput("trestbps")
                        ),
                        
                        column(2,
                               
                               # Add Patient's chol
                               verbatimTextOutput("chol")
                        ),
                        
                        column(2,
                               
                               # Add Patient's fbs
                               verbatimTextOutput("fbs")
                        )
                      ),
                      
                      fluidRow(
                        
                        column(2,
                               
                               # Patient's Resting Electrocardiographic Results
                               selectInput("restecg", label = h3("Resting Electrocardiographic Results"),
                                           choices = list("Normal", "ST-T wave abnormality", "Left ventricular hyperthrophy"))
                        ),
                        
                        column(2,
                               
                               # Patient's Maximum Heart Rate Achieved
                               sliderInput("thalach", label = h3("Max. Heart Rate Achieved"),
                                           min = 71, max = 202, value = 100)
                        ),
                        
                        column(2,
                               
                               # Patient's Exercise Induced Angina
                               radioButtons("exang", label = h3("Exercise Induced Angina"),
                                            choices = list("False", "True"))
                        ),
                        
                        column(2,
                               
                               # ST depression induced by exercise relative to rest
                               numericInput("oldpeak", label = h3("St Depression Induced by Exercise:"), value = 0, min = 0, max = 6.2, step = 0.1)
                        ),
                        
                        column(2,
                               
                               # Slope of the Peak
                               selectInput("slope", label = h3("Exercise ST Segment (Slope)"),
                                           choices = list("upsloping", "flat", "downsloping"))
                        ),
                        
                        column(2,
                               
                               # Number of major vessels
                               numericInput("ca", label = h3("Number of Major Vessels"), value = 0, min = 0, max = 4))
                      ),
                      
                      
                      fluidRow(
                        
                        column(2,
                               
                               # Add Patient's restecg
                               verbatimTextOutput("restecg")
                        ),
                        
                        column(2,
                               
                               # Add Patient's thal
                               verbatimTextOutput("thalach")
                        ),
                        
                        column(2,
                               
                               # Add Patient's exang
                               verbatimTextOutput("exang")
                        ),
                        
                        column(2,
                               
                               # Add Patient's oldpeak
                               verbatimTextOutput("oldpeak")
                        ),
                        
                        column(2,
                               
                               # Add Patient's slope
                               verbatimTextOutput("slope")
                        ),
                        
                        column(2,
                               
                               # Add Patient's ca
                               verbatimTextOutput("ca")
                        )
                      ),
                      
                      
                      
                      fluidRow(
                        
                        column(2,
                               
                               # Patient's Thal
                               selectInput("thal", label = h3("Display Thalassemia"),
                                           choices = list("No Defect", "Normal Defect", "Fixed Defect", "Reversible Defect")),
                               br(),
                               # Add patient's thal
                               verbatimTextOutput("thal")       
                               
                        ),
                        
                        column(2,
                          
                               h3(strong("Check Result:")),
                               br(),
                               actionButton("prediction_button", "Predict")
                               
                        ),
                        
                        column(4,
                               
                               verbatimTextOutput("prediction_text")
                        ),
                        
                        column(4,
                        
                               verbatimTextOutput("best_prediction_text")       
                        )
                        
                      ),
                      hr(),
                      
                fluidRow(column(3,
                              
                                h3("SVM Accuracy:-"),
                                verbatimTextOutput("svm_accuracy")
                                              
                        ),
                        
                        column(3,
                        
                               h3("XGBoost Accuracy:-"),
                               verbatimTextOutput("xg_accuracy")
                        ),
                        
                        column(3,
                        
                               h3("Linear Regression:-"),
                               verbatimTextOutput("lm_accuracy")
                        )
                        
                )
                      
                      
                  )
             
             )
    
)
