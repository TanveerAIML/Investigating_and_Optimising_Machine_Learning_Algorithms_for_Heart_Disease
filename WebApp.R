#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(
      h1(strong(span("Heart", style = "color:red"), "Disease Prediction"), align = "center")
    ),
     titlePanel(
       h3("Know your Disease", align = "center")
     ),
      titlePanel(
        h3("Save your Life", align = "center")
      )
   

    # Sidebar with a slider input for number of bins 
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
