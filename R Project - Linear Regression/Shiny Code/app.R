# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(caret)

# set working directory
tips_predict_model <- readRDS("./Model_tips.rds")

# Default values
max_size <- 6
lambda <- 0.1414141

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Tips Prediction - Kaggle", windowTitle = "Tips Prediction - Kaggle"),
    
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Open+Sans&display=swap');
      h1 {
        font-family: 'Yusei Magic', sans-serif;
        text-align: center;
        font-size: 50px
      }
      h3 {
        font-family: 'Yusei Magic', sans-serif;
        text-align: center;
        font-size: 30px            
      }")),
    
    # Link to dataset
    tags$a(href = "https://www.kaggle.com/code/sanjanabasu/tips-dataset/data", "Click the link to download the dataset"),
    hr(),
    
    sidebarLayout(position = "right",
                  mainPanel(
                      # Output fields
                      br(),
                      h3("Predicted Tip"),
                      br(),
                      h1(textOutput("Predict"))
                  ),
                  sidebarPanel(
                      # Input fields
                      numericInput("totalbill", "What's your total bill?", value = 15, min = 3, max = 51),
                      # max and min set so that theres no extrapolation.
                      sliderInput("size", "Number of members", value = 1, min = 1, max = max_size),
                      
                      actionButton("Submit", "Predict", class = "btn-success"),
                  )
    ),
    hr()
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    isValidNum <- reactive({
        print("Inside")
        !is.null(input$totalbill) && input$totalbill >= 3 && input$totalbill <= 51
    })
    
    observeEvent(input$totalbill, {
            if(is.na(isValidNum())) {
                output$Predict <- renderText({
                    paste("Error : Please enter total bill values between $3 and $51")
                })
            }
            else if(isValidNum()) {
                output$Predict <- renderText({
                    paste("")
                })
            }
            else {
                output$Predict <- renderText({
                    paste("Error : Please enter total bill values between $3 and $51")
                })
                # Extrapolation
            }
    })

    observeEvent(input$Submit, {
        if(is.na(isValidNum())) {
            output$Predict <- renderText({
                paste("Error : Please enter total bill values between $3 and $51")
            })
        }
        else if(isValidNum()) {
            total_bill <- as.numeric(input$totalbill)
            size <- as.numeric(input$size)
            
            pred <- predict(tips_predict_model, data.frame(total_bill = input$totalbill , size = input$size))
            pred <- round((lambda * pred + 1)^(1/lambda), digits = 2)
            
            output$Predict <- renderText({
                paste("$", pred)
            })
        }
        else {
            output$Predict <- renderText({
                paste("Error : Please enter total bill values between $3 and $51")
            })
            # Extrapolation
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)