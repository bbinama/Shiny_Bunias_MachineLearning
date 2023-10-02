############################################
# Code edit from: http://github.com/dataprofessor          #
 
############################################

# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")
# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
TrainSet$population <- factor(TrainSet$population, levels = c("01AL", "02T3", "03T4"," 04RO","05LT","06CB","07JE","08WU","09DR","10DI","11GO","12PA"))
TrainSet$fertilisation <- factor(TrainSet$fertilisation, levels = c("high", "low"))
TrainSet$status <- factor(TrainSet$status, levels = c("native", "invasive","naturalised"))
str(TrainSet)
####################################
# User interface                   #
####################################

ui <- fluidPage(
  
  # Page header
  headerPanel('Bunias Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    selectInput("population", label = "Population", choices = unique(TrainSet$population)),
    selectInput("fertilisation", label = "Fertilisation", choices = unique(TrainSet$fertilisation)),
    sliderInput("nLeaves1", label = "Number of Leaves", value = 12,
                min = min(TrainSet$nLeaves1),
                max = max(TrainSet$nLeaves1)),
    sliderInput("LongestLeaf1", label = "Longest Leaf", value = 13.9,
                min = min(TrainSet$LongestLeaf1),
                max = max(TrainSet$LongestLeaf1)),
    sliderInput("Glucosinolates1", label = "Glucosinolates", value = 5.084,
                min = min(TrainSet$Glucosinolates1),
                max = max(TrainSet$Glucosinolates1)),
    sliderInput("HsGlucosinolates1", label = "HsGlucosinolates", value = 0.356,
                min = min(TrainSet$HsGlucosinolates1),
                max = max(TrainSet$HsGlucosinolates1)),
    sliderInput("nPathogenSpots1", label = "Number of Pathogen Spots", value = 2.0,
                min = min(TrainSet$nPathogenSpots1),
                max = max(TrainSet$nPathogenSpots1)),
    sliderInput("nHerbivores1", label = "Number of Herbivores", value = 11,
                min = min(TrainSet$nHerbivores1),
                max = max(TrainSet$nHerbivores1)),
    sliderInput("HsHerbivores1", label = "HsHerbivores", value = 0.966,
                min = min(TrainSet$HsHerbivores1),
                max = max(TrainSet$HsHerbivores1)), 
    sliderInput("LeafHerbivory1Cons", label = "Leaf Herbivory Consumed", value = 0,
               min = min(TrainSet$LeafHerbivory1Cons),
              max = max(TrainSet$LeafHerbivory1Cons)),
    sliderInput("nPredators1", label = "Number of Predators", value = 2.0,
                min = min(TrainSet$nPredators1),
                max = max(TrainSet$nPredators1)),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
  )
)

####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  # Reactive function to process input and make predictions
  datasetInput <- reactive({
    
    # Create a list with input values
    input_data <- list(
      population = factor(input$population, levels = levels(TrainSet$population)),
      fertilisation = factor(input$fertilisation, levels = levels(TrainSet$fertilisation)),
      nLeaves1 = as.integer(input$nLeaves1),
      LongestLeaf1 = as.numeric(input$LongestLeaf1),
      Glucosinolates1 = as.numeric(input$Glucosinolates1),
      HsGlucosinolates1 = as.numeric(input$HsGlucosinolates1),
      nPathogenSpots1 = as.integer(input$nPathogenSpots1),
      nHerbivores1 = as.integer(input$nHerbivores1),
      HsHerbivores1 = as.numeric(input$HsHerbivores1),
      LeafHerbivory1Cons = as.integer(input$LeafHerbivory1Cons),
      nPredators1 = as.integer(input$nPredators1)
    ) 
    
    # Convert the list to a data frame
    input_data <- as.data.frame(input_data)
    
    # Write input data to a CSV file (optional)
    write.csv(input_data, "input.csv", row.names = FALSE)
    
    # Make predictions using the model and input data
    prediction <- predict(model, newdata = input_data)
    probabilities <- predict(model, newdata = input_data, type = "prob")
    
    # Combine input data and predictions
    output_data <- data.frame(
      Prediction = prediction,
      native = probabilities[, "native"],
      invasive = probabilities[, "invasive"],
      naturalised = probabilities[, "naturalised"]
    )
    
    return(output_data)
  })
  
  # Display prediction results
  output$tabledata <- renderTable({
    req(input$submitbutton)  # Wait until the "Submit" button is clicked
    datasetInput()
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)