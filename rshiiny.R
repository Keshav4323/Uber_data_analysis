library(shiny)
library(randomForest)
library(dplyr)

# Create the dataset
data <- data.frame(
  start_date = c("01-01-2016 21:11", "01-02-2016 01:25", "01-02-2016 20:25", "01-05-2016 17:31", "01-06-2016 14:42", "01-06-2016 17:15", "01-06-2016 17:30", "01-07-2016 13:27", "01-10-2016 08:05", "01-10-2016 12:17"),
  end_date = c("01-01-2016 21:17", "01-02-2016 01:37", "01-02-2016 20:38", "01-05-2016 17:45", "01-06-2016 15:49", "01-06-2016 17:19", "01-06-2016 17:35", "01-07-2016 13:33", "01-10-2016 08:25", "01-10-2016 12:44"),
  category = c("Business", "Business", "Business", "Business", "Business", "Business", "Business", "Business", "Business", "Business"),
  start = c("Fort Pierce", "Fort Pierce", "Fort Pierce", "Fort Pierce", "Fort Pierce", "West Palm Beach", "West Palm Beach", "Cary", "Cary", "Jamaica"),
  stop = c("Fort Pierce", "Fort Pierce", "Fort Pierce", "Fort Pierce", "West Palm Beach", "West Palm Beach", "Palm Beach", "Cary", "Morrisville", "New York"),
  miles = c(5.1, 5.0, 4.8, 4.7, 63.7, 4.3, 7.1, 0.8, 8.3, 16.5),
  purpose = c("Meal/Entertain", "Meal/Entertain", "Errand/Supplies", "Meeting", "Customer Visit", "Meal/Entertain", "Meeting", "Meeting", "Meeting", "Customer Visit")
)

# Define UI
ui <- fluidPage(
  titlePanel("Uber Dataset Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature1", "Select Feature 1", choices = names(data)),
      selectInput("feature2", "Select Feature 2", choices = names(data)),
      actionButton("runBtn", "Run Analysis")
    ),
    mainPanel(
      h4("Analysis Results"),
      tableOutput("durationTable"),
      br(),
      verbatimTextOutput("rmseOutput")
    )
  ),
  # Apply some basic CSS styling
  # Apply some basic CSS styling
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        background-color: grey;
        color: white;
      }
      .container-fluid {
        max-width: 1200px;
        margin: 20px auto;
      }
      .sidebar {
        background-color: black;
        color: white;
        padding: 20px;
        size:10px
      }
      .main-panel {
        background-color: #fff;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      table {
        border-collapse: collapse;
        width: 200px;
      height: 200px;
      }
      th, td {
        padding: 8px;
        text-align: left;
        border-bottom: 1px solid Black;
      }
      th {
        background-color: black;
        color: white;
      }
      td {
        background-color: black;
        color:white
      }
      .output {
        font-size: 18px;
        color: black;
      }
    "))
  )
)

# Define server
server <- function(input, output) {
  analysis_data <- reactive({
    if (all(c(input$feature1, input$feature2) %in% names(data))) {
      features <- c(input$feature1, input$feature2, "start_date", "end_date", "miles")
      analysis_data <- data %>% select(all_of(features))
      
      analysis_data$start_date <- as.POSIXct(analysis_data$start_date, format = "%m-%d-%Y %H:%M")
      analysis_data$end_date <- as.POSIXct(analysis_data$end_date, format = "%m-%d-%Y %H:%M")
      
      analysis_data$duration <- as.numeric(difftime(analysis_data$end_date, analysis_data$start_date, units = "mins"))
      
      return(analysis_data)
    } else {
      return(NULL)
    }
  })
  
  observeEvent(input$runBtn, {
    req(analysis_data())
    
    set.seed(123)
    trainIndex <- sample(1:nrow(analysis_data()), 0.8 * nrow(analysis_data()))
    train_data <- analysis_data()[trainIndex, -which(names(analysis_data()) == "miles")]
    test_data <- analysis_data()[-trainIndex, -which(names(analysis_data()) == "miles")]
    train_label <- analysis_data()[trainIndex, "miles"]
    test_label <- analysis_data()[-trainIndex, "miles"]
    
    model <- randomForest(x = train_data, y = train_label)
    
    predictions <- predict(model, newdata = test_data)
    accuracy <- sqrt(mean((predictions - test_label)^2))
    
    output$durationTable <- renderTable({
      analysis_data()
    })
    
    output$rmseOutput <- renderText({
      paste("Root Mean Squared Error:", accuracy)
    })
  })
}

# Run the application
shinyApp(ui, server)
