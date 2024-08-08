library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Sales Performance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      dateRangeInput("dateRange", "Select Date Range"),
      selectInput("category", "Select Category", choices = NULL),
      actionButton("update", "Update"),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Prediction", verbatimTextOutput("prediction"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded data
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df$Date <- as.Date(df$Date)  # Ensure Date is in Date format
    df
  })
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    req(data())
    df <- data()
    
    if (!is.null(input$dateRange)) {
      df <- df[df$Date >= as.Date(input$dateRange[1]) & df$Date <= as.Date(input$dateRange[2]), ]
    }
    
    if (input$category != "") {
      df <- df[df$Category == input$category, ]
    }
    
    df
  })
  
  # Update category choices based on the uploaded data
  observe({
    req(data())
    updateSelectInput(session, "category", choices = c("", unique(data()$Category)))
  })
  
  # Summary statistics
  output$summary <- renderPrint({
    req(filteredData())
    if(nrow(filteredData()) > 0) {
      summary(filteredData())
    } else {
      cat("No data available for the selected filters.")
    }
  })
  
  # Plot
  output$plot <- renderPlot({
    req(filteredData())
    if(nrow(filteredData()) > 0) {
      ggplot(filteredData(), aes(x = Date, y = Sales)) +
        geom_line() +
        facet_wrap(~Category) +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.2)
    }
  })
  
  # Simple linear regression model for prediction
  output$prediction <- renderPrint({
    req(filteredData())
    
    if(nrow(filteredData()) > 0) {
      model <- lm(Sales ~ Date, data = filteredData())
      prediction <- predict(model, newdata = filteredData())
      
      cat("Linear Regression Model Summary:\n")
      print(summary(model))
      cat("\nPredicted Sales:\n")
      print(prediction)
    } else {
      cat("No data available for the selected filters.")
    }
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() { "filtered_data.csv" },
    content = function(file) {
      req(filteredData())
      write.csv(filteredData(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
