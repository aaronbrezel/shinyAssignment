#install.packages("reshape")
library(shiny)
library(EBMAforecast)
library(ggplot2)
data("presidentialForecast")
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Presidential Forecasts"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Include clarifying text ----
      helpText("Note: Here are the results from presidential forecasts from 1952-2008."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      selectInput(inputId = "forecaster",
                  label = "Choose a forecast:",
                  choices = colnames(presidentialForecast),
                  selected = colnames(presidentialForecast),
                  multiple = TRUE),
      
      sliderInput("range", "Chose years:", min = 1952, max = 2008, step = 4, value = c(1952, 2008), sep = "")
      

    ),
    
 
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("view"),
      plotOutput(outputId = "resultPlot", click = "plot_click"),
      verbatimTextOutput("info") 
    )
    
  )
)


# Define server logic to summarize and view selected dataset ----

server <- function(input, output) {
  library(EBMAforecast)
  data("presidentialForecast")
  library(reshape)
  presidentialForecast$year <- rownames(presidentialForecast)
  presidentialForecast$year <- as.numeric(presidentialForecast$year)

  
  output$view <- renderTable({
    presidentialForecast[which(presidentialForecast$year >= input$range[1] & presidentialForecast$year <= input$range[2]),c(input$forecaster, "year")]
  })
  output$resultPlot <- renderPlot({
    test <- presidentialForecast[which(presidentialForecast$year >= input$range[1] & presidentialForecast$year <= input$range[2]),c(input$forecaster, "year")]
    test <- melt(test, year = c("campbell", "Lewis-Beck", "EWT2C2", "Fair", "Hibbs", "Abramowitz", "Actual"))
    meltYear <- rownames(presidentialForecast)
    meltYear <- as.numeric(meltYear)
    test$year <- meltYear
    test <- test[!grepl("year",test$variable),]
    
    if(ncol(test) == 1){
      ggplot(presidentialForecast, aes(x= presidentialForecast$year, y= presidentialForecast$Actual)) + xlim(input$range[1], input$range[2]) + ylim(40,65)
    }
    
    else{
      ggplot(test, aes(x = test$year, y = test$value, colour = test$variable)) + geom_line() + xlim(input$range[1], input$range[2]) + ylim(40,65) + xlab("year") + ylab("Percentage share of vote") + ggtitle("Election results by year")  
    }
  })
  
  output$info <- renderText({
    paste0("year=", input$plot_click$x, "\nVote Share=", input$plot_click$y)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
