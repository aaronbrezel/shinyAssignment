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
      plotOutput(outputId = "resultPlot", click = "plot_click")
      
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
    if(ncol(test) == 1){
      ggplot(presidentialForecast, aes(x= presidentialForecast$year, y= presidentialForecast$Actual)) + xlim(input$range[1], input$range[2]) + ylim(0,70)
    }
    
    #plotVector <- c("ggplot(data = test, aes(x = test$year, y = test[1]))")
    #for(i in 2:ncol(test)-1){
    #  plotVector <- c(plotVector, "+ goem_line( ")
    #}
    #plotVector <- c(plotVector, '+ xlab("year") + ylab("Percentage share of vote") + ggtitle("Election results by year")')
    #temp = parse(text = paste(plotVector, sep = " "))
    #eval(temp)
    else{
      ggplot(data = test, aes(x = test$year, y = test$Actual)) + geom_line() + xlab("year") + ylab("Percentage share of vote") + ggtitle("Election results by year")  
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
str(presidentialForecast)

#I want to be able to insert this blank plot into the problem set. But for some reason it isn;t working
ggplot(presidentialForecast, aes(x= presidentialForecast$year, y= presidentialForecast$Actual)) + geom_line(aes(x = presidentialForecast$year, y= presidentialForecast$Fair)) + xlim(1952, 2008) + ylim(0,70)
presidentialForecast$test

?geom_line

a = 1 
b = 2 
c = parse(text = "log(a + b)") 
c
?paste
eval(c) 
?geom_line
colnam


test <- melt(presidentialForecast, year = c("campbell", "Lewis-Beck", "EWT2C2", "Fair", "Hibbs", "Abramowitz", "Actual"))
meltYear <- rownames(presidentialForecast)
meltYear <- as.numeric(meltYear)
test$year <- meltYear
test <- test[!grepl("year",test$variable),]
ggplot(test, aes(x = test$year, y = test$value, colour = test$variable)) + geom_line()

ChickWeight

colnames(test)
test
ggplot(presidentialForecast, aes(x = presidentialForecast$year, y =))
