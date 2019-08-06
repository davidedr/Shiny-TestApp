#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(maps)) install.packages('maps')
library(maps)

if (!require(mapproj)) install.packages('mapproj')
library(mapproj)

# One time execution
counties <- readRDS("data/counties.rds")
source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel('title', windowTitle = 'Window title'),
  sidebarLayout(sidebarPanel(
    helpText("Create demographic..."),
    selectInput("var", label = "Choose a variable to display", choices = c("Percent White", "Percent Black", "Percent Hispanic", "Percent Asian"), selected = "Percent White"),
    sliderInput("range", label = "Range of interest", min = 0, max = 100, value = c(25, 75))
  ),
                
  mainPanel(
    img(src = "rstudio.png", height = 140, width = 400),
    actionButton(inputId = "clicks", label = "Click me!"),
    sliderInput(inputId = 'num', label = "Choose a number", min = 0, max = 50, value = 25),
    textInput(inputId = 'title', label = 'Write a title', value = "Default title"),
    plotOutput("map")
  )
  
  ),
  
  textOutput("selected_var"),
  textOutput("selected_slider"),
  plotOutput(outputId = 'hist'),
  textOutput(outputId = 'numText'),
  textOutput(outputId = 'statsText'),
  verbatimTextOutput(outputId = 'statsVerbatimText')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$clicks, { print(as.numeric(input$clicks)) })
  data <- reactive( {rnorm(input$num)} ) 
  
  output$hist <- renderPlot({
    title <- "100 random normal values"
    hist(data(), main = input$title)
  }) # between { and }  the R code that generates the plot
  
  output$numText <- renderText( {input$num} )
  
  output$statsText <- renderText( {summary(data())} )
  
  output$statsVerbatimText <- renderPrint( {summary(data())} )
  
  output$selected_var <- renderText( {paste("You have selected: ", input$var)} )
  output$selected_slider <- renderText( {paste("You have selected from: ", input$range[1], " to: ", input$range[2])} )
  output$map <- renderPlot( {
    data <- switch(input$var, "Percent White" = counties$white, "Percent Black" = counties$black, "Percent Hispanic" = counties$hispanic, "Percent Asian" = counties$asian)
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")    
    legend <- switch(input$var, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    percent_map(data, color, legend, input$range[1], input$range[2])
  } ) 
   
}

# Run the application 
shinyApp(ui = ui, server = server)
