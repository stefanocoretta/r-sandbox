library(shiny)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Illustrating mixed effects models"),
  
  # Sidebar with various controls
  sidebarLayout(
    sidebarPanel(
      h4("Data"), br(),
      sliderInput("group.no", "Number of groups:", min = 1, max = 100, value = 1, step=1),
      sliderInput("measurement.no", "Number of measurements:", min = 1, max = 100, value = 50, step=1),
      checkboxInput("longitudinal",
                    "Longitudinal data",
                    value = F),
      checkboxInput("adult",
                    "Adults",
                    value = F),
      br(), 
      h4("Model"), br(),
      checkboxInput("randintr",
                    "Random intercepts",
                     value = F),
      checkboxInput("randslope",
                    "Random slopes",
                    value = F),
      checkboxInput("showmodel",
                    "Show model predictions",
                    value = F),
      checkboxInput("showrandom",
                    "Show random effects",
                    value = F),
      br(), 
      numericInput("iterations", "Number of iterations:", 
                   value=1,
                   min=1,
                   step=1),
      actionButton("run", "Run")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("dataPlot"), br(),
      h4(textOutput("summary")), br()
      #verbatimTextOutput("modelSummary")
    )
  )
))