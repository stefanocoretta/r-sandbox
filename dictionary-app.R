library(shiny)
library(shinyWidgets)

words <- list("a" = c("abba", "abrut", "arsa"), "b" = c(4, 5, 6), "c" = c("abba", "abrut", "arsa"), "a" = c("abba", "abrut", "arsa"), "d" = c("abba", "abrut", "arsa"), "r" = c("abba", "abrut", "arsa"), "a" = c("abba", "abrut", "arsa"))

wordis <- rep(c("abba", "abrut", "bernat", "bores"), each = 20)
lolo <- split(wordis, substr(wordis, 1, 1))

ui <- fluidPage(
  pickerInput('picker1', 'picker 1', lolo, options=pickerOptions(liveSearch=T)),
  textOutput("result")
)

server <- function(input, output, session) {
  output$result <- renderText({
    paste("You chose", input$picker1)
  })
}

shinyApp(ui, server)
