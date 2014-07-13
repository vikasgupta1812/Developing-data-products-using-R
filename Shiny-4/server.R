library(shiny)
diabetesRisk  <- function(gulucose) gulucose / 200
shinyServer(
  function(input, output) { 
    output$inputValue = renderPrint({input$gulucose})
    output$prediction = renderPrint({diabetesRisk(input$gulucose)})
  }
)