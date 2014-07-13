# shinyServer(
#   function(input, output) {
#     output$text1  <- renderText({input$text1})
#     output$text2  <- renderText({input$text2})
#     output$text3  <- renderText({
#       input$goButton
#       isolate(paste(input$text1, input$text2))
#     })
#   })


shinyServer(
  function(input, output) {
    output$text1  <- renderText({input$text1})
    output$text2  <- renderText({input$text2})
    output$text3  <- renderText({
      if (input$goButton == 0) "You have not pressed the button"
      else if (input$goButton == 1) "You pressed it once"
      else "OK Quit pressing it."
    })
  })