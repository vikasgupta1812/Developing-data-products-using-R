# Illustrating Inputs 
shinyUI(pageWithSidebar(
  headerPanel("Illustrating inputs"),
  sidebarPanel(
    numericInput(inputId = "id1",
                 label = "Numeric input labeled id1", 
                 value = 0,
                 min=0, 
                 max=10, 
                 step=1),
    checkboxGroupInput(
      inputId = "id2", 
      label = "Checkbox", 
      choices = c("Value1" = "1",
                  "Value2" = "2",
                  "Value3" = "3"), 
      selected = NULL, 
      inline = FALSE),
    dateInput("date","Date:")
    ),
  mainPanel(
    h3("Illustrating outputs"),
    h4("you entered"),
    verbatimTextOutput("oid1"),
    h4("you entered"),
    verbatimTextOutput("oid2"),
    h4("you entered"),
    verbatimTextOutput("odate")
    )
  ))