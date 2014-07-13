library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("data science FTW!"),
  sidebarPanel(
    h3("Sidebae text")
  ),
  mainPanel(
    h3("Main Panel text")
  )
))