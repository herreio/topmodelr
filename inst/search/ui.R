library(shiny)

ui <- fluidPage(
  titlePanel("Title Topic Search"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("words", "Number of words to display",  
                  min = 1, max = 10, value = 5),
      sliderInput("topics", "Number of topics to display",  
                  min = 1, max = 5, value = 1),
      textInput("search", "", "natural language processing"),
      actionButton("button", "Search")
    ),
    mainPanel(
      h4("Results"),
      verbatimTextOutput("value")
    )
  )
)
