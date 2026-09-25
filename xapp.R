library(shiny)

ui <- fluidPage(
  
  titlePanel("My first big Shiny app"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        "n",
        "Choose a number:",
        min = 1,
        max = 100,
        value = 50
      )
    ),
    
    mainPanel(
      h3("Your number is:"),
      textOutput("number")
    )
  )
)

server <- function(input, output, session) {
  
  output$number <- renderText({
    input$n
  })
}

shinyApp(ui = ui, server = server)