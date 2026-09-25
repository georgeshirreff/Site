# library(ggplot2)
# library(dplyr)

n_func <- function(n, at_start = F, hyphen = F, et_un = T){
  if(n == 0) return("")
  if(n < 10){
    un = ifelse(et_un & (!at_start), "et-un", "un")
    return(paste0(ifelse(hyphen, "-", ""), 
                  c(un, "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf")[n]))
  }
  if(n < 20){
    return(paste0(ifelse(hyphen, "-", ""), 
                  c("dix", "onze", "douze", "treize", "quatorze", "quinze", "seize", "dix-sept", "dix-huit", "dix-neuf")[n - 9]))
  }
  if(n < 60){
    return(paste0(ifelse(hyphen, "-", ""), 
                  c("vingt", "trente", "quarante", "cinquante")[(n%/%10 - 1)], n_func(n%%10, hyphen = T, et_un = T)))
  }
  if(n < 80){
    return(paste0(ifelse(hyphen, "-", ""), 
                  "soixante", n_func(n%%20, hyphen = T, et_un = T)))
  }
  if(n < 100){
    return(paste0(ifelse(hyphen, "-", ""), 
                  "quatre-vingt", n_func(n%%20, hyphen = T, et_un = F)))
  }
  if(n < 1000){
    return(paste0(ifelse(hyphen, "-", ""), 
                  ifelse(n%/%100 == 1, "", paste0(n_func(n%/%100, hyphen = F, et_un = F), "-")), 
                  "cent", 
                  n_func(n%%100, hyphen = T, et_un = F)))
  }
  if(n < 1000000){
    return(paste0(ifelse(hyphen, "-", ""), 
                  ifelse(n%/%1000 == 1, "", paste0(n_func(n%/%1000, hyphen = F, et_un = F), "-")), 
                  "mille", 
                  n_func(n%%1000, hyphen = T, et_un = F)))
  }
}

# gamme_tib <- tibble(lo = c(1, 1, 1, 1980, 1900, 1, 1), 
#                         hi = c(10, 100, 1000, 2030, 2100, 10000, 999999), 
#                     label = paste0(lo, sep = "-", hi))

gamme_tib <- data.frame(lo = c(1, 1, 1, 1980, 1900, 1, 1), 
                    hi = c(10, 100, 1000, 2030, 2100, 10000, 999999))

gamme_tib$label = paste0(gamme_tib$lo, sep = "-", gamme_tib$hi)




js <- 
'$(document).keyup(function(event) {
  console.log("keypress detected");
  if ($("#tente").is(":focus") && (event.key == "Enter")) {
    console.log("enter pressed");
    $("#soumettre").click();
  }
});'

ui <- fluidPage(
  
  # gl_talk_shinyUI(id = "talk"), 
  
  # Application title
  # titlePanel("French numbers"),
  
  tags$script(HTML(js)),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "gamme", 
                   label = "Gamme:", 
                   choices = as.list(gamme_tib$label) |> setNames(gamme_tib$label), 
                   selected = "1-10"),
      actionButton("launch", label = "Lancer"), 
      
      br(),
      
      # Text input for guessing
      textInput(inputId = "tente", 
                label = "Ecrivez le numéro en chiffres:", 
                placeholder = "Tapez votre réponse puis Entrée"),
      
      # Submit guess button
      actionButton(inputId = "soumettre", label = "Soumettre")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("prompt")),
      
      h3(textOutput("result")),
      
      # h3(textOutput("test")),
      
      textOutput("score"),
      
      textOutput("streak"), 
      
      plotOutput("response_time_plot", width = "400px")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive value to store the random number
  game <- reactiveValues(
    number = NULL,
    started = FALSE,
    score = 0,
    streak = 0, 
    start_time = NULL,
    
    number_vec = integer(0),
    response_vec = integer(0), 
    responseTime_vec = double(0), 
    
  )
  
  # -----------------------------
  # Helper: generate number
  # -----------------------------
  
  generate_number <- function() {
    
    min_val <- gamme_tib$lo[gamme_tib$label == input$gamme]
    max_val <- gamme_tib$hi[gamme_tib$label == input$gamme]
    
    sample(min_val:max_val, 1)
    
  }
  
  
  
  # Generate random number when launch button is pressed
  observeEvent(input$launch, {
    game$number <- generate_number()
    game$started <- TRUE
    game$start_time <- Sys.time()
    
    updateTextInput(session, "tente", value = "")
  })
  
  number_text = reactive({
    n_func(game$number, at_start = T)})
  
  output$prompt <- renderText({
    
    if (!game$started) {
      return("Cliquez sur Lancer")
    }
    
    number_text()
    
  })
  
  # callModule(gl_talk_shiny, "talk", transcript = number_text())
  
  
  # Check the guess
  observeEvent(input$soumettre, {
    
    responseTime <- as.numeric(Sys.time() - game$start_time, units = "secs")
    req(game$started)
    
    # output$test <- renderText({ "checking" })
    
    tente_value <- suppressWarnings(as.numeric(trimws(input$tente)))
    
    if (is.na(tente_value)) {

      output$result <- renderText({
        "Entrez un numéro valide."
      })

      return()
    }
    
    game$number_vec = c(game$number_vec, game$number)
    game$response_vec = c(game$response_vec, tente_value)
    game$responseTime_vec = c(game$responseTime_vec, responseTime)
    

    if (tente_value == game$number) {
      
      game$score <- game$score + 1
      game$streak <- game$streak + 1
      
      
      output$result <- renderText({
        paste0("🎉 Vrai!", "\t", responseTime)
      })
      
      # Next question automatically
      game$number <- generate_number()
      game$start_time <- Sys.time()
      
    } else {
      
      game$streak <- 0
      
      output$result <- renderText({
        paste0("Faux!", "\t", responseTime)
      })
      
      # new number after wrong answer too
      game$number <- generate_number()
      game$start_time <- Sys.time()
      
    }
    
    updateTextInput(session, "tente", value = "")
    
  })
  
  output$score <- renderText({paste0("Score: ", game$streak)})
  output$streak <- renderText({paste0("Streak: ", game$score)})
  
  output$response_time_plot <- renderPlot({
    
    n = length(game$responseTime_vec)
    if(n > 0){
      
      time_storage <- data.frame(number = game$number_vec[1:n], response = game$response_vec[1:n], responseTime = game$responseTime_vec)
      
      time_storage$correct = time_storage$number == time_storage$response
      
      with(time_storage, plot(x = number, y = responseTime, col = ifelse(correct, 1, 2), xlab = "True number", ylab = "Time (s)", pch = 16))
      
      
        # tibble(number = game$number_vec[1:n], response = game$response_vec[1:n], responseTime = game$responseTime_vec) %>% 
        #   mutate(correct = number == response) %>%           
        #   ggplot(aes(x = number, y = responseTime, colour = correct)) + 
        #   geom_point() + 
        #   theme_bw()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

