#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
library(shiny)
library(ggplot2)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Correlation Guessing Game"),
  sidebarLayout(
    sidebarPanel(
      numericInput("guess",
                   "Your guess for cor(X, Y):",
                   value = 0, step = 0.01),
      actionButton("submit", "Submit Guess"),
      actionButton("reload", "Restart"),
      verbatimTextOutput("feedback"),
      verbatimTextOutput("solution")
    ),
    mainPanel(
      # Use 70% width; dynamic height maintains square aspect
      plotOutput(
        outputId = "scatter",
        width = "70%"
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Function to generate new data
  generateData <- function() {
    X      <- runif(50, min = -10, max = 10)
    C      <- runif(1, min = -3, max = 3)
    E      <- sample(c(-1, 1, 2, 3), 1)
    Y0     <- C * X^E
    err_sd <- sample(c(0, 0.5, 1, 2, 3), 1)
    Y_raw  <- Y0 + rnorm(length(X), mean = 0, sd = sd(Y0) * err_sd)
    Y      <- (Y_raw - min(Y_raw)) / diff(range(Y_raw)) * 20 - 10
    list(df = data.frame(X = X, Y = Y), true_cor = cor(X, Y))
  }
  
  # Reactive storage for data
  rv <- reactiveVal(generateData())
  
  # Create a guess state, default to not submitted
  state <- reactiveValues(
    submitted = FALSE
  )
  
  # Make sure guess state is updated when submit is pressed
  observeEvent(input$submit, {
    state$submitted <- TRUE
  })
  
  # Restart data
  observeEvent(input$reload, {
    rv(generateData())
    updateNumericInput(session, "guess", value = 0)
    state$submitted <- FALSE
  })
  
  # Capture guess only on submit
  guessVal <- eventReactive(input$submit, {
    input$guess
  })
  
  # Scatterplot with equal axes and optional regression line
  output$scatter <- renderPlot({
    df <- rv()$df
    p <- ggplot(df, aes(x = X, y = Y)) +
      geom_point() +
      labs(x = "X", y = "Y") +
      theme_minimal() +
      coord_fixed(ratio = 1)
    # Add regression line after solution is shown
    if (input$submit > input$reload) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    p
  }, height = function() {
    # Keep plot square: height equals 70% of available width
    session$clientData$output_scatter_width * 0.7
  })
  
  # Feedback messaging
  output$feedback <- renderText({
    req(state$submitted)
    cor_val <- rv()$true_cor
    guess <- guessVal()
    err <- abs(guess - cor_val)
    sign_correct <- sign(guess) == sign(cor_val)
    
    if (!sign_correct) {
      return("Wrong direction (sign). Try again!")
    }
    
    if (err <= 0.05) {
      "Perfect guess!"
    } else if (err <= 0.1) {
      "Very good guess!"
    } else if (err <= 0.20) {
      "Good guess!"
    } else {
      "Almost. Try again!"
    }
  })
  
  # Display true correlation
  output$solution <- renderText({
    req(state$submitted)
    paste0("True correlation: ", round(rv()$true_cor, 2))
  })
}

# ---- Run App ----
shinyApp(ui, server)



