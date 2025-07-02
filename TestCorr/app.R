#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Multi-Tab Correlation Games"),
  tabsetPanel(
    tabPanel("Game 1",
             sidebarLayout(
               sidebarPanel(
                 # Dynamic multiple-choice input for correlation guesses
                 uiOutput("guess1_ui"),
                 actionButton("submit1", "Submit Guess"),
                 actionButton("reload1", "Restart"),
                 verbatimTextOutput("feedback1"),
                 verbatimTextOutput("solution1")
               ),
               mainPanel(
                 plotOutput("scatter1", width = "70%")
               )
             )
    ),
    tabPanel("Game 2",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("mode2", "Data mechanism:",
                              choices = list(
                                "Linear only" = "linear",
                                "Allow nonlinear" = "nonlinear"
                              ),
                              selected = "nonlinear"),
                 numericInput("guess2", "Your guess for cor(X, Y):", value = 0, step = 0.01),
                 actionButton("submit2", "Submit Guess"),
                 actionButton("reload2", "Restart"),
                 verbatimTextOutput("feedback2"),
                 verbatimTextOutput("solution2")
               ),
               mainPanel(
                 plotOutput("scatter2", width = "70%")
               )
             )
    ),
    tabPanel("Game 3",
             fluidRow(
               column(12,
                      h3("Third game placeholder"),
                      p("This space is reserved for your third game. Add controls and outputs here!")
               )
             )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # ==== GAME 1: Linear generator with multiple-choice guesses ====  
  genData1 <- function() {
    X         <- runif(50, min = -10, max = 10)
    C         <- runif(1, min = -4, max = 4)
    Y_base    <- C * X
    error_sd  <- runif(1, min = 0, max = 3)
    Y_noise   <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * error_sd)
    Y_scaled  <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
    true_cor  <- round(cor(X, Y_scaled), 2)
    # Generate three distractors
    # 2nd guess
    repeat {
      rd1 <- runif(1, -0.7, 0.7)
      g2  <- round(true_cor + rd1, 2)
      if (g2 > 1 || g2 < -1) g2 <- round(true_cor - rd1, 2)
      if (abs(g2 - true_cor) > 0.1) break
    }
    # 3rd guess
    repeat {
      rd2 <- runif(1, -0.7, 0.7)
      g3  <- round(true_cor + rd2, 2)
      if (g3 > 1 || g3 < -1) g3 <- round(true_cor - rd2, 2)
      if (abs(g3 - true_cor) > 0.1 && abs(g3 - g2) > 0.1) break
    }
    # 4th guess
    repeat {
      rd3 <- runif(1, -0.7, 0.7)
      g4  <- round(true_cor + rd3, 2)
      if (g4 > 1 || g4 < -1) g4 <- round(true_cor - rd3, 2)
      if (abs(g4 - true_cor) > 0.1 && abs(g4 - g2) > 0.1 && abs(g4 - g3) > 0.1) break
    }
    options <- sample(c(true_cor, g2, g3, g4))
    list(df = data.frame(X = X, Y = Y_scaled), true_cor = true_cor, options = options)
  }
  
  # Reactive data+options for Game 1
  rv1    <- reactiveVal(genData1())
  state1 <- reactiveValues(submitted = FALSE)
  
  # UI for multiple-choice
  output$guess1_ui <- renderUI({
    opts <- rv1()$options
    radioButtons("guess1", "Select your guess for cor(X, Y):",
                 choices = setNames(opts, sprintf("%.2f", opts)))
  })
  
  # Observers for Game 1
  observeEvent(input$submit1, { state1$submitted <- TRUE })
  observeEvent(input$reload1, {
    rv1(genData1())
    state1$submitted <- FALSE
  })
  
  # Outputs for Game 1
  output$scatter1 <- renderPlot({
    df <- rv1()$df
    p  <- ggplot(df, aes(X, Y)) + geom_point() + coord_fixed() + theme_minimal() +
      labs(x = "X", y = "Y")
    if (state1$submitted) p <- p + geom_smooth(method = "lm", se = FALSE)
    p
  }, height = function() session$clientData$output_scatter1_width * 0.7)
  
  output$feedback1 <- renderText({
    req(state1$submitted)
    cor_val <- rv1()$true_cor
    guess   <- as.numeric(input$guess1)
    if (guess == cor_val) {
      "Correct!"
    } else if (sign(guess) != sign(cor_val)) {
      "Wrong direction (sign)."
    } else {
      "Incorrect guess."
    }
  })
  
  output$solution1 <- renderText({
    req(state1$submitted)
    paste0("True correlation: ", sprintf("%.2f", rv1()$true_cor))
  })
  
  
  # ==== GAME 2: Exponent-based generator ====  
  genData2 <- function() {
    X      <- runif(50, min = -10, max = 10)
    C      <- runif(1, min = -4, max = 4)
    E      <- sample(c(-1, 1, 2, 3), 1)
    Y_base <- C * X^E
    err_sd <- runif(1, min = 0, max = 3)
    Y_noise <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
    Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
    list(df = data.frame(X = X, Y = Y_scaled), true_cor = cor(X, Y_scaled))
  }
  
  # Reactive values and state for Game 2
  rv2    <- reactiveVal(genData2())
  state2 <- reactiveValues(submitted = FALSE)
  
  # Game 2 observers
  observeEvent(input$submit2, { state2$submitted <- TRUE })
  observeEvent(input$reload2, {
    rv2(genData2())
    updateNumericInput(session, "guess2", value = 0)
    state2$submitted <- FALSE
  })
  
  # Game 2 guess and outputs
  guess2_val <- eventReactive(input$submit2, { input$guess2 })
  
  output$scatter2 <- renderPlot({
    df <- rv2()$df
    p  <- ggplot(df, aes(X, Y)) + geom_point() + coord_fixed() + theme_minimal() +
      labs(x = "X", y = "Y")
    if (state2$submitted) p <- p + geom_smooth(method = "lm", se = FALSE)
    p
  }, height = function() session$clientData$output_scatter2_width * 0.7)
  
  output$feedback2 <- renderText({
    req(state2$submitted)
    cor_val <- rv2()$true_cor
    guess   <- guess2_val()
    err     <- abs(guess - cor_val)
    if (sign(guess) != sign(cor_val)) {
      return("Wrong direction (sign). Try again!")
    }
    if (err <= 0.05)      "Perfect guess!"
    else if (err <= 0.1)  "Very good guess!"
    else if (err <= 0.2)  "Good guess!"
    else                  "Almost. Try again!"
  })
  
  output$solution2 <- renderText({
    req(state2$submitted)
    sprintf("True correlation: %.2f", rv2()$true_cor)
  })
}
# ---- Run App ----
shinyApp(ui, server)


