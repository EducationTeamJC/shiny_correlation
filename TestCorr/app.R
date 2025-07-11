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
library(latex2exp)

# ---- UI ----
ui <- fluidPage(
  titlePanel("The Correlator Game"),
  tabsetPanel(
    # Introduction tab
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      h3("Welcome to The Correlator Game"),
                      p("In this applet you will be able to practice with the Pearson's correlation and linearity in three different games."),
                      h4("What is Pearson's correlation?"),
                      p("The Pearson's correlation (noted as 'r') is a measure for direction and strength of a linear relationship between two continuous variables:"),
                      tags$ul(
                        tags$li("r = 1 indicates a perfect positive relationship (as x increases, y increases proportionally)"),
                        tags$li("r = -1 indicates a perfect negative relationship (as x increases, y decreases proportionally)"),  
                        tags$li("r = 0 indicates no linear relationship")
                      )
               )
             ),
             fluidRow(
               column(6,
                      h4("For the math enthousiasts among us:"),
                      p("The correlation r between x and y is calculated using the following formula:"),
                      plotOutput("pearson_plot", width = "100%", height = "280px")
               ),
               column(6,
                      h4("Interpretation Guide"),
                      p(strong("Strong correlations (|r| > 0.7):"), "Variables move together in a predictable way"),
                      p(strong("Moderate correlations (0.3 < |r| < 0.7):"), "Some relationship exists but with more scatter"),
                      p(strong("Weak correlations (|r| < 0.3):"), "Little to no linear relationship"),
                      br(),
                      p("Remember: Correlation does not imply causation! A strong correlation between two variables does not mean one causes the other.")
               )
             )
    ),
    # Game 1: Multiple-choice correlation guess
    tabPanel("Guess the Correlation (multiple choice)",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("guess1_ui"),
                 actionButton("submit1", "Submit Guess"),
                 actionButton("reload1", "Restart"),
                 verbatimTextOutput("feedback1"),
                 verbatimTextOutput("solution1")
               ),
               mainPanel(
                 plotOutput("scatter1", width = "85%")
               )
             )
    ),
    # Game 2: Numeric guess with linear/nonlinear toggle
    tabPanel("Guess the Correlation (numeric input)",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("mode2", "Data mechanism:",
                              choices = c("Linear only" = "linear", "Allow nonlinear" = "nonlinear"),
                              selected = "linear"
                 ),
                 numericInput("guess2", "Your guess for cor(X, Y):", value = 0, step = 0.01),
                 actionButton("submit2", "Submit Guess"),
                 actionButton("reload2", "Restart"),
                 verbatimTextOutput("feedback2"),
                 verbatimTextOutput("solution2")
               ),
               mainPanel(
                 plotOutput("scatter2", width = "85%")
               )
             )
    ),
    # Game 3 placeholder
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
  # Output for intro tab
  output$pearson_plot <- renderPlot({
    # Create a plot that displays the Pearson correlation formula
    p <- ggplot() + 
      xlim(3, 7) + 
      ylim(0, 5) +
      # Main formula
      annotate("text", x = 5, y = 3.5, 
               label = TeX("$r = \\frac{1}{n-1} \\sum \\left(\\frac{x_i - \\bar{x}}{s_x}\\right) \\left(\\frac{y_i - \\bar{y}}{s_y}\\right)$"),
               size = 8, hjust = 0.5) +
      # Explanation of components
      annotate("text", x = 5, y = 2.5, 
               label = "Where:", 
               size = 5, hjust = 0.5, fontface = "bold") +
      annotate("text", x = 5, y = 2, 
               label = TeX("$r$ = Pearson correlation coefficient"),
               size = 5, hjust = 0.5) +
      annotate("text", x = 5, y = 1.6, 
               label = TeX("$n$ = number of data points"),
               size = 5, hjust = 0.5) +
      annotate("text", x = 5, y = 1.2, 
               label = TeX("$\\bar{x}, \\bar{y}$ = sample means of $x$ and $y$"),
               size = 5, hjust = 0.5) +
      annotate("text", x = 5, y = 0.8, 
               label = TeX("$s_x, s_y$ = sample standard deviations of $x$ and $y$"),
               size = 5, hjust = 0.5) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
    p
  })
  
  
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
  
  
  # ==== GAME 2: Reactive data generator with linear/nonlinear mode ====  
  data2 <- reactive({
    
    input$reload2  # Regenerate data on reload or mode change
    
    X <- runif(50, min = -10, max = 10)
    C <- runif(1, min = -4, max = 4)
    # Branch on the UI mode: linear-only forces exponent = 1
    E <- if (isTRUE(input$mode2 == "linear")) {
      1
    } else {
      sample(c(-1, 1, 2, 3), 1)
    }
    Y_base   <- C * X^E
    err_sd   <- runif(1, min = 0, max = 3)
    Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
    Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
    list(
      df       = data.frame(X = X, Y = Y_scaled),
      true_cor = cor(X, Y_scaled)
    )
  })
  
  state2 <- reactiveValues(submitted = FALSE)
  observeEvent(input$submit2, { state2$submitted <- TRUE })
  observeEvent(input$reload2, { state2$submitted <- FALSE })
  observeEvent(input$mode2, { state2$submitted <- FALSE })
  
  output$scatter2 <- renderPlot({
    req(data2())
    df <- data2()$df
    p  <- ggplot(df, aes(X, Y)) +
      geom_point() + coord_fixed() + theme_minimal() +
      labs(x = "X", y = "Y")
    if (state2$submitted) p <- p + geom_smooth(method = "lm", se = FALSE)
    p
  }, height = function() session$clientData$output_scatter2_width * 0.7)
  
  output$feedback2 <- renderText({
    req(state2$submitted)
    val   <- data2()$true_cor
    guess <- input$guess2
    err   <- abs(guess - val)
    if (sign(guess) != sign(val))            "Wrong direction (sign)."
    else if (err <= 0.05)                    "Perfect guess!"
    else if (err <= 0.1)                     "Very good guess!"
    else if (err <= 0.2)                     "Good guess!"
    else                                     "Almost. Try again!"
  })
  
  output$solution2 <- renderText({
    req(state2$submitted)
    sprintf("True correlation: %.2f", data2()$true_cor)
  })
}
  # ---- Run App ----
  shinyApp(ui, server)

