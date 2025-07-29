#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyBS)
library(ggplot2)
library(latex2exp)

# ---- UI ----
ui <- fluidPage(
  # Add custom CSS for the logo positioning
  tags$head(
    tags$style(HTML("
      /* Header styling */
      .navbar-default {
        background-color: #2196F3 !important;
        border: none !important;
        margin-bottom: 0 !important;
      }
      
      h1 {
        background-color: #2196F3;
        color: white !important;
        margin: 0 !important;
        padding: 1em 1.5em;
        font-weight: 500;
      }
      
      .sidebar-logo {
        width: 80%;
        margin: 1.5em auto;
        text-align: center;
        position: absolute;
        bottom: 10vh;
        left: 70%;
        transform: translateX(-50%);
      }
      .sidebar-logo img {
        width: 100%;
        height: auto;
        max-width: 12em;
      }
      .logo-text {
        font-size: 1.1em;
        color: #666;
        margin-top: 0.8em;
        text-align: center;
      }
      .logo-text a {
        color: #2196F3;
        text-decoration: none;
      }
      .logo-text a:hover {
        text-decoration: underline;
      }
      /* Ensure the sidebar has relative positioning for absolute positioning to work */
      .col-sm-4 {
        position: relative;
        min-height: 100vh;
        padding-bottom: 10vh;
      }
      
      /* Introduction tab needs relative positioning for absolute logo */
      .tab-content {
        position: relative;
      }
      .intro-logo-text a {
        color: #2196F3;
        text-decoration: none;
      }
      .intro-logo-text a:hover {
        text-decoration: underline;
      }
    "))
  ),
  
  titlePanel("The Correlation Game"),
  
  tabsetPanel(
    # Introduction tab
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      h3("Welcome to The Correlation Game"),
                      p("In this applet you will be able to practice with the Pearson's correlation and linearity in three different games."),
                      
                      h4("What is Pearson's correlation?"),
                      p("The Pearson's correlation (noted as 'r') is a measure for direction and strength of a linear relationship between two continuous variables:"),
                      tags$ul(
                        tags$li("r = 1 indicates a perfect positive relationship (as x increases, y increases proportionally)"),
                        tags$li("r = -1 indicates a perfect negative relationship (as x increases, y decreases proportionally)"),  
                        tags$li("r = 0 indicates no linear relationship")
                      ),
                      h4("Interpretation Guide"),
                      tags$ul(
                        tags$li("Strong correlations (|r| > 0.7): Variables move together in a predictable way"),
                        tags$li("Moderate correlations (0.3 < |r| < 0.7): Some relationship exists but with more scatter"),
                        tags$li("Weak correlations (|r| < 0.3): Little to no linear relationship")
                      ),
                      p("Remember: Correlation does not imply causation! A strong correlation between two variables does not mean one causes the other."),
                      
                      # Logo positioned absolutely to not interfere with layout
                      div(style = "position: absolute; top: 20px; right: 60px; text-align: center; z-index: 10;",
                          img(src = "umc_utrecht_logo.png", alt = "UMC Utrecht Logo", style = "width: 200px; height: auto;"),
                          div(style = "font-size: 14px; color: #666; margin-top: 15px; line-height: 1.3;", 
                              HTML("Created by <a href='https://www.linkedin.com/in/merlin-urbanski-151ba3268' target='_blank' style='color: #2196F3; text-decoration: none;'>Merlin Urbanski</a>"), br(),
                              HTML("and the <a href='https://datasci-biostat.juliuscentrum.nl/about/' target='_blank' style='color: #2196F3; text-decoration: none;'>UMCU biostatistics teaching team</a>"))
                      ),
                      
                      bsCollapse(
                        id = "math_collapse", open = NULL,
                        bsCollapsePanel("For the math enthusiasts among us:",
                                        column(6,
                                               p("The correlation r between x and y is calculated using the following formula:"),
                                               plotOutput("pearson_plot", width = "100%", height = "280px"),
                                               withMathJax(p("From this formula, we can see that for each pair of values \\((x_i, y_i)\\), we check how far each value is from its average (mean). This \"distance from average\" is then standardized by dividing by the standard deviation of \\(X\\) and \\(Y\\), so that the result does not depend on the units (e.g., minutes vs. hours).")),
                                               
                                               withMathJax(p("You can imagine that if both \\(x_i\\) and \\(y_i\\) are relatively the same distance above (or below) their averages — in other words, they move in the same direction — then they contribute positively to the correlation. If one is above average and the other is below, they contribute negatively.")),
                                               
                                               withMathJax(p("These individual contributions are summed up and then divided by \\(n−1\\) to get the average: this is Pearson's correlation coefficient, \\(r\\). It tells us how strongly and in what direction \\(X\\) and \\(Y\\) move together."))
                                        )
                        ))
               )
             )
    ),
    # Game 1: Multiple-choice correlation guess
    tabPanel("Guess the Correlation (multiple choice)",
             sidebarLayout(
               sidebarPanel(
                 h4("Instruction:", style = "color: #2196F3; margin-bottom: 10px;"),
                 p("Out of the four options select the correct correlation strength between X and Y that you see in the scatterplot.", 
                   style = "margin-bottom: 20px; padding: 10px; background-color: #f8f9fa; border-left: 3px solid #2196F3;"),
                 uiOutput("guess1_ui"),
                 actionButton("submit1", "Submit Guess"),
                 actionButton("reload1", "Restart"),
                 verbatimTextOutput("feedback1"),
                 verbatimTextOutput("solution1"),
                 # Logo in sidebar
                 div(class = "sidebar-logo",
                     img(src = "umc_utrecht_logo.png", alt = "UMC Utrecht Logo"),
                     div(class = "logo-text", 
                         HTML("Created by <a href='https://www.linkedin.com/in/merlin-urbanski-151ba3268' target='_blank'>Merlin Urbanski</a>"), br(),
                         HTML("and the <a href='https://datasci-biostat.juliuscentrum.nl/about/' target='_blank'>UMCU biostatistics teaching team</a>"))
                 )
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
                 h4("Instruction:", style = "color: #2196F3; margin-bottom: 10px;"),
                 p("Guess the correlation strength that you see between X and Y in the scatterplot by typing it in the field. You can select the data generating mechanism (the true underlying relationship between X and Y in the scatterplot) between \"Linear only\" (easy) and \"Allow nonlinear\" (harder). Optionally you can try to guess the ranked based Spearman correlation and see how it differs from the \"normal\" Pearson correlation.", 
                   style = "margin-bottom: 20px; padding: 10px; background-color: #f8f9fa; border-left: 3px solid #2196F3;"),
                 radioButtons("mode2", "Data mechanism:",
                              choices = c("Linear only" = "linear", "Allow nonlinear" = "nonlinear"),
                              selected = "linear"
                 ),
                 h4("Pearson Correlation"),
                 numericInput("guess2", "Your guess for cor(X, Y):", value = 0, step = 0.01),
                 actionButton("submit2", "Submit Guess"),
                 br(), br(),
                 
                 # Collapsible panel for Spearman correlation
                 tags$details(
                   tags$summary("Advanced: Spearman Correlation", 
                                style = "cursor: pointer; font-weight: bold; color: #337ab7;"),
                   br(),
                   p("Spearman correlation measures monotonic relationships using ranks instead of raw values.",
                     style = "font-size: 12px; color: #666;"),
                   numericInput("guess2_spearman", "Your guess for Spearman cor(X, Y):", 
                                value = 0, step = 0.01),
                   actionButton("submit2_spearman", "Submit Spearman Guess")
                 ),
                 
                 br(),
                 actionButton("reload2", "Restart"),
                 verbatimTextOutput("feedback2"),
                 verbatimTextOutput("solution2"),
                 verbatimTextOutput("feedback2_spearman"),
                 verbatimTextOutput("solution2_spearman"),
                 # Logo in sidebar
                 div(class = "sidebar-logo",
                     img(src = "umc_utrecht_logo.png", alt = "UMC Utrecht Logo"),
                     div(class = "logo-text", 
                         HTML("Created by <a href='https://www.linkedin.com/in/merlin-urbanski-151ba3268' target='_blank'>Merlin Urbanski</a>"), br(),
                         HTML("and the <a href='https://datasci-biostat.juliuscentrum.nl/about/' target='_blank'>UMCU biostatistics teaching team</a>"))
                 )
               ),
               mainPanel(
                 plotOutput("scatter2", width = "85%")
               )
             )
    ),
    # Game 3: Guess the data relationship
    tabPanel("Guess the Data Relationship",
             sidebarLayout(
               sidebarPanel(
                 h4("Instruction:", style = "color: #2196F3; margin-bottom: 10px;"),
                 p("Select the correct data generating mechanism (the true underlying relationship between X and Y in the scatterplot) from the five options below.", 
                   style = "margin-bottom: 20px; padding: 10px; background-color: #f8f9fa; border-left: 3px solid #2196F3;"),
                 radioButtons("guess3", "Select the data generating mechanism:",
                              choices = c("Linear" = "linear",
                                          "Quadratic" = "quadratic", 
                                          "Cubic" = "cubic",
                                          "Exponential" = "exponential",
                                          "Logarithmic" = "logarithmic"),
                              selected = "linear"
                 ),
                 actionButton("submit3", "Submit Guess"),
                 actionButton("reload3", "Restart"),
                 verbatimTextOutput("feedback3"),
                 verbatimTextOutput("solution3"),
                 # Logo in sidebar
                 div(class = "sidebar-logo",
                     img(src = "umc_utrecht_logo.png", alt = "UMC Utrecht Logo"),
                     div(class = "logo-text", 
                         HTML("Created by <a href='https://www.linkedin.com/in/merlin-urbanski-151ba3268' target='_blank'>Merlin Urbanski</a>"), br(),
                         HTML("and the <a href='https://datasci-biostat.juliuscentrum.nl/about/' target='_blank'>UMCU biostatistics teaching team</a>"))
                 )
               ),
               mainPanel(
                 plotOutput("scatter3", width = "85%")
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
    if (isTRUE(input$mode2 == "linear")) {
      E <- 1
      Y_base   <- C * X^E
      err_sd   <- runif(1, min = 0, max = 3)
      Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
      Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
    } else {
      # Allow nonlinear: polynomial or exponential
      mechanism <- sample(c("polynomial", "exponential"), 1)
      if (mechanism == "polynomial") {
        E <- sample(c(-1, 1, 2, 3), 1)
        Y_base   <- C * X^E
        err_sd   <- runif(1, min = 0, max = 3)
        Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
        Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
      } else {
        # Exponential mechanism
        C <- sample(c(-5, -4, 4, 5), size = 1)
        E <- runif(1, min = 0.3, max = 0.9)
        Y_base   <- C * exp(E * X)
        err_sd   <- runif(1, min = 0, max = 0.7)
        Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
        Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
      }
    }
    
    list(
      df       = data.frame(X = X, Y = Y_scaled),
      true_cor = cor(X, Y_scaled),
      true_spearman = cor(X, Y_scaled, method = "spearman")
    )
  })
  
  state2 <- reactiveValues(submitted = FALSE, submitted_spearman = FALSE)
  observeEvent(input$submit2, { state2$submitted <- TRUE })
  observeEvent(input$submit2_spearman, { state2$submitted_spearman <- TRUE })
  observeEvent(input$reload2, { 
    state2$submitted <- FALSE
    state2$submitted_spearman <- FALSE
    # Reset input values
    updateNumericInput(session, "guess2", value = 0)
    updateNumericInput(session, "guess2_spearman", value = 0)
  })
  observeEvent(input$mode2, { 
    state2$submitted <- FALSE
    state2$submitted_spearman <- FALSE
    # Reset input values when mode changes
    updateNumericInput(session, "guess2", value = 0)
    updateNumericInput(session, "guess2_spearman", value = 0)
  })
  
  output$scatter2 <- renderPlot({
    req(data2())
    df <- data2()$df
    p  <- ggplot(df, aes(X, Y)) +
      geom_point() + coord_fixed() + theme_minimal() +
      labs(x = "X", y = "Y")
    if (state2$submitted || state2$submitted_spearman) p <- p + geom_smooth(method = "lm", se = FALSE)
    p
  }, height = function() session$clientData$output_scatter2_width * 0.7)
  
  output$feedback2 <- renderText({
    req(state2$submitted)
    val   <- data2()$true_cor
    guess <- input$guess2
    err   <- abs(guess - val)
    if (sign(guess) != sign(val))            "Pearson: Wrong direction (sign)."
    else if (err <= 0.05)                    "Pearson: Perfect guess!"
    else if (err <= 0.1)                     "Pearson: Very good guess!"
    else if (err <= 0.2)                     "Pearson: Good guess!"
    else                                     "Pearson: Almost. Try again!"
  })
  
  output$solution2 <- renderText({
    req(state2$submitted)
    sprintf("True Pearson correlation: %.2f", data2()$true_cor)
  })
  
  output$feedback2_spearman <- renderText({
    req(state2$submitted_spearman)
    val   <- data2()$true_spearman
    guess <- input$guess2_spearman
    err   <- abs(guess - val)
    if (sign(guess) != sign(val))            "Spearman: Wrong direction (sign)."
    else if (err <= 0.05)                    "Spearman: Perfect guess!"
    else if (err <= 0.1)                     "Spearman: Very good guess!"
    else if (err <= 0.2)                     "Spearman: Good guess!"
    else                                     "Spearman: Almost. Try again!"
  })
  
  output$solution2_spearman <- renderText({
    req(state2$submitted_spearman)
    sprintf("True Spearman correlation: %.2f", data2()$true_spearman)
  })
  
  # ==== GAME 3: Guess the data relationship ====
  genData3 <- function() {
    # Sample 50 X values
    X <- runif(50, min = -10, max = 10)
    # Sample either 1, 2, 3, 4, or 5 
    mm <- as.numeric(sample(1:5, size = 1))
    
    if (mm == 1) {
      # Generate Y using a linear relationship
      C <- runif(1, min = -4, max = 4)
      Y_base   <- C * X
      err_sd   <- runif(1, min = 0.3, max = 0.7)
      Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
      Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
      relationship <- "linear"
      
    } else if (mm == 2) {
      # Generate Y using a quadratic relationship
      C <- runif(1, min = -4, max = 4)
      E <- 2 # Fixed exponent for quadratic
      Y_base   <- C * X^E
      err_sd   <- runif(1, min = 0.3, max = 0.7)
      Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
      Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
      relationship <- "quadratic"
      
    } else if (mm == 3) {
      # Generate Y using a cubic relationship
      C <- runif(1, min = -4, max = 4)
      E <- 3 # Fixed exponent for cubic
      Y_base   <- C * X^E
      err_sd   <- runif(1, min = 0.3, max = 0.7)
      Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
      Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
      relationship <- "cubic"
      
    } else if (mm == 4){
      # Generate Y using an exponential relationship
      C <- sample(c(-5, -4, 4, 5), size = 1)
      E <- runif(1, min = 0.3, max = 0.9) # Random exponent for exponential
      Y_base   <- C * exp(E * X)
      err_sd   <- runif(1, min = 0, max = 0.7)
      Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
      Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
      relationship <- "exponential"
      
    } else {
      # Generate Y using a logarithmic relationship
      C <- sample(c(-5, -4, 4, 5), size = 1)
      E <- runif(1, min = 5, max = 7) # Random exponent for logarithmic
      X_pos <- X - min(X) + 1   # now X_pos ∈ [1, 21]
      Y_base   <- C * log(E * X_pos + 1) # Adding 1 to avoid log(0)
      err_sd   <- runif(1, min = 0, max = 0.5)
      Y_noise  <- Y_base + rnorm(length(X), mean = 0, sd = sd(Y_base) * err_sd)
      Y_scaled <- (Y_noise - min(Y_noise)) / diff(range(Y_noise)) * 20 - 10
      relationship <- "logarithmic"
    }
    
    list(
      df = data.frame(X = X, Y = Y_scaled),
      true_relationship = relationship
    )
  }
  
  # Reactive data for Game 3
  rv3    <- reactiveVal(genData3())
  state3 <- reactiveValues(submitted = FALSE)
  
  # Observers for Game 3
  observeEvent(input$submit3, { state3$submitted <- TRUE })
  observeEvent(input$reload3, {
    rv3(genData3())
    state3$submitted <- FALSE
  })
  
  # Outputs for Game 3
  output$scatter3 <- renderPlot({
    df <- rv3()$df
    p  <- ggplot(df, aes(X, Y)) + geom_point() + coord_fixed() + theme_minimal() +
      labs(x = "X", y = "Y")
    p
  }, height = function() session$clientData$output_scatter3_width * 0.7)
  
  output$feedback3 <- renderText({
    req(state3$submitted)
    true_rel <- rv3()$true_relationship
    guess    <- input$guess3
    if (guess == true_rel) {
      "Correct! Well done!"
    } else {
      "Incorrect. Try to look at the shape of the relationship more carefully."
    }
  })
  
  output$solution3 <- renderText({
    req(state3$submitted)
    paste0("True relationship: ", rv3()$true_relationship)
  })
}

# ---- Run App ----
shinyApp(ui, server)