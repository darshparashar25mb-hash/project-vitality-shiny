# ==========================
# Required packages
# ==========================
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("DT")

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)

# ---------- Helper functions ----------

compute_fcff <- function(
    rev0      = 50,
    g         = 0.20,
    ebit_m    = 0.15,
    tax       = 0.21,
    da_m      = 0.04,
    capex_m   = 0.08,
    nwc_m     = 0.03,
    years     = 5
) {
  year <- 1:years
  revenue <- rev0 * (1 + g) ^ year
  ebit    <- revenue * ebit_m
  da      <- revenue * da_m
  capex   <- revenue * capex_m
  d_nwc   <- revenue * nwc_m
  fcff    <- ebit * (1 - tax) + da - capex - d_nwc
  
  data.frame(
    Year = year,
    Revenue = revenue,
    EBIT = ebit,
    DandA = da,
    CapEx = capex,
    DeltaNWC = d_nwc,
    FCFF = fcff
  )
}

compute_wacc <- function(
    rf   = 0.042,
    mrp  = 0.055,
    beta = 1.45,
    rd   = 0.07,
    w_d  = 0.15,
    w_e  = 0.85,
    tax  = 0.21
) {
  ke  <- rf + beta * mrp
  rd_a <- rd * (1 - tax)
  wacc <- w_e * ke + w_d * rd_a
  list(ke = ke, rd_after = rd_a, wacc = wacc)
}

compute_valuation <- function(
    fcff_df,
    wacc = 0.1118,
    pgr  = 0.03,
    net_debt = 15,
    shares  = 5
) {
  years <- fcff_df$Year
  fcff  <- fcff_df$FCFF
  
  disc_factor <- (1 + wacc) ^ years
  pv_fcff <- fcff / disc_factor
  
  fcff5 <- tail(fcff, 1)
  tv5 <- fcff5 * (1 + pgr) / (wacc - pgr)
  
  pv_tv <- tv5 / ((1 + wacc) ^ max(years))
  ev    <- sum(pv_fcff) + pv_tv
  eq_v  <- ev - net_debt
  price <- eq_v / shares
  
  list(
    pv_fcff = sum(pv_fcff),
    pv_tv   = pv_tv,
    ev      = ev,
    eq_v    = eq_v,
    price   = price,
    tv5     = tv5
  )
}

# ---------- UI ----------

ui <- fluidPage(
  titlePanel("Project Vitality – PulsePoint AI DCF & Monte Carlo Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Base Case Assumptions"),
      numericInput("rev0", "Year 0 Revenue (M$)", 50, 1, 500, 1),
      numericInput("g", "Revenue Growth (Yr 1–5, %)", 20, 0, 50, 0.5),
      numericInput("ebit_m", "EBIT Margin (% of Revenue)", 15, 0, 50, 0.5),
      numericInput("tax", "Tax Rate (%)", 21, 0, 50, 0.5),
      numericInput("da_m", "D&A (% of Revenue)", 4, 0, 50, 0.5),
      numericInput("capex_m", "CapEx (% of Revenue)", 8, 0, 50, 0.5),
      numericInput("nwc_m", "ΔNWC (% of Revenue)", 3, 0, 50, 0.5),
      hr(),
      h4("Capital Structure & WACC"),
      numericInput("rf", "Risk-Free Rate (%)", 4.2, 0, 15, 0.1),
      numericInput("mrp", "Market Risk Premium (%)", 5.5, 0, 15, 0.1),
      sliderInput("beta", "Target Beta", min = 1.0, max = 2.0, value = 1.45, step = 0.01),
      numericInput("rd", "Pre-tax Cost of Debt (%)", 7.0, 0, 20, 0.1),
      sliderInput("wd", "Debt Weight (D/V)", min = 0, max = 0.5, value = 0.15, step = 0.01),
      hr(),
      h4("Terminal & Equity Bridge"),
      numericInput("pgr", "Perpetuity Growth Rate (%)", 3.0, 0, 6, 0.1),
      numericInput("net_debt", "Net Debt (M$)", 15, 0, 200, 1),
      numericInput("shares", "Shares Outstanding (M)", 5, 0.1, 100, 0.1),
      hr(),
      h4("Monte Carlo Settings"),
      numericInput("n_sims", "Number of Simulations", 1000, 100, 5000, 100),
      actionButton("run_mc", "Run Monte Carlo")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Base Case DCF",
                 fluidRow(
                   column(6,
                          h4("FCFF Projection"),
                          DTOutput("fcff_table")
                   ),
                   column(6,
                          h4("FCFF Over Time"),
                          plotOutput("fcff_plot")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(6,
                          h4("WACC Breakdown"),
                          verbatimTextOutput("wacc_text")
                   ),
                   column(6,
                          h4("Valuation Summary"),
                          verbatimTextOutput("val_text")
                   )
                 )
        ),
        tabPanel("Monte Carlo Valuation",
                 fluidRow(
                   column(6,
                          h4("Price Distribution"),
                          plotOutput("mc_hist")
                   ),
                   column(6,
                          h4("Simulation Stats"),
                          verbatimTextOutput("mc_stats")
                   )
                 )
        )
      )
    )
  )
)

# ---------- Server ----------

server <- function(input, output, session) {
  
  wacc_params <- reactive({
    w_d <- input$wd
    w_e <- 1 - w_d
    compute_wacc(
      rf   = input$rf / 100,
      mrp  = input$mrp / 100,
      beta = input$beta,
      rd   = input$rd / 100,
      w_d  = w_d,
      w_e  = w_e,
      tax  = input$tax / 100
    )
  })
  
  fcff_base <- reactive({
    compute_fcff(
      rev0    = input$rev0,
      g       = input$g / 100,
      ebit_m  = input$ebit_m / 100,
      tax     = input$tax / 100,
      da_m    = input$da_m / 100,
      capex_m = input$capex_m / 100,
      nwc_m   = input$nwc_m / 100
    )
  })
  
  val_base <- reactive({
    wacc <- wacc_params()$wacc
    compute_valuation(
      fcff_df  = fcff_base(),
      wacc     = wacc,
      pgr      = input$pgr / 100,
      net_debt = input$net_debt,
      shares   = input$shares
    )
  })
  
  output$fcff_table <- renderDT({
    df <- fcff_base()
    df_fmt <- df %>%
      mutate(across(-Year, round, 2))
    datatable(df_fmt, options = list(pageLength = 5))
  })
  
  output$fcff_plot <- renderPlot({
    df <- fcff_base()
    ggplot(df, aes(x = Year, y = FCFF)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      scale_y_continuous(labels = label_dollar(suffix = "M", prefix = "$")) +
      theme_minimal() +
      labs(title = "Free Cash Flow to Firm (FCFF)",
           y = "FCFF (M$)", x = "Year")
  })
  
  output$wacc_text <- renderPrint({
    wp <- wacc_params()
    w_d <- input$wd
    w_e <- 1 - w_d
    cat(sprintf("Cost of Equity (Ke): %.2f%%\n", wp$ke * 100))
    cat(sprintf("After-tax Cost of Debt (Rd): %.2f%%\n", wp$rd_after * 100))
    cat(sprintf("Equity Weight (E/V): %.1f%%\n", w_e * 100))
    cat(sprintf("Debt Weight (D/V): %.1f%%\n\n", w_d * 100))
    cat(sprintf("WACC: %.2f%%", wp$wacc * 100))
  })
  
  output$val_text <- renderPrint({
    v <- val_base()
    cat(sprintf("PV of FCFF (Years 1–5): $%.2fM\n", v$pv_fcff))
    cat(sprintf("PV of Terminal Value: $%.2fM\n", v$pv_tv))
    cat(sprintf("Enterprise Value (EV): $%.2fM\n", v$ev))
    cat(sprintf("Equity Value: $%.2fM\n", v$eq_v))
    cat(sprintf("Implied Share Price: $%.2f\n", v$price))
  })
  
  mc_results <- eventReactive(input$run_mc, {
    n <- input$n_sims
    
    g_sim <- rnorm(n, mean = 0.20, sd = 0.05)
    
    r <- runif(n)
    a <- 0.10; b <- 0.22; c <- 0.15
    ebit_sim <- ifelse(
      r < (c - a) / (b - a),
      a + sqrt(r * (b - a) * (c - a)),
      b - sqrt((1 - r) * (b - a) * (b - c))
    )
    
    beta_sim <- runif(n, min = 1.20, max = 1.70)
    pgr_sim  <- rnorm(n, mean = 0.03, sd = 0.006)
    
    rf   <- input$rf / 100
    mrp  <- input$mrp / 100
    rd   <- input$rd / 100
    tax  <- input$tax / 100
    da_m <- input$da_m / 100
    capex_m <- input$capex_m / 100
    nwc_m   <- input$nwc_m / 100
    rev0    <- input$rev0
    net_debt <- input$net_debt
    shares   <- input$shares
    w_d <- input$wd
    w_e <- 1 - w_d
    
    prices <- numeric(n)
    
    for (i in seq_len(n)) {
      ke_i   <- rf + beta_sim[i] * mrp
      rd_a_i <- rd * (1 - tax)
      wacc_i <- w_e * ke_i + w_d * rd_a_i
      
      fcff_i <- compute_fcff(
        rev0    = rev0,
        g       = g_sim[i],
        ebit_m  = ebit_sim[i],
        tax     = tax,
        da_m    = da_m,
        capex_m = capex_m,
        nwc_m   = nwc_m
      )
      
      val_i <- compute_valuation(
        fcff_df  = fcff_i,
        wacc     = wacc_i,
        pgr      = pgr_sim[i],
        net_debt = net_debt,
        shares   = shares
      )
      prices[i] <- val_i$price
    }
    
    data.frame(Price = prices)
  })
  
  output$mc_hist <- renderPlot({
    req(mc_results())
    df <- mc_results()
    ggplot(df, aes(x = Price)) +
      geom_histogram(fill = "darkorange", color = "white", bins = 40) +
      theme_minimal() +
      labs(title = "Monte Carlo Distribution of Implied Share Price",
           x = "Price ($)", y = "Frequency")
  })
  
  output$mc_stats <- renderPrint({
    req(mc_results())
    p <- mc_results()$Price
    cat(sprintf("Simulations: %d\n", length(p)))
    cat(sprintf("Mean Price: $%.2f\n", mean(p)))
    cat(sprintf("Median Price: $%.2f\n", median(p)))
    cat(sprintf("5th Percentile: $%.2f\n", quantile(p, 0.05)))
    cat(sprintf("95th Percentile: $%.2f\n", quantile(p, 0.95)))
  })
}

shinyApp(ui = ui, server = server)
