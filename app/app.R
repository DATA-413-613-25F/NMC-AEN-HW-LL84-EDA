## ===============================
## NYC LL84 Building Energy Shiny App
## Honglin Wang 
## ===============================
library(shiny)
library(tidyverse)
library(broom)
library(DT)

library(bslib)
library(thematic)
library(showtext)

thematic_shiny(font = "auto")

## 1. Business Logic -------------
## 1.1 Introduction tab content

intro_content <- tags$div(
  class = "intro",
  tags$h3("Welcome to the NYC LL84 Building Energy Explorer"),
  tags$p(
    "This app lets you explore building energy and water performance data for New York City,",
    "based on public benchmarking data disclosed under Local Law 84."
  ),
  tags$h4("What you can do in this app"),
  tags$ul(
    tags$li("Use the Univariate tab to look at distributions of single variables by reporting year."),
    tags$li("Use the Bivariate tab to compare two variables and fit simple linear models."),
    tags$li("Use the Data Table tab to filter and browse the full benchmark dataset.")
  ),
  tags$h4("Data sources"),
  tags$p(
    "Main dataset: NYC Building Energy and Water Data Disclosure for Local Law 84 (NYC Open Data portal): ",
    tags$a(
      "NYC Open Data – LL84 Benchmarking",
      href  = "https://data.cityofnewyork.us/Environment/NYC-Building-Energy-and-Water-Data-Disclosure-for-/5zyy-y8am/about_data",
      target = "_blank"
    ),
    "."
  ),
  tags$p(
    "Benchmarking is required under Local Law 84 (LL84) for large buildings in NYC, ",
    "and data are reported through the EPA Portfolio Manager tool."
  ),
  tags$p(
    "This app is for teaching and exploratory analysis only, ",
    "and should not be used for official compliance or policy decisions."
  )
)

## 1.2 Read in Data
nyc_raw <- read_csv("Desktop/NYC_Building_Energy_and_Water_Data_Disclosure_for_Local_Law_84_(2022-Present)_20251027.csv")
nyc_raw

## 1.3 Transform & create key variables ----
dc_energy <- nyc_raw |>
  dplyr::rename(
    Report_Year        = `Calendar Year`,
    Energy_Star_Score  = `ENERGY STAR Score`,
    Electricity_Grid_Usage = `Electricity Use - Grid Purchase (kWh)`,
    SQFT_Gross         = `Property GFA - Calculated (Buildings) (ft²)`,
    SQFT_Tax           = `Property GFA - Self-Reported (ft²)`,
    Built              = `Year Built`,
    Ward               = Borough,
    Type_SS            = `Primary Property Type - Self Selected`,
    Type_EPA           = `Primary Property Type - Portfolio Manager-Calculated`,
    Metered_Energy     = `Metered Areas (Energy)`,
    Metered_Water      = `Metered Areas (Water)`
  ) |>
  ## 把这些变量转成 factor
  mutate(
    Ward           = as.factor(Ward),
    Report_Year    = as.factor(Report_Year),
    Type_SS        = as.factor(Type_SS),
    Type_EPA       = as.factor(Type_EPA),
    Metered_Energy = as.factor(Metered_Energy),
    Metered_Water  = as.factor(Metered_Water)
  ) |>
  ## 创建 Era
  mutate(
    Era = case_when(
      Built < 1900 ~ "Pre-1900",
      Built < 1951 ~ "Early-Mid 20th",
      Built < 2000 ~ "Late 20th",
      Built < 2011 ~ "Aughts",
      TRUE        ~ "Teens and later"
    ),
    Era = factor(
      Era,
      levels = c(
        "Pre-1900",
        "Early-Mid 20th",
        "Late 20th",
        "Aughts",
        "Teens and later"
      )
    ),
    .after = Built
  )

## 1.4 Helper: t.test -> tibble
t_test_tbl <- function(x, mu) {
  tt <- t.test(x, mu = mu)
  tibble(
    mu        = mu,
    estimate  = unname(tt$estimate),
    p_value   = tt$p.value,
    conf_low  = tt$conf.int[1],
    conf_high = tt$conf.int[2]
  )
}

## 1.5 Theme
theme_large_axes <- theme(
  axis.title = element_text(size = rel(1.2)),
  axis.text  = element_text(size = rel(1.2))
)

## ===============================
## 2. User Interface -------------
## ===============================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("NYC LL84 Building Energy Explorer"),
  
  tabsetPanel(
    
    ## ---- Tab: Introduction ----
    tabPanel(
      "Introduction",
      intro_content
    ),
    
    ## ---- Tab 1: Univariate ----
    tabPanel(
      "Univariate analysis",
      sidebarLayout(
        sidebarPanel(
          varSelectInput(
            inputId = "sv_var",
            label = "Single variable:",
            data = dc_energy,
            selected = "Site_EUI"  # 默认用 Site EUI
          ),
          checkboxInput(
            inputId = "sv_log",
            label = "Log transform single variable?",
            value = FALSE
          ),
          checkboxInput(
            inputId = "sv_flip",
            label = "Flip coordinates for factor variable?",
            value = FALSE
          ),
          sliderInput(
            inputId = "sv_bins",
            label = "Number of bins (numeric variables):",
            min = 1,
            max = 100,
            value = 40
          ),
          numericInput(
            inputId = "sv_mu",
            label = "Null hypothesis mean (mu)",
            min   = 0,
            max   = 1000,  # 可以按需要改，比如 Site EUI 上限
            value = 0
          ),
          checkboxGroupInput(
            inputId = "sv_years",
            label = "Report Years:",
            choices  = sort(unique(dc_energy$Report_Year)),
            selected = "2022"
          )
        ),
        mainPanel(
          plotOutput("sv_plot"),
          tableOutput("ttest_table")
        )
      )
    ),
    
    ## ---- Tab 2: Bivariate ----
    tabPanel(
      "Bivariate analysis",
      sidebarLayout(
        sidebarPanel(
          varSelectInput(
            inputId = "mv_x",
            label = "X variable:",
            data = dc_energy,
            selected = "SQFT_Gross"
          ),
          checkboxInput(
            inputId = "mv_log_x",
            label = "Log transform X?",
            value = FALSE
          ),
          varSelectInput(
            inputId = "mv_y",
            label = "Y variable:",
            data = dc_energy,
            selected = "Site_EUI"
          ),
          checkboxInput(
            inputId = "mv_log_y",
            label = "Log transform Y?",
            value = FALSE
          ),
          checkboxInput(
            inputId = "mv_smoother",
            label = "Show linear smoother & model results?",
            value = TRUE
          ),
          checkboxInput(
            inputId = "mv_nonlinear",
            label = "Add non-linear smoother (LOESS)?",
            value = FALSE
          ),
          checkboxGroupInput(
            inputId = "mv_years",
            label = "Report Years (bivariate):",
            choices  = sort(unique(dc_energy$Report_Year)),
            selected = "2022"
          )
        ),
        mainPanel(
          plotOutput("bv_plot"),
          verbatimTextOutput("lm_summary")
        )
      )
    ),
    
    ## ---- Tab 3: Data Table ----
    tabPanel(
      "Data Table",
      sidebarLayout(
        sidebarPanel(
          checkboxInput(
            inputId = "dt_numeric_only",
            label = "Show numeric variables only?",
            value = FALSE
          )
        ),
        mainPanel(
          DT::dataTableOutput("dc_table")
        )
      )
    )
    
  ) # end tabsetPanel
) # end fluidPage

## ===============================
## 3. Server Logic ---------------
## ===============================

server <- function(input, output, session) {
  
  ## 3.1 Single Variable Plot ----
  output$sv_plot <- renderPlot({
    
    var_name <- as.character(input$sv_var)
    var_sym  <- rlang::sym(var_name)
    
    sv_data <- dc_energy |>
      dplyr::filter(Report_Year %in% input$sv_years) |>
      dplyr::filter(!is.na(!!var_sym))
    
    # 如果是 numeric，顺便去掉 0（通常是无效值）
    if (is.numeric(dc_energy[[var_name]])) {
      sv_data <- sv_data |> dplyr::filter(!!var_sym != 0)
    }
    
    is_cat <- is.factor(dc_energy[[var_name]]) ||
      is.character(dc_energy[[var_name]])
    
    # log 检查
    if (isTRUE(input$sv_log) && !is.numeric(dc_energy[[var_name]])) {
      plot.new()
      title("Log transform only works for numeric variables.")
      return(invisible(NULL))
    }
    
    if (isTRUE(input$sv_log) && is.numeric(dc_energy[[var_name]]) &&
        any(sv_data[[var_name]] <= 0, na.rm = TRUE)) {
      plot.new()
      title("Log transform requires all values > 0.")
      return(invisible(NULL))
    }
    
    if (is_cat) {
      p <- ggplot(sv_data, aes(x = !!var_sym)) +
        geom_bar() +
        facet_wrap(~ Report_Year, scales = "free_x")
      
      if (isTRUE(input$sv_flip)) {
        p <- p + coord_flip()
      }
      
    } else {
      p <- ggplot(sv_data, aes(x = !!var_sym)) +
        geom_histogram(bins = input$sv_bins) +
        facet_wrap(~ Report_Year, scales = "free_x")
      
      if (isTRUE(input$sv_log)) {
        p <- p + scale_x_log10()
      }
    }
    
    p + theme_large_axes
  })
  
  ## 3.2 t-test Table -------------
  output$ttest_table <- renderTable({
    
    var_name <- input$sv_var
    
    if (!is.numeric(dc_energy[[var_name]])) {
      return(tibble(
        message = "Select a numeric variable for t-test."
      ))
    }
    
    var_sym <- rlang::sym(var_name)
    
    x <- dc_energy |>
      dplyr::filter(Report_Year %in% input$sv_years) |>
      dplyr::pull(!!var_sym)
    
    x <- x[!is.na(x) & x != 0]
    
    if (length(x) == 0) {
      return(tibble(
        message = "No non-zero, non-missing values for current selection."
      ))
    }
    
    if (isTRUE(input$sv_log)) {
      x <- log10(x)
    }
    
    t_test_tbl(x, mu = input$sv_mu)
  })
  
  ## 3.3 Bivariate Plot -----------
  output$bv_plot <- renderPlot({
    
    x_name <- as.character(input$mv_x)
    y_name <- as.character(input$mv_y)
    
    x_sym <- rlang::sym(x_name)
    y_sym <- rlang::sym(y_name)
    
    bv_data <- dc_energy |>
      dplyr::filter(Report_Year %in% input$mv_years) |>
      dplyr::filter(
        !is.na(!!x_sym), !is.na(!!y_sym)
      )
    
    if (is.numeric(dc_energy[[x_name]])) {
      bv_data <- bv_data |> dplyr::filter(!!x_sym != 0)
    }
    if (is.numeric(dc_energy[[y_name]])) {
      bv_data <- bv_data |> dplyr::filter(!!y_sym != 0)
    }
    
    x_num <- is.numeric(dc_energy[[x_name]])
    y_num <- is.numeric(dc_energy[[y_name]])
    
    if (nrow(bv_data) == 0) {
      plot.new()
      title("No data for current selection.")
      return(invisible(NULL))
    }
    
    # log 检查 X
    if (isTRUE(input$mv_log_x)) {
      if (!x_num) {
        plot.new()
        title("Log transform for X only works for numeric variables.")
        return(invisible(NULL))
      }
      if (any(bv_data[[x_name]] <= 0, na.rm = TRUE)) {
        plot.new()
        title("Log transform for X requires all values > 0.")
        return(invisible(NULL))
      }
    }
    
    # log 检查 Y
    if (isTRUE(input$mv_log_y)) {
      if (!y_num) {
        plot.new()
        title("Log transform for Y only works for numeric variables.")
        return(invisible(NULL))
      }
      if (any(bv_data[[y_name]] <= 0, na.rm = TRUE)) {
        plot.new()
        title("Log transform for Y requires all values > 0.")
        return(invisible(NULL))
      }
    }
    
    ## case 1: both numeric
    if (x_num && y_num) {
      
      p <- ggplot(bv_data, aes(x = !!x_sym, y = !!y_sym, color = Report_Year)) +
        geom_point(alpha = 0.6)
      
      if (isTRUE(input$mv_log_x)) {
        p <- p + scale_x_log10(name = paste0(x_name, " (log scale)"))
      } else {
        p <- p + xlab(x_name)
      }
      
      if (isTRUE(input$mv_log_y)) {
        p <- p + scale_y_log10(name = paste0(y_name, " (log scale)"))
      } else {
        p <- p + ylab(y_name)
      }
      
      if (isTRUE(input$mv_smoother)) {
        p <- p + geom_smooth(method = "lm", se = FALSE, inherit.aes = TRUE)
      }
      
      if (isTRUE(input$mv_nonlinear)) {
        p <- p + geom_smooth(
          se = FALSE,
          linetype = 2
        )
      }
      p <- p + theme_large_axes
      return(p)
    }
    
    ## case 2: x numeric, y categorical
    if (x_num && !y_num) {
      p <- ggplot(bv_data, aes(x = !!y_sym, y = !!x_sym, fill = Report_Year)) +
        geom_boxplot(alpha = 0.7)
      
      if (isTRUE(input$mv_log_x)) {
        p <- p + scale_y_log10(name = paste0(x_name, " (log scale)"))
      } else {
        p <- p + ylab(x_name)
      }
      
      p <- p + xlab(y_name)
      p <- p + theme_large_axes
      return(p)
    }
    
    ## case 3: x categorical, y numeric
    if (!x_num && y_num) {
      p <- ggplot(bv_data, aes(x = !!x_sym, y = !!y_sym, fill = Report_Year)) +
        geom_boxplot(alpha = 0.7)
      
      if (isTRUE(input$mv_log_y)) {
        p <- p + scale_y_log10(name = paste0(y_name, " (log scale)"))
      } else {
        p <- p + ylab(y_name)
      }
      
      p <- p + xlab(x_name)
      p <- p + theme_large_axes
      return(p)
    }
    
    ## case 4: both categorical
    p <- ggplot(bv_data, aes(x = !!x_sym, y = !!y_sym, color = Report_Year)) +
      geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
      xlab(x_name) + ylab(y_name) +
      theme_large_axes
    p
  })
  
  ## 3.4 Linear Model Summary -----
  output$lm_summary <- renderPrint({
    
    if (!isTRUE(input$mv_smoother)) {
      cat("Select 'Show linear smoother & model results?' to see linear model summary.")
      return(invisible(NULL))
    }
    
    x_name <- as.character(input$mv_x)
    y_name <- as.character(input$mv_y)
    
    if (!is.numeric(dc_energy[[x_name]]) || !is.numeric(dc_energy[[y_name]])) {
      cat("Linear model requires both X and Y to be numeric.")
      return(invisible(NULL))
    }
    
    lm_data <- dc_energy |>
      dplyr::filter(Report_Year %in% input$mv_years) |>
      dplyr::filter(
        !is.na(.data[[x_name]]),
        !is.na(.data[[y_name]]),
        .data[[x_name]] != 0,
        .data[[y_name]] != 0
      )
    
    if (nrow(lm_data) < 2) {
      cat("Not enough data for selected variables and years.")
      return(invisible(NULL))
    }
    
    # log transform
    if (isTRUE(input$mv_log_x)) {
      if (any(lm_data[[x_name]] <= 0, na.rm = TRUE)) {
        cat("Log transform for X requires all values > 0.")
        return(invisible(NULL))
      }
      lm_data[[x_name]] <- log10(lm_data[[x_name]])
    }
    if (isTRUE(input$mv_log_y)) {
      if (any(lm_data[[y_name]] <= 0, na.rm = TRUE)) {
        cat("Log transform for Y requires all values > 0.")
        return(invisible(NULL))
      }
      lm_data[[y_name]] <- log10(lm_data[[y_name]])
    }
    
    lm_data <- lm_data |>
      dplyr::filter(
        is.finite(.data[[x_name]]),
        is.finite(.data[[y_name]])
      )
    
    if (nrow(lm_data) < 2) {
      cat("Not enough finite data after transformations.")
      return(invisible(NULL))
    }
    
    fml <- as.formula(paste(y_name, "~", x_name))
    
    fit <- lm(fml, data = lm_data)
    summary(fit)
  })
  
  ## 3.5 Data Table ----------------
  output$dc_table <- DT::renderDataTable({
    
    data_to_show <- dc_energy
    
    if (isTRUE(input$dt_numeric_only)) {
      num_cols <- sapply(dc_energy, is.numeric)
      data_to_show <- dc_energy[, num_cols, drop = FALSE]
    }
    
    DT::datatable(
      data_to_show,
      options = list(pageLength = 20),
      filter = "top"
    )
  })
  
}

## ===============================
## 4. Run the App ----------------
## ===============================
shinyApp(ui, server)
