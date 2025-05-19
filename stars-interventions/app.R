###############################################################################
# app.R – Dynamic visualisation of intervention simulations
###############################################################################

# ---------------------------------------------------------------------------
# 1. PACKAGES ---------------------------------------------------------------
# ---------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)
library(bslib)      # for nicer Bootstrap cards (optional – installs with shiny)

theme_set(theme_minimal(base_size = 18))

# ---------------------------------------------------------------------------
# 2. LOAD & PREP DATA -------------------------------------------------------
# ---------------------------------------------------------------------------
file_path <- "InterventionAnalysisOutput.csv"   # adjust if needed
df <- read_csv(file_path, show_col_types = FALSE)

# ---- Create the `name` column if it is not already present ---------------
if (!"name" %in% names(df)) {
  intervention_cols <- c(
    ReductionFromWorkplaceEducation = "Workplace Education",
    ReductionFromFirearmHold        = "Firearm Hold",
    ReductionFromSafeStorage        = "Safe Storage",
    ReductionFromSafeStorageMeds    = "Safe Storage of Meds",
    ReductionFromHandgunPermit      = "Handgun Permit",
    ReductionFromAssertiveAftercare = "Assertive Aftercare",
    ReductionFromCashTransfer       = "Cash Transfers"
  )
  
  df <- df %>%
    mutate(
      name = pmap_chr(
        select(., all_of(names(intervention_cols))),
        function(...) {
          lbls <- intervention_cols[c(...) != 0]
          if (length(lbls) == 0) "Baseline" else paste(lbls, collapse = ", ")
        }
      )
    )
}

# Make sure key columns have the right type
df <- df %>%
  mutate(
    intervention_iter = as.factor(intervention_iter),
    year              = as.integer(year)
  )

baseline_df <- df %>% filter(ExperimentId == 1)   # first Baseline run

names_no_baseline <- unique(df$name[df$name != "Baseline"])

# ---------------------------------------------------------------------------
# 3. UI ---------------------------------------------------------------------
# ---------------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5),
  
  # ---- custom CSS -------------------------------------------------
  tags$head(
    tags$style(HTML("
      /* one rule for both sidebars */
      .scroll-y {
        overflow-y: auto;
        max-height: calc(100vh - 1rem);  /* fills the viewport, minus a gap */
      }
    "))
  ),
  fluidRow(
    column(
      width = 12,
      h1(class = "text-center",
         "STARS Intervention Visualization")
    )
  ),
  
  fluidRow(
    # ---- LEFT SIDEBAR -----------------------------------------------------
    column(
      width = 3,
      div(class = "scroll-y",
        h4("Select Interventions"),
        selectInput(
          "selected_names",
          label = "Intervention(s)",
          choices = names_no_baseline,
          multiple = TRUE,
          selectize = TRUE
        ),
        uiOutput("scenario_selectors"),
        tags$br(),
        tags$strong("Scenario definitions"),
        tags$pre(
          "Scenario 1: Full coverage, 2-year implementation
Scenario 2: Full coverage, 5-year implementation
Scenario 3: Half coverage, 2-year implementation
Scenario 4: Half coverage, 5-year implementation
Scenario 5: Total pop. coverage, immediate implementation"
        )
      )
    ),
    
    # ---- MAIN PANEL ------------------------------------------------------
    column(
      width = 6,
      plotOutput("deaths_plot", height = "400px"),
      plotOutput("attempts_plot", height = "400px")
    ),
    
    # ---- RIGHT SIDEBAR ---------------------------------------------------
    column(
      width = 3,
      div(class = "scroll-y",
        h4("Impact (2025-2050)"),
        uiOutput("impact_cards")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# 4. SERVER -----------------------------------------------------------------
# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Dynamically create one Scenario dropdown per selected intervention --
  output$scenario_selectors <- renderUI({
    req(input$selected_names)
    
    map(input$selected_names, \(nm) {
      # available scenarios for this intervention
      avail <- df %>%
        filter(name == nm) %>%
        arrange(intervention_iter) %>%
        pull(intervention_iter) %>%
        unique()
      
      selectInput(
        inputId  = paste0("scen_", digest::digest(nm)),   # safe unique id
        label    = paste0("Scenario for \"", nm, "\""),
        choices  = avail,
        selected = avail[1],
        multiple = FALSE
      )
    })
  })
  
  # --- Collect the chosen series into one dataframe -----------------------
  chosen_series <- reactive({
    req(input$selected_names)
    
    # build a small look-up table that couples each name to its chosen scenario
    sel_tbl <- tibble(
      name = input$selected_names,
      intervention_iter = map_chr(
        input$selected_names,
        \(nm) input[[paste0("scen_", digest::digest(nm))]]
      )
    )
    
    # keep baseline + only those rows that appear in sel_tbl
    df %>%
      semi_join(sel_tbl, by = c("name", "intervention_iter")) %>%   # ← key line
      bind_rows(baseline_df) %>%                                    # put baseline back
      mutate(
        label = if_else(
          name == "Baseline",
          "Baseline",
          paste0(name, " (Scen ", intervention_iter, ")")
        )
      )
  })

  
  # --- Plot: deaths_rate ---------------------------------------------------
  output$deaths_plot <- renderPlot({
    dat <- chosen_series()
    
    max_per_row <- 2
    n_rows <- ceiling(length(unique(dat$label)) / max_per_row)
    
    ggplot(dat, aes(x = year, y = deaths_rate,
                    colour = label,
                    group  = label)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(
        values = c("Baseline" = "black",
                   setNames(
                     palette.colors(length(unique(dat$label)) + 2, "Okabe-Ito")[-1],
                     setdiff(unique(dat$label), "Baseline")
                   )
        ),
        name = NULL,
        # labels = \(lab) sub("\\.\\d+$", "", lab)   # drop iter in legend label
      ) +
      guides(colour = guide_legend(nrow = n_rows, byrow = TRUE)) +
      labs(title = "Deaths Rate", x = NULL, y = "Deaths per 100k Pop.") +
      theme(legend.position = "bottom", legend.box = "horizontal")
  })
  
  # --- Plot: attempts_rate -------------------------------------------------
  output$attempts_plot <- renderPlot({
    dat <- chosen_series()
    
    ggplot(dat, aes(x = year, y = attempts_rate,
                    colour = interaction(name, intervention_iter),
                    group  = interaction(name, intervention_iter))) +
      geom_line(linewidth = 1) +
      scale_colour_manual(
        values = c("Baseline.1" = "black",
                   setNames(
                     palette.colors(length(unique(dat$name)) + 2, "Okabe-Ito")[-1],
                     setdiff(unique(interaction(dat$name, dat$intervention_iter)),
                             "Baseline.1")
                   )
        ),
        guide = "none"   # legend shown in first plot
      ) +
      labs(title = "Attempts Rate", x = NULL, y = "Attempts per 100k Pop.")
  })
  
  # --- Impact cards (2025-2050) -------------------------------------------
  output$impact_cards <- renderUI({
    dat <- chosen_series()
    if (nrow(dat) == 0) return(NULL)
    
    base_2525 <- baseline_df %>% filter(year >= 2025 & year <= 2050)
    base_deaths   <- base_2525$deaths_rate
    base_attempts <- base_2525$attempts_rate
    
    # Loop over each non-baseline series
    cards <- dat %>%
      filter(name != "Baseline", year >= 2025 & year <= 2050) %>%
      group_by(name, intervention_iter) %>%
      summarise(
        deaths   = list(deaths_rate),
        attempts = list(attempts_rate),
        .groups = "drop"
      ) %>%
      pmap(\(name, intervention_iter, deaths, attempts) {
        deaths_vec   <- unlist(deaths)
        attempts_vec <- unlist(attempts)
        
        pct_red_deaths    <- 100 * mean((base_deaths - deaths_vec) / base_deaths)
        pct_red_attempts  <- 100 * mean((base_attempts - attempts_vec) / base_attempts)
        cum_red_deaths    <- sum(base_deaths   - deaths_vec)
        cum_red_attempts  <- sum(base_attempts - attempts_vec)
        avg_red_deaths    <- mean(base_deaths   - deaths_vec)
        avg_red_attempts  <- mean(base_attempts - attempts_vec)
        
        card(
          card_header(paste0(name, " – Scenario ", intervention_iter)),
          card_body(
            tags$ul(class = "list-unstyled mb-0",
                    tags$li(sprintf("Pct. Reduction of Deaths (2025-2050): %.1f%%", pct_red_deaths)),
                    tags$li(sprintf("Pct. Reduction of Attempts (2025-2050): %.1f%%", pct_red_attempts)),
                    tags$li(sprintf("Cumulative Deaths Reduction: %.0f", cum_red_deaths)),
                    tags$li(sprintf("Cumulative Attempts Reduction: %.0f", cum_red_attempts)),
                    tags$li(sprintf("Avg. Yearly Deaths Reduction: %.2f", avg_red_deaths)),
                    tags$li(sprintf("Avg. Yearly Attempts Reduction: %.2f", avg_red_attempts))
            )
          ),
          class = "mb-3"
        )
      })
    
    tagList(cards)
  })
}

# ---------------------------------------------------------------------------
# 5. RUN --------------------------------------------------------------------
# ---------------------------------------------------------------------------
shinyApp(ui, server)
