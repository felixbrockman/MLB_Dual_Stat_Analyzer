# app.R
# Dual Stat Analyzer - Compare How Players Rank in Two Stats at Once

library(shiny)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(plotly)

# HELPER FUNCTIONS

# Calculate plate appearances
calculate_PA <- function(AB, BB, HBP, SF, SH) {
  coalesce(AB, 0) + coalesce(BB, 0) + coalesce(HBP, 0) + 
  coalesce(SF, 0) + coalesce(SH, 0)
}

# Calculate innings pitched
calculate_IP <- function(IPouts) {
  if_else(!is.na(IPouts) & IPouts > 0, IPouts / 3, NA_real_)
}

# Recalculate batting stats from aggregated counting stats
recalculate_batting_stats <- function(df) {
  df %>%
    mutate(
      # Supporting calculations
      PA = calculate_PA(AB, BB_bat, HBP_bat, SF_bat, SH_bat),
      singles = if_else(!is.na(H_bat), H_bat - X2B - X3B - HR_bat, NA_integer_),
      
      # User-visible calculated stats
      XBH = X2B + X3B + HR_bat,
      AVG = if_else(!is.na(AB) & AB > 0, H_bat / AB, NA_real_),
      OBP = if_else((coalesce(AB, 0) + coalesce(BB_bat, 0) + 
                     coalesce(HBP_bat, 0) + coalesce(SF_bat, 0)) > 0,
                    (coalesce(H_bat, 0) + coalesce(BB_bat, 0) + 
                     coalesce(HBP_bat, 0)) / 
                    (coalesce(AB, 0) + coalesce(BB_bat, 0) + 
                     coalesce(HBP_bat, 0) + coalesce(SF_bat, 0)),
                    NA_real_),
      SLG = if_else(!is.na(AB) & AB > 0,
                    (coalesce(singles, 0) + 2 * coalesce(X2B, 0) + 
                     3 * coalesce(X3B, 0) + 4 * coalesce(HR_bat, 0)) / AB,
                    NA_real_),
      OPS = OBP + SLG,
      HR_pct = if_else(PA > 0, (HR_bat / PA) * 100, NA_real_),
      BB_pct = if_else(PA > 0, (BB_bat / PA) * 100, NA_real_),
      K_pct = if_else(PA > 0, (SO_bat / PA) * 100, NA_real_),
      SB_pct = if_else((coalesce(SB, 0) + coalesce(CS, 0)) > 0,
                       (SB / (coalesce(SB, 0) + coalesce(CS, 0))) * 100,
                       NA_real_),
      BB_K = if_else(SO_bat > 0, BB_bat / SO_bat, NA_real_),
      # Fielding stats
      FPCT = if_else((PO + A + E) > 0, (PO + A) / (PO + A + E), NA_real_),
      CS_pct = if_else((CS_fld + SB_fld) > 0, CS_fld / (CS_fld + SB_fld), NA_real_),
      PB_9 = if_else(InnOuts > 0, (PB * 27) / InnOuts, NA_real_)
    )
}

# Recalculate pitching stats from aggregated counting stats
recalculate_pitching_stats <- function(df) {
  df %>%
    mutate(
      IP = calculate_IP(IPouts),
      ERA = if_else(!is.na(IP) & IP > 0, (coalesce(ER, 0) * 9) / IP, NA_real_),
      WHIP = if_else(!is.na(IP) & IP > 0, (BB_pit + coalesce(H_pit, 0)) / IP, NA_real_),
      K_9 = if_else(!is.na(IP) & IP > 0, (SO_pit * 9) / IP, NA_real_),
      BB_9 = if_else(!is.na(IP) & IP > 0, (BB_pit * 9) / IP, NA_real_),
      HR_9 = if_else(!is.na(IP) & IP > 0, (HR_pit * 9) / IP, NA_real_),
      BAA = if_else((coalesce(BFP, 0) - BB_pit - 
                     coalesce(HBP_pit, 0) - coalesce(SH_pit, 0) - 
                     coalesce(SF_pit, 0)) > 0,
                    H_pit / (coalesce(BFP, 0) - BB_pit - 
                             coalesce(HBP_pit, 0) - coalesce(SH_pit, 0) - 
                             coalesce(SF_pit, 0)),
                    NA_real_),
      K_BB = if_else(BB_pit > 0, SO_pit / BB_pit, NA_real_),
      W_pct = if_else((coalesce(W, 0) + coalesce(L, 0)) > 0,
                      W / (coalesce(W, 0) + coalesce(L, 0)),
                      NA_real_)
    )
}

# UI

ui <- fluidPage(
  # Custom CSS for pickerInput to match stat dropdown style
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Passion+One:wght@400;700;900&display=swap", rel = "stylesheet"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,400;1,900&display=swap", rel = "stylesheet"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "scripts.js")
  ),
  
  titlePanel(
    h1("Dual Stat Analyzer", align = "center", 
       style = "font-size: 5.5em; margin-bottom: 15px; margin-top: 30px; padding-top: 12px;"),
    windowTitle = "Dual Stat Analyzer"
  ),
  
  h4("— Compare How MLB Players Rank in Two Stats at Once —", align = "center", 
     style = "margin-top: 5px; margin-bottom: 45px;"),
  
  # Search panel at the top - full width
  div(style = "background-color: #240368; padding: 15px; margin-bottom: 35px; border-radius: 5px;",
    # Player Pool Filters (including Player Type)
    div(style = "margin-bottom: 15px;",
      fluidRow(
        column(2,
          h5("Player Type:"),
          radioGroupButtons("player_type", NULL,
                           choices = list("Hitters" = "hitters", "Pitchers" = "pitchers"),
                           selected = "hitters",
                           direction = "horizontal",
                           justified = TRUE)
        ),
        column(2,
          h5("Year Range:"),
          uiOutput("years_dropdown_ui")
        ),
        column(2,
          h5("Primary Position(s):"),
          uiOutput("position_dropdown_ui")
        ),
        column(2,
          h5("Team(s):"),
          uiOutput("teams_dropdown_ui")
        ),
        column(2,
          h5("Bats:"),
          uiOutput("bats_filter")
        ),
        column(2,
          h5("Throws:"),
          uiOutput("throws_filter")
        )
      )
    ),
    
    # Stat Selection and Action Buttons on same line
    div(style = "margin-bottom: 10px;",
      div(
        class = "stat-bar",
        div(
          class = "stat-item stat-stat",
          div("Stat 1:", class = "stat-label"),
          div(style = "flex: 1;", uiOutput("stat1_dropdown"))
        ),
        div(
          class = "stat-item stat-stat",
          div("Stat 2:", class = "stat-label"),
          div(style = "flex: 1;", uiOutput("stat2_dropdown"))
        ),
        div(
          class = "stat-item stat-generate",
          actionButton("generate", "Generate Player Rankings",
                       class = "btn btn-default", width = "100%")
        ),
        div(
          class = "stat-item stat-reset",
          actionButton("reset_search", "Reset Search",
                       class = "btn btn-default", width = "100%")
        )
      )
    )
  ),
  
  # Main content area
  div(
    # Graph at the top
    h3("Explore Results Below", align = "center", style = "margin-bottom: 37px;"),
    div(
      id = "graph_search_wrap",
      style = "width: 85%; margin: 0 auto;",
      div(
        style = "display: flex; align-items: center; gap: 10px;",
        div(
          style = "flex: 1;",
          selectizeInput(
            "search_graph",
            label = NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Search player(s) to highlight in graph...",
              maxItems = 5,
              create = FALSE,
              persist = FALSE,
              openOnFocus = TRUE,
              plugins = list("remove_button"),
              # Close dropdown after a selection is made
              onItemAdd = I("function(value, item) { this.close(); this.blur(); }")
            ),
            width = "100%"
          )
        ),
        actionButton(
          "clear_search_graph",
          "Deselect all",
          class = "btn btn-default",
          style = "height: 38px; white-space: nowrap;"
        )
      )
    ),
    br(),
      div(
        id = "percentile_plot_wrap",
        style = "position: relative;",
        div(id = "graph_quadrant_help", HTML("*the top right quadrant shows players above average in both stats,<br>the bottom right shows below average in both, etc.")),
        div(id = "graph_tools_help", "← zoom, pan, and download png of graph"),
        plotlyOutput("percentile_plot", height = "600px")
      ),
    br(),
    br(),
    # Tables below the graph
    fluidRow(
      column(6,
             div(class = "ranking-title",
                 h3(textOutput("stat1_title"), align = "center")
             ),
             textInput("search1", label = NULL, placeholder = "Search player name...", width = "100%"),
             div(style = "height: 400px; overflow-y: auto; position: relative;",
                 tableOutput("ranking1"),
                 conditionalPanel(
                   condition = "!output.show_results",
                   div(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); 
                                background-color: rgba(36, 3, 104, 0.9); 
                                border: 1px solid #07bcff; 
                                border-radius: 4px; 
                                padding: 20px; 
                                text-align: center; 
                                z-index: 10;",
                       p("Rankings will populate after a search is made", 
                         style = "margin: 0; font-size: 16px; color: #07bcff;")
                   )
                 )
             ),
             p(textOutput("pool_count1"), align = "center", style = "margin-top: 10px;")
      ),
      column(6,
             div(class = "ranking-title",
                 h3(textOutput("stat2_title"), align = "center")
             ),
             textInput("search2", label = NULL, placeholder = "Search player name...", width = "100%"),
             div(style = "height: 400px; overflow-y: auto; position: relative;",
                 tableOutput("ranking2"),
                 conditionalPanel(
                   condition = "!output.show_results",
                   div(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); 
                                background-color: rgba(36, 3, 104, 0.9); 
                                border: 1px solid #07bcff; 
                                border-radius: 4px; 
                                padding: 20px; 
                                text-align: center; 
                                z-index: 10;",
                       p("Rankings will populate after a search is made", 
                         style = "margin: 0; font-size: 16px; color: #07bcff;")
                   )
                 )
             ),
             p(textOutput("pool_count2"), align = "center", style = "margin-top: 10px;")
      )
    )
  ),
  tags$footer(
    style = "
      width: 100%;
      margin-top: 60px;
      padding: 15px 0;
      text-align: center;
      font-size: 12px;
      color: rgba(255,255,255,0.65);
      background: transparent;
    ",
    HTML(paste0(
      'Data © <a href="https://www.seanlahman.com/baseball-archive/statistics/" ',
      'target="_blank" style="color:rgba(255,255,255,0.65); text-decoration: underline;">',
      'Sean Lahman — Lahman Baseball Database</a><br>',
      '© 2025 Felix Brockman. All rights reserved.<br>',
      'Analytics, visualizations, and concept design by Felix Brockman.'
    ))
  )
)

# SERVER

server <- function(input, output, session) {
  
  # Store last generated stat selections
  last_stat1 <- reactiveVal(NULL)
  last_stat2 <- reactiveVal(NULL)
  
  # Load datasets
  hitters_data <- reactive({
    readRDS("data_processed/hitters_by_year.rds")
  })
  
  pitchers_data <- reactive({
    readRDS("data_processed/pitchers_by_year.rds")
  })
  
  # Get current dataset based on player type
  current_data <- reactive({
    if (input$player_type == "hitters") {
      hitters_data()
    } else {
      pitchers_data()
    }
  })
  
  # User-visible stats for hitters (named vector: display label = column name)
  hitter_stats <- c(
    "R (runs)" = "R_bat",
    "H (hits)" = "H_bat",
    "2B (doubles)" = "X2B",
    "3B (triples)" = "X3B",
    "HR (home runs)" = "HR_bat",
    "RBI (runs batted in)" = "RBI",
    "SB (stolen bases)" = "SB",
    "XBH (extra base hits)" = "XBH",
    "BB (walks)" = "BB_bat",
    "AVG (batting average)" = "AVG",
    "OBP (on-base percentage)" = "OBP",
    "SLG (slugging percentage)" = "SLG",
    "OPS (on-base plus slugging)" = "OPS",
    "HR% (home run rate)" = "HR_pct",
    "BB% (walk rate)" = "BB_pct",
    "K% (strikeout rate)" = "K_pct",
    "SB% (stolen base success rate)" = "SB_pct",
    "BB/K (walk-to-strikeout ratio)" = "BB_K",
    "FLD% (fielding percentage)" = "FPCT",
    "CS (runners caught stealing, catchers only)" = "CS_fld",
    "CS% (caught stealing percentage, catchers only)" = "CS_pct",
    "PB/9 (passed balls per 9 innings, catchers only)" = "PB_9"
  )
  
  # User-visible stats for pitchers (named vector: display label = column name)
  # Note: BB_pit (walks) is kept in supporting data for calculations but not user-selectable
  pitcher_stats <- c(
    "W (wins)" = "W",
    "GP (games pitched)" = "G_pit",
    "GS (games started)" = "GS",
    "CG (complete games)" = "CG",
    "SHO (shutouts)" = "SHO",
    "SV (saves)" = "SV",
    "SO (strikeouts)" = "SO_pit",
    "IP (innings pitched)" = "IP",
    "ERA (earned run average)" = "ERA",
    "WHIP (walks plus hits per inning pitched)" = "WHIP",
    "K/9 (strikeouts per 9 innings)" = "K_9",
    "BB/9 (walks per 9 innings)" = "BB_9",
    "HR/9 (home runs allowed per 9 innings)" = "HR_9",
    "BAA (batting average against)" = "BAA",
    "K/BB (strikeout-to-walk ratio)" = "K_BB",
    "W-L% (win percentage)" = "W_pct"
  )
  
  # Get available stats based on player type
  available_stats <- reactive({
    if (input$player_type == "hitters") {
      hitter_stats
    } else {
      pitcher_stats
    }
  })
  
  # Helper function to get display name from stat value
  get_stat_display_name <- function(stat_value) {
    all_stats <- c(hitter_stats, pitcher_stats)
    display_name <- names(all_stats)[all_stats == stat_value]
    if (length(display_name) > 0) {
      return(display_name[1])
    }
    return(stat_value)  # Fallback to original if not found
  }
  
  # Stats where lower values are better (need reversed percentile calculation)
  # For these: lowest value = 100th percentile, highest value = 0th percentile
  lower_is_better_hitters <- c("K_pct", "PB_9")
  lower_is_better_pitchers <- c("ERA", "WHIP", "BB_9", "HR_9", "BAA")
  
  # Catcher-only stats (require position filter to be only "C")
  catcher_only_stats <- c("CS_fld", "CS_pct", "PB_9")
  
  # Helper function to check if a stat is "lower is better"
  is_lower_better <- function(stat_value, player_type) {
    if (player_type == "hitters") {
      return(stat_value %in% lower_is_better_hitters)
    } else {
      return(stat_value %in% lower_is_better_pitchers)
    }
  }
  
  # Helper function to check if a stat is catcher-only
  is_catcher_only_stat <- function(stat_value) {
    return(stat_value %in% catcher_only_stats)
  }
  
  # Helper functions for ranking
  make_ranking <- function(df, stat_col, lower_is_better = FALSE) {
    df2 <- df %>% filter(!is.na(.data[[stat_col]]))
    total <- nrow(df2)

    if (total == 0) {
      return(df2 %>% transmute(rank = integer(), playerID, full_name, value = numeric(), percentile = numeric()))
    }

    df2 <- if (lower_is_better) {
      df2 %>% arrange(.data[[stat_col]])
    } else {
      df2 %>% arrange(desc(.data[[stat_col]]))
    }

    df2 %>%
      mutate(rank = row_number()) %>%
      mutate(percentile = if (total > 1) ((total - rank) / (total - 1)) * 100 else rep(100.0, n())) %>%
      transmute(rank, playerID, full_name, value = .data[[stat_col]], percentile)
  }

  make_plot_name <- function(full_name, playerID) {
    dup <- duplicated(full_name) | duplicated(full_name, fromLast = TRUE)
    if_else(dup, paste0(full_name, " (", playerID, ")"), full_name)
  }
  
  # Get unique values for filters
  year_choices <- reactive({
    sort(unique(current_data()$yearID))
  })
  
  unique_positions <- reactive({
    data <- current_data()
    positions <- unique(data$primary_position)
    
    # For hitters, exclude "P" (pitcher position)
    if (input$player_type == "hitters") {
      positions <- positions[positions != "P"]
    }
    
    # Define desired order: C, 1B, 2B, 3B, SS, LF, CF, RF, DH, P
    desired_order <- c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH", "P")
    
    # Reorder positions according to desired order, keeping only those that exist
    ordered_positions <- desired_order[desired_order %in% positions]
    
    # Add any remaining positions not in desired order (shouldn't happen, but just in case)
    remaining <- setdiff(positions, desired_order)
    if (length(remaining) > 0) {
      ordered_positions <- c(ordered_positions, sort(remaining))
    }
    
    ordered_positions
  })
  
  unique_teams <- reactive({
    data <- current_data()
    all_teams <- unique(unlist(strsplit(data$teams_this_year[!is.na(data$teams_this_year)], ",")))
    teams_sorted <- sort(unique(all_teams))
    
    # Create named vector for display labels
    # Most teams use their abbreviation as both label and value
    teams_named <- setNames(teams_sorted, teams_sorted)
    
    # Custom labels for WAS and MON
    if ("WAS" %in% teams_named) {
      names(teams_named)[teams_named == "WAS"] <- "WAS (2005 - present)"
    }
    if ("MON" %in% teams_named) {
      names(teams_named)[teams_named == "MON"] <- "MON (last season 2004)"
    }
    
    # Reorder: put MON at the end (after WAS if both exist)
    if ("MON" %in% teams_named) {
      mon_idx <- which(teams_named == "MON")
      teams_named <- c(teams_named[-mon_idx], teams_named[mon_idx])
    }
    
    teams_named
  })
  
  unique_bats <- reactive({
    data <- current_data()
    bats <- sort(unique(data$bats[!is.na(data$bats) & data$bats != "U"]))
    # Create named vector: names = display labels, values = actual values for filtering
    bats_named <- c()
    if ("L" %in% bats) bats_named["L"] <- "L"
    if ("R" %in% bats) bats_named["R"] <- "R"
    if ("B" %in% bats) bats_named["S"] <- "B"
    bats_named
  })
  
  unique_throws <- reactive({
    data <- current_data()
    throws <- sort(unique(data$throws[!is.na(data$throws) & data$throws != "U"]))
    # Create named vector: names = display labels, values = actual values for filtering
    throws_named <- c()
    if ("L" %in% throws) throws_named["L"] <- "L"
    if ("R" %in% throws) throws_named["R"] <- "R"
    throws_named
  })
  
  # Create dropdown UIs using pickerInput
  output$years_dropdown_ui <- renderUI({
    choices <- year_choices()
    pickerInput(
      inputId = "years",
      label   = NULL,
      choices = choices,
      selected = choices,   # default: all years
      multiple = TRUE,
      options = list(
        `actions-box`          = TRUE,
        `live-search`          = TRUE,
        `selected-text-format` = "count > 8",
        `count-selected-text`  = "{0} years selected",
        `none-selected-text`   = "No years selected"
      )
    )
  })
  
  output$position_dropdown_ui <- renderUI({
    if (input$player_type == "hitters") {
      choices <- unique_positions()
      pickerInput(
        inputId = "positions",
        label   = NULL,
        choices = choices,
        selected = choices,
        multiple = TRUE,
        options = list(
          `actions-box`          = TRUE,
          `live-search`          = TRUE,
          `selected-text-format` = "count > 6",
          `count-selected-text`  = "{0} positions selected",
          `none-selected-text`   = "No positions selected"
        )
      )
    } else {
      pickerInput(
        inputId = "positions",
        label   = NULL,
        choices = c("P"),
        selected = "P",
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `live-search` = FALSE,
          `none-selected-text` = "P"
        )
      )
    }
  })
  
  output$teams_dropdown_ui <- renderUI({
    choices <- unique_teams()
    pickerInput(
      inputId = "teams",
      label   = NULL,
      choices = choices,
      selected = choices,   # default: all teams
      multiple = TRUE,
      options = list(
        `actions-box`          = TRUE,
        `live-search`          = TRUE,
        `selected-text-format` = "count > 10",
        `count-selected-text`  = "{0} teams selected",
        `none-selected-text`   = "No teams selected"
      )
    )
  })
  
  output$bats_filter <- renderUI({
    choices <- unique_bats()
    selected <- if (is.null(input$bats)) choices else input$bats
    checkboxGroupButtons("bats", NULL,
                        choices = choices,
                        selected = selected,
                        direction = "horizontal",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check")))
  })
  
  output$throws_filter <- renderUI({
    choices <- unique_throws()
    selected <- if (is.null(input$throws)) choices else input$throws
    checkboxGroupButtons("throws", NULL,
                        choices = choices,
                        selected = selected,
                        direction = "horizontal",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check")))
  })
  
  
  # Reset filters when player type changes
  observeEvent(input$player_type, {
    # Reset to defaults when switching
    if (input$player_type == "hitters") {
      choices <- unique_positions()
      updatePickerInput(session, "positions", 
                       choices = choices,
                       selected = choices,
                       options = list(
                         `actions-box`          = TRUE,
                         `live-search`          = TRUE,
                         `selected-text-format` = "count > 6",
                         `count-selected-text`  = "{0} positions selected",
                         `none-selected-text`   = "No positions selected"
                       ))
    } else {
      # For pitchers, force position to "P"
      updatePickerInput(session, "positions",
                        choices = c("P"),
                        selected = "P",
                        options = list(
                          `actions-box` = FALSE,
                          `live-search` = FALSE,
                          `none-selected-text` = "P"
                        ))
    }
    choices_teams <- unique_teams()
    updatePickerInput(session, "teams", 
                    choices = choices_teams,
                    selected = choices_teams,
                    options = list(
                      `actions-box`          = TRUE,
                      `live-search`          = TRUE,
                      `selected-text-format` = "count > 10",
                      `count-selected-text`  = "{0} teams selected",
                      `none-selected-text`   = "No teams selected"
                    ))
    # Bats and throws will update automatically via renderUI
  })
  
  # Prevent clearing "P" for pitchers
  observeEvent(input$positions, {
    if (input$player_type == "pitchers") {
      if (is.null(input$positions) || length(input$positions) == 0 || !("P" %in% input$positions)) {
        updatePickerInput(session, "positions", selected = "P")
      }
    }
  }, ignoreInit = TRUE)
  
  # Reset Search button - reset all filters to defaults
  observeEvent(input$reset_search, {
    # Reset years
    years_choices <- year_choices()
    updatePickerInput(session, "years",
                     choices = years_choices,
                     selected = years_choices,
                     options = list(
                       `actions-box`          = TRUE,
                       `live-search`          = TRUE,
                       `selected-text-format` = "count > 8",
                       `count-selected-text`  = "{0} years selected",
                       `none-selected-text`   = "No years selected"
                     ))
    
    # Reset positions
    if (input$player_type == "hitters") {
      pos_choices <- unique_positions()
      updatePickerInput(session, "positions",
                       choices = pos_choices,
                       selected = pos_choices,
                       options = list(
                         `actions-box`          = TRUE,
                         `live-search`          = TRUE,
                         `selected-text-format` = "count > 6",
                         `count-selected-text`  = "{0} positions selected",
                         `none-selected-text`   = "No positions selected"
                       ))
    } else {
      # For pitchers, reset to "P"
      updatePickerInput(session, "positions",
                        choices = c("P"),
                        selected = "P",
                        options = list(
                          `actions-box` = FALSE,
                          `live-search` = FALSE,
                          `none-selected-text` = "P"
                        ))
    }
    
    # Reset teams
    teams_choices <- unique_teams()
    updatePickerInput(session, "teams",
                     choices = teams_choices,
                     selected = teams_choices,
                     options = list(
                       `actions-box`          = TRUE,
                       `live-search`          = TRUE,
                       `selected-text-format` = "count > 10",
                       `count-selected-text`  = "{0} teams selected",
                       `none-selected-text`   = "No teams selected"
                     ))
    
    # Reset bats - get all available bats
    data <- current_data()
    bats <- sort(unique(data$bats[!is.na(data$bats) & data$bats != "U"]))
    updateCheckboxGroupButtons(session, "bats", selected = bats)
    
    # Reset throws - get all available throws
    throws <- sort(unique(data$throws[!is.na(data$throws) & data$throws != "U"]))
    updateCheckboxGroupButtons(session, "throws", selected = throws)
    
    # Reset stat selections
    stats <- available_stats()
    updateSelectInput(session, "stat1", 
                     choices = c("Choose Stat 1" = "", stats),
                     selected = "")
    updateSelectInput(session, "stat2", 
                     choices = c("Choose Stat 2" = "", stats),
                     selected = "")
    
    # Clear rankings by invalidating the generate button
    # This will make rankings_data() return NULL when stats are empty
    # We'll handle this in the ranking outputs by checking for empty stats
  })
  
  output$stat1_dropdown <- renderUI({
    stats <- available_stats()
    selectInput(
      "stat1",
      NULL,
      choices = c("Choose Stat 1" = "", stats),
      selected = "",
      width = "100%"
    )
  })
  
  output$stat2_dropdown <- renderUI({
    stats <- available_stats()
    selectInput(
      "stat2",
      NULL,
      choices = c("Choose Stat 2" = "", stats),
      selected = "",
      width = "100%"
    )
  })
  
  observe({
    if (!is.null(input$bats) && length(input$bats) == 0) {
      showNotification("You must have at least one batting hand selected.", type = "error", duration = NULL)
      # Will be reset by renderUI
    }
  })
  
  observe({
    if (!is.null(input$throws) && length(input$throws) == 0) {
      showNotification("You must have at least one throwing hand selected.", type = "error", duration = NULL)
      # Will be reset by renderUI
    }
  })
  
  # Process data when generate button is clicked
  rankings_data <- eventReactive(input$generate, {
    # Validate stat selection
    if (is.null(input$stat1) || input$stat1 == "" || 
        is.null(input$stat2) || input$stat2 == "") {
      showNotification("You must select two stats.", type = "error", duration = NULL)
      return(NULL)
    }
    if (input$stat1 == input$stat2) {
      showNotification("You must pick two separate stats.", type = "error", duration = NULL)
      return(NULL)
    }
    
    # Validate filters
    if (is.null(input$years) || length(input$years) == 0 ||
        is.null(input$teams) || length(input$teams) == 0 ||
        is.null(input$bats) || length(input$bats) == 0 ||
        is.null(input$throws) || length(input$throws) == 0) {
      showNotification("All filters must have at least one option selected.", type = "error", duration = NULL)
      return(NULL)
    }
    
    # Validate position filter only for hitters
    if (input$player_type == "hitters" && 
        (is.null(input$positions) || length(input$positions) == 0)) {
      showNotification("All filters must have at least one option selected.", type = "error", duration = NULL)
      return(NULL)
    }
    
    # Validate catcher-only stats: if selected, position filter must be only "C"
    if (input$player_type == "hitters") {
      stat1_is_catcher_only <- is_catcher_only_stat(input$stat1)
      stat2_is_catcher_only <- is_catcher_only_stat(input$stat2)
      
      if (stat1_is_catcher_only || stat2_is_catcher_only) {
        # Check if position filter is set to only "C"
        if (is.null(input$positions) || 
            length(input$positions) != 1 || 
            !("C" %in% input$positions)) {
          showNotification("Catcher-only stats (CS, CS%, PB/9) require the position filter to be set to only 'C' (Catcher).", type = "error", duration = NULL)
          return(NULL)
        }
      }
    }
    
    data <- current_data()
    selected_years <- as.numeric(input$years)
    
    # STEP 1: Filter by years only, then aggregate ALL stats from those years
    # (regardless of team/position filters - those only determine eligibility)
    data_by_years <- data %>%
      filter(yearID %in% selected_years)
    
    # Aggregate stats across selected years (includes ALL years, all teams)
    if (input$player_type == "hitters") {
      data_aggregated <- data_by_years %>%
        group_by(playerID, full_name) %>%
        summarise(
          # Counting stats
          R_bat = sum(R_bat, na.rm = TRUE),
          H_bat = sum(H_bat, na.rm = TRUE),
          X2B = sum(X2B, na.rm = TRUE),
          X3B = sum(X3B, na.rm = TRUE),
          HR_bat = sum(HR_bat, na.rm = TRUE),
          RBI = sum(RBI, na.rm = TRUE),
          SB = sum(SB, na.rm = TRUE),
          AB = sum(AB, na.rm = TRUE),
          BB_bat = sum(BB_bat, na.rm = TRUE),
          IBB_bat = sum(IBB_bat, na.rm = TRUE),
          HBP_bat = sum(HBP_bat, na.rm = TRUE),
          SF_bat = sum(SF_bat, na.rm = TRUE),
          SH_bat = sum(SH_bat, na.rm = TRUE),
          SO_bat = sum(SO_bat, na.rm = TRUE),
          CS = sum(CS, na.rm = TRUE),
          # Fielding stats
          PO = sum(PO, na.rm = TRUE),
          A = sum(A, na.rm = TRUE),
          E = sum(E, na.rm = TRUE),
          CS_fld = sum(CS_fld, na.rm = TRUE),
          SB_fld = sum(SB_fld, na.rm = TRUE),
          InnOuts = sum(InnOuts, na.rm = TRUE),
          PB = sum(PB, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        recalculate_batting_stats() %>%
        # Filter hitters: minimum 300 plate appearances in selected date range
        filter(PA >= 300)
    } else {
      data_aggregated <- data_by_years %>%
        group_by(playerID, full_name) %>%
        summarise(
          W = sum(W, na.rm = TRUE),
          G_pit = sum(G_pit, na.rm = TRUE),
          GS = sum(GS, na.rm = TRUE),
          CG = sum(CG, na.rm = TRUE),
          SHO = sum(SHO, na.rm = TRUE),
          SV = sum(SV, na.rm = TRUE),
          SO_pit = sum(SO_pit, na.rm = TRUE),
          IPouts = sum(IPouts, na.rm = TRUE),
          H_pit = sum(H_pit, na.rm = TRUE),
          ER = sum(ER, na.rm = TRUE),
          HR_pit = sum(HR_pit, na.rm = TRUE),
          BB_pit = sum(BB_pit, na.rm = TRUE),
          IBB_pit = sum(IBB_pit, na.rm = TRUE),
          BFP = sum(BFP, na.rm = TRUE),
          HBP_pit = sum(HBP_pit, na.rm = TRUE),
          SH_pit = sum(SH_pit, na.rm = TRUE),
          SF_pit = sum(SF_pit, na.rm = TRUE),
          L = sum(L, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        recalculate_pitching_stats() %>%
        # Filter pitchers: minimum 65 innings pitched in selected date range
        filter(IP >= 65)
    }
    
    # STEP 2: Determine eligibility based on filters (but don't affect stats)
    # Get list of eligible players based on team/position/bats/throws filters
    eligible_players <- data_by_years %>%
      # Filter by position (only for hitters, at least one year in range must match)
      { if (input$player_type == "hitters") {
          group_by(., playerID) %>%
          filter(any(primary_position %in% input$positions)) %>%
          ungroup()
        } else { . }
      } %>%
      # Filter by teams (at least one year in range must match)
      filter(!is.na(teams_this_year)) %>%
      {
        team_pattern <- paste0("(^|,)", paste(input$teams, collapse = "|"), "(,|$)")
        filter(., grepl(team_pattern, teams_this_year))
      } %>%
      # Filter by bats and throws (career-level, must match)
      filter(bats %in% input$bats, throws %in% input$throws) %>%
      distinct(playerID)
    
    # STEP 3: Filter aggregated results to only eligible players
    data_aggregated <- data_aggregated %>%
      filter(playerID %in% eligible_players$playerID)
    
    # Create rankings with percentiles (factored)
    stat1_lower_better <- is_lower_better(input$stat1, input$player_type)
    stat2_lower_better <- is_lower_better(input$stat2, input$player_type)

    ranking1 <- make_ranking(data_aggregated, input$stat1, stat1_lower_better) %>%
      mutate(plot_name = make_plot_name(full_name, playerID))

    ranking2 <- make_ranking(data_aggregated, input$stat2, stat2_lower_better) %>%
      mutate(plot_name = make_plot_name(full_name, playerID))
    
    # Store the stat selections that generated these results
    last_stat1(input$stat1)
    last_stat2(input$stat2)
    
    list(ranking1 = ranking1, ranking2 = ranking2)
  })
  
  # Create reactive for plot data (used by both observer and renderPlotly)
  plot_data_reactive <- reactive({
    # Check if we should show results (only if Generate was clicked, data exists, and stats match)
    show_results <- input$generate > 0 && 
                    !is.null(rankings_data()) && 
                    identical(input$stat1, last_stat1()) && 
                    identical(input$stat2, last_stat2())
    
    if (!show_results) return(NULL)
    
    # Get both rankings
    r1 <- rankings_data()$ranking1
    r2 <- rankings_data()$ranking2
    
    if (is.null(r1) || is.null(r2)) return(NULL)
    
    # Merge by plot_name to get both percentiles and values for each player (disambiguates Jose Ramirez)
    plot_data <- r1 %>%
      select(plot_name, full_name, value1 = value, percentile1 = percentile) %>%
      inner_join(
        r2 %>% select(plot_name, value2 = value, percentile2 = percentile),
        by = "plot_name"
      )
    
    if (nrow(plot_data) == 0) return(NULL)
    
    # Transform percentiles from 0-100 to -50 to +50
    plot_data$x <- plot_data$percentile1 - 50
    plot_data$y <- plot_data$percentile2 - 50
    
    # Format values for display
    plot_data$value1_formatted <- if_else(
      abs(plot_data$value1 - round(plot_data$value1)) < 1e-10,
      as.character(round(plot_data$value1)),
      sprintf("%.3f", plot_data$value1)
    )
    plot_data$value2_formatted <- if_else(
      abs(plot_data$value2 - round(plot_data$value2)) < 1e-10,
      as.character(round(plot_data$value2)),
      sprintf("%.3f", plot_data$value2)
    )
    
    # Get stat display names for hover text
    stat1_label <- get_stat_display_name(input$stat1)
    stat2_label <- get_stat_display_name(input$stat2)
    
    # Create hover text: Player name (400 weight), Stat1 value (percentile1%), Stat2 value (percentile2%) (300 weight)
    plot_data$hover_text <- paste0(
      "<span style='font-family: Lato, sans-serif; font-weight: 400;'>", plot_data$full_name, "</span><br>",
      "<span style='font-family: Lato, sans-serif; font-weight: 300;'>", 
      stat1_label, ": ", plot_data$value1_formatted, " (", sprintf("%.1f", plot_data$percentile1), "%)<br>",
      stat2_label, ": ", plot_data$value2_formatted, " (", sprintf("%.1f", plot_data$percentile2), "%)",
      "</span>"
    )
    
    plot_data
  })
  
  # Populate graph search dropdown AFTER Generate, using the same pool as the plot
  observeEvent(input$generate, {
    pd <- plot_data_reactive()
    if (is.null(pd) || nrow(pd) == 0) {
      updateSelectizeInput(session, "search_graph",
                           choices = character(0),
                           selected = character(0),
                           server = FALSE)
      return()
    }

    names_on_plot <- sort(unique(pd$plot_name))

    # keep selections that still exist
    current_sel <- input$search_graph
    if (is.null(current_sel)) current_sel <- character(0)
    current_sel <- intersect(current_sel, names_on_plot)

    updateSelectizeInput(session, "search_graph",
                         choices = names_on_plot,
                         selected = current_sel,
                         server = FALSE)
  }, ignoreInit = TRUE)

  # Deselect all button
  observeEvent(input$clear_search_graph, {
    updateSelectizeInput(session, "search_graph", selected = character(0))
  }, ignoreInit = TRUE)
  
  # Reactive to determine if results should be shown (used for conditional UI)
  show_results_reactive <- reactive({
    input$generate > 0 && 
    !is.null(rankings_data()) && 
    identical(input$stat1, last_stat1()) && 
    identical(input$stat2, last_stat2())
  })
  
  # Output for JavaScript to read show_results state
  output$show_results <- reactive({
    show_results_reactive()
  })
  outputOptions(output, "show_results", suspendWhenHidden = FALSE)
  
  # Create percentile scatter plot
  output$percentile_plot <- renderPlotly({
    # Check if we should show results (only if Generate was clicked, data exists, and stats match)
    show_results <- input$generate > 0 && 
                    !is.null(rankings_data()) && 
                    identical(input$stat1, last_stat1()) && 
                    identical(input$stat2, last_stat2())
    
    # Get stat display names for axis labels - only show when results should be displayed
    if (show_results) {
      stat1_label <- get_stat_display_name(input$stat1)
      stat2_label <- get_stat_display_name(input$stat2)
    } else {
      stat1_label <- NULL
      stat2_label <- NULL
    }
    
    # Initialize empty plot
    p <- plot_ly() %>%
      layout(
        hovermode = "closest",
        xaxis = list(
          title = list(text = ""),
          range = c(-55, 55),
          showgrid = TRUE,
          gridcolor = "white",
          gridwidth = 0.5,
          showticklabels = FALSE,
          tickmode = "array",
          tickvals = c(-50, 0, 50),
          ticklen = 0,
          zeroline = TRUE,
          zerolinecolor = "white",
          zerolinewidth = 2.5
        ),
        yaxis = list(
          title = list(text = ""),
          range = c(-55, 55),
          showgrid = TRUE,
          gridcolor = "white",
          gridwidth = 0.5,
          showticklabels = FALSE,
          tickmode = "array",
          tickvals = c(-50, 0, 50),
          ticklen = 0,
          zeroline = TRUE,
          zerolinecolor = "white",
          zerolinewidth = 2.5
        ),
        plot_bgcolor = "#130347",
        paper_bgcolor = "#130347",
        showlegend = FALSE,
        margin = list(l = 110, r = 60, t = 40, b = 90),  # Add padding around the plot (increased left for y-axis labels, increased bottom for x-axis labels)
        font = list(
          family = "Lato, sans-serif",
          size = 12,
          color = "#ffffff"
        ),
        annotations = c(
          # Left-side percentile labels (always shown)
          list(
            list(
              text = "max",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = 50.3,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "50th percentile",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = 0.3,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "min",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = -49.7,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            )
          ),
          # Bottom (x-axis) labels as paper-space annotations
          list(
            list(
              text = "min",
              xref = "x",
              yref = "paper",
              x = -50,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "50th percentile",
              xref = "x",
              yref = "paper",
              x = 0,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "max",
              xref = "x",
              yref = "paper",
              x = 50,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            )
          ),
          # Blank graph message (only when no results)
          if (!show_results) {
            list(
              list(
                text = "Graph will populate after a search is made",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                xanchor = "center",
                yanchor = "middle",
                showarrow = FALSE,
                font = list(size = 16, color = "#07bcff"),
                bgcolor = "rgba(36, 3, 104, 0.9)",
                bordercolor = "#07bcff",
                borderwidth = 1,
                borderpad = 10
              )
            )
          } else {
            list()
          }
        )
      )
    
    # Only add points if we should show results
    if (show_results) {
      
      # Get plot data from reactive
      plot_data <- plot_data_reactive()
      
      if (!is.null(plot_data) && nrow(plot_data) > 0) {
        
        # Check if players are selected (multi-select)
        sel <- input$search_graph
        if (is.null(sel)) sel <- character(0)
        
        selected_data <- plot_data %>% filter(plot_name %in% sel)
        unselected_data <- plot_data %>% filter(!(plot_name %in% sel))
        
        # Add unselected points first (so selected appear on top)
        if (nrow(unselected_data) > 0) {
          p <- p %>%
            add_trace(
              x = ~x,
              y = ~y,
              text = ~hover_text,
              hoverinfo = "text",
              type = "scatter",
              mode = "markers",
              cliponaxis = FALSE,
              marker = list(
                size = 8,
                color = "#07bcff",
                opacity = 1.0,
                line = list(color = "#07bcff", width = 0)
              ),
              hoverlabel = list(
                bgcolor = "#dc01ff",  # Magenta color for hover box (matches hover state)
                bordercolor = "#dc01ff",
                font = list(color = "white")
              ),
              data = unselected_data,
              name = "Players"
            )
        }
        
        # Add selected/highlighted points with different color
        if (nrow(selected_data) > 0) {
          p <- p %>%
            add_trace(
              x = ~x,
              y = ~y,
              text = ~hover_text,
              hoverinfo = "text",
              type = "scatter",
              mode = "markers",
              cliponaxis = FALSE,
              marker = list(
                size = 10,
                color = "#dc01ff",  # Magenta color for selected
                opacity = 1.0,
                line = list(color = "#dc01ff", width = 2)
              ),
              hoverlabel = list(
                bgcolor = "#dc01ff",  # Magenta color for hover box
                bordercolor = "#dc01ff",
                font = list(color = "white")
              ),
              data = selected_data,
              name = "Selected"
            )
          
          # Add persistent info boxes (annotations) for selected players
          ann <- lapply(seq_len(nrow(selected_data)), function(i) {
            list(
              x = selected_data$x[i],
              y = selected_data$y[i],
              text = selected_data$hover_text[i],
              xref = "x", yref = "y",
              showarrow = TRUE,
              arrowhead = 2,
              ax = 40, ay = -40,
                bgcolor = "#dc01ff",
                bordercolor = "#dc01ff",
              font = list(color = "white", size = 12),
              align = "left"
            )
          })
          
          # Create custom axis labels
          custom_axis_labels <- list(
            # X label (bottom)
            list(
              text = stat1_label,
              xref = "paper", yref = "paper",
              x = 0.5, y = -0.10,
              xanchor = "center", yanchor = "top",
              showarrow = FALSE,
              font = list(size = 16, color = "white", family = "Lato, sans-serif", weight = 100)
            ),
            # Y label (left, rotated)
            list(
              text = stat2_label,
              xref = "paper", yref = "paper",
              x = -0.085, y = 0.5,
              xanchor = "right", yanchor = "middle",
              textangle = -90,
              showarrow = FALSE,
              font = list(size = 16, color = "white", family = "Lato, sans-serif", weight = 100)
            )
          )
          
          # Combine base annotations (percentile labels) with selected player annotations
          base_ann <- list(
            # Left-side labels
            list(
              text = "max",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = 50.3,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "50th percentile",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = 0.3,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "min",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = -49.7,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            # Bottom labels
            list(
              text = "min",
              xref = "x",
              yref = "paper",
              x = -50,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "50th percentile",
              xref = "x",
              yref = "paper",
              x = 0,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "max",
              xref = "x",
              yref = "paper",
              x = 50,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            )
          )
          # Merge all annotations: custom axis labels + base percentile labels + selected player annotations
          all_annotations <- c(custom_axis_labels, base_ann, ann)
          p <- p %>% layout(annotations = all_annotations)
        } else {
          # No selected players -> keep base percentile labels and custom axis labels
          custom_axis_labels <- list(
            # X label (bottom)
            list(
              text = stat1_label,
              xref = "paper", yref = "paper",
              x = 0.5, y = -0.10,
              xanchor = "center", yanchor = "top",
              showarrow = FALSE,
              font = list(size = 16, color = "white", family = "Lato, sans-serif", weight = 100)
            ),
            # Y label (left, rotated)
            list(
              text = stat2_label,
              xref = "paper", yref = "paper",
              x = -0.085, y = 0.5,
              xanchor = "right", yanchor = "middle",
              textangle = -90,
              showarrow = FALSE,
              font = list(size = 16, color = "white", family = "Lato, sans-serif", weight = 100)
            )
          )
          base_ann <- list(
            # Left-side labels
            list(
              text = "max",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = 50.3,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "50th percentile",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = 0.3,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "min",
              xref = "paper",
              yref = "y",
              x = -0.009,
              y = -49.7,
              xanchor = "right",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            # Bottom labels
            list(
              text = "min",
              xref = "x",
              yref = "paper",
              x = -50,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "50th percentile",
              xref = "x",
              yref = "paper",
              x = 0,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            ),
            list(
              text = "max",
              xref = "x",
              yref = "paper",
              x = 50,
              y = -0.013,
              yanchor = "top",
              showarrow = FALSE,
              font = list(color = "white", size = 9, family = "Lato, sans-serif", weight = 100),
              cliponaxis = FALSE
            )
          )
          # Merge custom axis labels with base percentile labels
          all_annotations <- c(custom_axis_labels, base_ann)
          p <- p %>% layout(annotations = all_annotations)
        }
        
        # If no selected points but we have data, add all as unselected
        if (nrow(selected_data) == 0 && nrow(unselected_data) > 0) {
          # Already added above
        } else if (nrow(selected_data) == 0 && nrow(unselected_data) == 0 && nrow(plot_data) > 0) {
          # Fallback: add all points if split didn't work
          p <- p %>%
            add_trace(
              x = ~x,
              y = ~y,
              text = ~hover_text,
              hoverinfo = "text",
              type = "scatter",
              mode = "markers",
              cliponaxis = FALSE,
              marker = list(
                size = 8,
                color = "#07bcff",
                opacity = 1.0,
                line = list(color = "#07bcff", width = 0)
              ),
              hoverlabel = list(
                bgcolor = "#dc01ff",  # Magenta color for hover box
                bordercolor = "#dc01ff",
                font = list(color = "white")
              ),
              data = plot_data,
              name = "Players"
            )
        }
      }
    }
    
    # Configure modebar: keep download, zoom, pan, reset; remove other tools
    p <- p %>% config(
      displayModeBar = TRUE,     # always show the toolbar
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "select2d",
        "lasso2d",
        "autoScale2d",
        "zoom2d",                # removes zoom-rectangle tool, keeps zoom in/out buttons
        "hoverClosestCartesian", # removes the toggle button
        "hoverCompareCartesian", # removes the toggle button
        "toggleSpikelines"
      )
    )
    
    # Return the plot (blank if show_results is FALSE, with data if TRUE)
    p
  })
  
  # Display rankings
  output$stat1_title <- renderText({
    if (!is.null(rankings_data()) && !is.null(input$stat1) && input$stat1 != "") {
      display_name <- get_stat_display_name(input$stat1)
      paste0("Ranking by ", display_name)
    } else {
      "Ranking by Stat 1"
    }
  })
  
  output$stat2_title <- renderText({
    if (!is.null(rankings_data()) && !is.null(input$stat2) && input$stat2 != "") {
      display_name <- get_stat_display_name(input$stat2)
      paste0("Ranking by ", display_name)
    } else {
      "Ranking by Stat 2"
    }
  })
  
  output$ranking1 <- renderTable({
    # Return empty table if stats are not selected or rankings_data is NULL
    if (is.null(input$stat1) || input$stat1 == "" || 
        is.null(input$stat2) || input$stat2 == "" ||
        is.null(rankings_data())) {
      return(data.frame(Rank = integer(), Player = character(), Stat = character(), Percentile = character()))
    }
    r1 <- rankings_data()$ranking1
    
    # Filter by search term if provided
    if (!is.null(input$search1) && input$search1 != "") {
      search_term <- tolower(trimws(input$search1))
      r1 <- r1 %>%
        filter(grepl(search_term, tolower(full_name), fixed = FALSE))
    }
    
    # Check if dataframe is empty after filtering
    if (nrow(r1) == 0) {
      return(data.frame(Rank = integer(), Player = character(), Stat = character(), Percentile = character()))
    }
    
    # Format values
    r1$value_formatted <- if_else(
      abs(r1$value - round(r1$value)) < 1e-10,
      as.character(round(r1$value)),
      sprintf("%.3f", r1$value)
    )
    # Format percentile (round to 1 decimal place)
    r1$percentile_formatted <- sprintf("%.1f", r1$percentile)
    result <- r1 %>%
      select(Rank = rank, Player = full_name, value = value_formatted, Percentile = percentile_formatted)
    names(result)[3] <- get_stat_display_name(input$stat1)
    result
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  output$ranking2 <- renderTable({
    # Return empty table if stats are not selected or rankings_data is NULL
    if (is.null(input$stat1) || input$stat1 == "" || 
        is.null(input$stat2) || input$stat2 == "" ||
        is.null(rankings_data())) {
      return(data.frame(Rank = integer(), Player = character(), Stat = character(), Percentile = character()))
    }
    r2 <- rankings_data()$ranking2
    
    # Filter by search term if provided
    if (!is.null(input$search2) && input$search2 != "") {
      search_term <- tolower(trimws(input$search2))
      r2 <- r2 %>%
        filter(grepl(search_term, tolower(full_name), fixed = FALSE))
    }
    
    # Check if dataframe is empty after filtering
    if (nrow(r2) == 0) {
      return(data.frame(Rank = integer(), Player = character(), Stat = character(), Percentile = character()))
    }
    
    # Format values
    r2$value_formatted <- if_else(
      abs(r2$value - round(r2$value)) < 1e-10,
      as.character(round(r2$value)),
      sprintf("%.3f", r2$value)
    )
    # Format percentile (round to 1 decimal place)
    r2$percentile_formatted <- sprintf("%.1f", r2$percentile)
    result <- r2 %>%
      select(Rank = rank, Player = full_name, value = value_formatted, Percentile = percentile_formatted)
    names(result)[3] <- get_stat_display_name(input$stat2)
    result
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # Display total player count for each ranking
  output$pool_count1 <- renderText({
    # Return empty if stats are not selected or rankings_data is NULL
    if (is.null(input$stat1) || input$stat1 == "" || 
        is.null(input$stat2) || input$stat2 == "" ||
        is.null(rankings_data())) {
      return("")
    }
    total_count <- nrow(rankings_data()$ranking1)
    paste0(total_count, " players in selected pool, minimum 300 PAs or 65 IP")
  })
  
  output$pool_count2 <- renderText({
    # Return empty if stats are not selected or rankings_data is NULL
    if (is.null(input$stat1) || input$stat1 == "" || 
        is.null(input$stat2) || input$stat2 == "" ||
        is.null(rankings_data())) {
      return("")
    }
    total_count <- nrow(rankings_data()$ranking2)
    paste0(total_count, " players in selected pool, minimum 300 PAs or 65 IP")
  })
}

# RUN APP

shinyApp(ui = ui, server = server)

