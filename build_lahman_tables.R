# build_lahman_tables.R
# Create separate per-year datasets for hitters and pitchers from Lahman
# One row per player per year (years >= 2000) with all counting stats
# Rate stats are calculated for each year, but can be recalculated from 
# counting stats when multiple years are combined in the main program
# Inclusion criteria:
#   - Hitters: at least 500 career plate appearances with PA since 2000
#   - Pitchers: at least 100 career innings pitched with IP since 2000
# Note: Two-way players (meeting both criteria) appear in both datasets

library(Lahman)
library(dplyr)
library(tidyr)
library(stringr)

min_year <- 2000
min_career_PA <- 500  # For hitters
min_career_IP <- 100  # For pitchers

# HELPER FUNCTIONS

# Calculate plate appearances from component stats
calculate_PA <- function(AB, BB, HBP, SF, SH) {
  coalesce(AB, 0) + coalesce(BB, 0) + coalesce(HBP, 0) + 
  coalesce(SF, 0) + coalesce(SH, 0)
}

# Calculate innings pitched from IPouts
calculate_IP <- function(IPouts) {
  if_else(!is.na(IPouts) & IPouts > 0, IPouts / 3, NA_real_)
}

# Map historical teamIDs to current standard abbreviations
map_teamID_to_standard <- function(teamID) {
  case_when(
    # Los Angeles Dodgers
    teamID == "LAN" ~ "LAD",
    # New York Yankees
    teamID == "NYA" ~ "NYY",
    # New York Mets
    teamID == "NYN" ~ "NYM",
    # Chicago White Sox
    teamID == "CHA" ~ "CWS",
    # Chicago Cubs
    teamID == "CHN" ~ "CHC",
    # Kansas City Royals
    teamID == "KCA" ~ "KCR",
    # Tampa Bay Rays
    teamID == "TBA" ~ "TBR",
    # Los Angeles Angels
    teamID == "ANA" ~ "LAA",
    # Miami Marlins (formerly Florida)
    teamID == "FLO" ~ "MIA",
    # San Diego Padres
    teamID == "SDN" ~ "SDP",
    # San Francisco Giants
    teamID == "SFN" ~ "SFG",
    # St. Louis Cardinals
    teamID == "SLN" ~ "STL",
    # Keep MON as MON (Montreal Expos, separate from WAS/WSN Nationals)
    # Keep all other teamIDs as-is (they should already be standard)
    TRUE ~ teamID
  )
}

# Add derived batting stats to a dataset
add_batting_stats <- function(df) {
  df %>%
    mutate(
      # Supporting data (for calculations, not user-visible)
      PA = calculate_PA(.data$AB, .data$BB_bat, .data$HBP_bat, .data$SF_bat, .data$SH_bat),
      singles = if_else(!is.na(.data$H_bat), 
                        .data$H_bat - .data$X2B - .data$X3B - .data$HR_bat, 
                        NA_integer_),
      
      # User-visible calculated stats
      # Extra Base Hits
      XBH = .data$X2B + .data$X3B + .data$HR_bat,
      
      # Rate stats
      AVG = if_else(!is.na(.data$AB) & .data$AB > 0, .data$H_bat / .data$AB, NA_real_),
      OBP = if_else((coalesce(.data$AB, 0) + coalesce(.data$BB_bat, 0) + 
                     coalesce(.data$HBP_bat, 0) + coalesce(.data$SF_bat, 0)) > 0,
                    (coalesce(.data$H_bat, 0) + coalesce(.data$BB_bat, 0) + 
                     coalesce(.data$HBP_bat, 0)) / 
                    (coalesce(.data$AB, 0) + coalesce(.data$BB_bat, 0) + 
                     coalesce(.data$HBP_bat, 0) + coalesce(.data$SF_bat, 0)),
                    NA_real_),
      SLG = if_else(!is.na(.data$AB) & .data$AB > 0,
                    (coalesce(singles, 0) + 2 * coalesce(.data$X2B, 0) + 
                     3 * coalesce(.data$X3B, 0) + 4 * coalesce(.data$HR_bat, 0)) / .data$AB,
                    NA_real_),
      OPS = OBP + SLG,
      
      # Percentage stats (multiply by 100 for percentage)
      HR_pct = if_else(PA > 0, (.data$HR_bat / PA) * 100, NA_real_),
      BB_pct = if_else(PA > 0, (.data$BB_bat / PA) * 100, NA_real_),
      K_pct = if_else(PA > 0, (.data$SO_bat / PA) * 100, NA_real_),
      SB_pct = if_else((coalesce(.data$SB, 0) + coalesce(.data$CS, 0)) > 0,
                       (.data$SB / (coalesce(.data$SB, 0) + coalesce(.data$CS, 0))) * 100,
                       NA_real_),
      # Walk to Strikeout Ratio (using only non-intentional walks)
      BB_K = if_else(.data$SO_bat > 0, 
                     .data$BB_bat / .data$SO_bat, 
                     NA_real_)
    )
}

# Add derived pitching stats to a dataset
add_pitching_stats <- function(df) {
  df %>%
    mutate(
      # Supporting data (for calculations, not user-visible)
      IP = calculate_IP(.data$IPouts),
      
      # User-visible calculated stats
      # Rate stats
      ERA = if_else(!is.na(IP) & IP > 0,
                    (coalesce(.data$ER, 0) * 9) / IP,
                    NA_real_),
      WHIP = if_else(!is.na(IP) & IP > 0,
                     (.data$BB_pit + coalesce(.data$H_pit, 0)) / IP,
                     NA_real_),
      # Per 9 innings stats
      K_9 = if_else(!is.na(IP) & IP > 0,
                    (.data$SO_pit * 9) / IP,
                    NA_real_),
      BB_9 = if_else(!is.na(IP) & IP > 0,
                     (.data$BB_pit * 9) / IP,
                     NA_real_),
      HR_9 = if_else(!is.na(IP) & IP > 0,
                     (.data$HR_pit * 9) / IP,
                     NA_real_),
      # Batting Average Against = H / (BFP - BB_pit - HBP - SH - SF)
      BAA = if_else((coalesce(.data$BFP, 0) - .data$BB_pit - 
                     coalesce(.data$HBP_pit, 0) - coalesce(.data$SH_pit, 0) - 
                     coalesce(.data$SF_pit, 0)) > 0,
                    .data$H_pit / (coalesce(.data$BFP, 0) - .data$BB_pit - 
                                   coalesce(.data$HBP_pit, 0) - coalesce(.data$SH_pit, 0) - 
                                   coalesce(.data$SF_pit, 0)),
                    NA_real_),
      # Strikeout to Walk Ratio (using only non-intentional walks)
      K_BB = if_else(.data$BB_pit > 0, .data$SO_pit / .data$BB_pit, NA_real_),
      # Winning Percentage
      W_pct = if_else((coalesce(.data$W, 0) + coalesce(.data$L, 0)) > 0,
                      .data$W / (coalesce(.data$W, 0) + coalesce(.data$L, 0)),
                      NA_real_)
    )
}

# SETUP

# create processed data folder if it doesn't exist
if (!dir.exists("data_processed")) {
  dir.create("data_processed")
}

# load core Lahman tables
people      <- Lahman::People
batting_raw <- Lahman::Batting
pitching_raw<- Lahman::Pitching
apps_raw    <- Lahman::Appearances
fielding_raw <- Lahman::Fielding

# IDENTIFY ELIGIBLE PLAYERS

# Calculate career plate appearances for all players (across all years)
career_PA <- batting_raw %>%
  group_by(playerID) %>%
  summarise(
    career_PA = sum(calculate_PA(AB, BB, HBP, SF, SH), na.rm = TRUE),
    .groups = "drop"
  )

# Calculate career innings pitched for all players (across all years)
career_IP <- pitching_raw %>%
  group_by(playerID) %>%
  summarise(
    career_IP = sum(calculate_IP(IPouts), na.rm = TRUE),
    .groups = "drop"
  )

# Identify players with activity since 2000
# For hitters: need at least some PA since 2000
hitters_with_2000_plus <- batting_raw %>%
  filter(yearID >= min_year) %>%
  group_by(playerID) %>%
  summarise(
    PA_since_2000 = sum(calculate_PA(AB, BB, HBP, SF, SH), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(PA_since_2000 > 0) %>%
  pull(playerID)

# For pitchers: need at least some IP since 2000
pitchers_with_2000_plus <- pitching_raw %>%
  filter(yearID >= min_year) %>%
  group_by(playerID) %>%
  summarise(
    IP_since_2000 = sum(calculate_IP(IPouts), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(IP_since_2000 > 0) %>%
  pull(playerID)

# Separate eligible player lists for hitters and pitchers
eligible_hitters <- career_PA %>%
  mutate(
    career_PA = coalesce(career_PA, 0),
    meets_hitting_threshold = career_PA >= min_career_PA,
    has_pa_since_2000 = playerID %in% hitters_with_2000_plus,
    qualifies = meets_hitting_threshold & has_pa_since_2000
  ) %>%
  filter(qualifies) %>%
  pull(playerID)

eligible_pitchers <- career_IP %>%
  mutate(
    career_IP = coalesce(career_IP, 0),
    meets_pitching_threshold = career_IP >= min_career_IP,
    has_ip_since_2000 = playerID %in% pitchers_with_2000_plus,
    qualifies = meets_pitching_threshold & has_ip_since_2000
  ) %>%
  filter(qualifies) %>%
  pull(playerID)

cat("Found", length(eligible_hitters), "hitters (", 
    min_career_PA, "+ career PA with PA since", min_year, ")\n")
cat("Found", length(eligible_pitchers), "pitchers (", 
    min_career_IP, "+ career IP with IP since", min_year, ")\n")
cat("Note: Players meeting both criteria appear in both datasets\n")

# Get batting/throwing handedness
# (Lahman doesn't track year-by-year changes in handedness)
player_info <- people %>%
  select(playerID, nameFirst, nameLast, bats, throws) %>%
  mutate(
    full_name = paste(nameFirst, nameLast),
    bats = if_else(is.na(bats) | bats == "", "U", bats),  # U for unknown
    throws = if_else(is.na(throws) | throws == "", "U", throws)
  )

# HELPER FUNCTION: Get player-year metadata (positions, teams, info)

get_player_year_metadata <- function(player_list) {
  # Get all years each player appeared (since 2000)
  player_years <- apps_raw %>%
    filter(playerID %in% player_list,
           yearID >= min_year) %>%
    distinct(playerID, yearID) %>%
    arrange(playerID, yearID)
  
  # Determine primary position for each player-year
  # Use Appearances table but exclude non-position columns (G_all, G_batting, G_defense, G_ph, G_pr)
  # Map lowercase position codes to standard uppercase abbreviations
  primary_pos_by_year <- apps_raw %>%
    filter(playerID %in% player_list,
           yearID >= min_year) %>%
    select(playerID, yearID, teamID, G_c, G_1b, G_2b, G_3b, G_ss, G_lf, G_cf, G_rf, G_dh, G_p) %>%
    pivot_longer(
      cols = c(G_c, G_1b, G_2b, G_3b, G_ss, G_lf, G_cf, G_rf, G_dh, G_p),
      names_to = "pos",
      values_to = "G"
    ) %>%
    mutate(
      pos = str_remove(pos, "^G_"),
      # Map lowercase position codes to standard uppercase abbreviations
      pos = case_when(
        pos == "c" ~ "C",
        pos == "1b" ~ "1B",
        pos == "2b" ~ "2B",
        pos == "3b" ~ "3B",
        pos == "ss" ~ "SS",
        pos == "lf" ~ "LF",
        pos == "cf" ~ "CF",
        pos == "rf" ~ "RF",
        pos == "dh" ~ "DH",
        pos == "p" ~ "P",
        TRUE ~ toupper(pos)
      )
    ) %>%
    filter(!is.na(G), G > 0) %>%
    group_by(playerID, yearID, pos) %>%
    summarise(
      games_at_pos = sum(G, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(playerID, yearID) %>%
    slice_max(order_by = games_at_pos, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(playerID, yearID, primary_position = pos)
  
  # Get teams played for each year
  teams_by_year <- apps_raw %>%
    filter(playerID %in% player_list,
           yearID >= min_year) %>%
    group_by(playerID, yearID) %>%
    summarise(
      teams_this_year = paste(sort(unique(teamID)), collapse = ","),
      .groups = "drop"
    ) %>%
    # Map historical teamIDs to standard abbreviations
    mutate(
      teams_this_year = sapply(teams_this_year, function(x) {
        team_ids <- strsplit(x, ",")[[1]]
        mapped_ids <- map_teamID_to_standard(team_ids)
        paste(sort(unique(mapped_ids)), collapse = ",")
      })
    )
  
  list(
    player_years = player_years,
    primary_pos = primary_pos_by_year,
    teams = teams_by_year
  )
}

# HITTERS DATASET
# One row per hitter per year (only years >= 2000)
# Includes: batting stats + fielding stats

# Get metadata for hitters
hitter_metadata <- get_player_year_metadata(eligible_hitters)

# Batting stats per player-year (only years >= 2000)
# USER-VISIBLE STATS: R, H, X2B, X3B, HR, RBI, SB, XBH, AVG, OBP, SLG, OPS, HR_pct, BB_pct, K_pct, SB_pct, BB_bat, BB_K
# Supporting data: G, AB, BB, IBB, CS, HBP, SH, SF, GIDP, PA, singles, SO
batting_year <- batting_raw %>%
  filter(playerID %in% eligible_hitters,
         yearID >= min_year) %>%
  group_by(playerID, yearID) %>%
  summarise(
    # Supporting data (not user-visible)
    G_bat = sum(G, na.rm = TRUE),
    AB = sum(AB, na.rm = TRUE),
    BB_bat = sum(BB, na.rm = TRUE),
    IBB_bat = sum(IBB, na.rm = TRUE),
    CS = sum(CS, na.rm = TRUE),
    HBP_bat = sum(HBP, na.rm = TRUE),
    SH_bat = sum(SH, na.rm = TRUE),
    SF_bat = sum(SF, na.rm = TRUE),
    GIDP_bat = sum(GIDP, na.rm = TRUE),
    SO_bat = sum(SO, na.rm = TRUE),
    # User-visible stats
    R_bat = sum(R, na.rm = TRUE),
    H_bat = sum(H, na.rm = TRUE),
    X2B = sum(X2B, na.rm = TRUE),
    X3B = sum(X3B, na.rm = TRUE),
    HR_bat = sum(HR, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    SB = sum(SB, na.rm = TRUE),
    .groups = "drop"
  )

# Fielding stats per player-year (only years >= 2000) - for hitters only
# USER-VISIBLE STATS: FPCT (all players), CS_fld (catchers), CS_pct (catchers), PB_9 (catchers)
# Supporting data (for calculations): G_fld, GS_fld, PO, A, E, SB_fld, CS_fld, InnOuts, PB
fielding_year <- fielding_raw %>%
  filter(playerID %in% eligible_hitters,
         yearID >= min_year) %>%
  group_by(playerID, yearID) %>%
  summarise(
    # Supporting data (not user-visible)
    G_fld = sum(G, na.rm = TRUE),
    GS_fld = sum(GS, na.rm = TRUE),
    PO = sum(PO, na.rm = TRUE),
    A = sum(A, na.rm = TRUE),
    E = sum(E, na.rm = TRUE),
    SB_fld = sum(SB, na.rm = TRUE),
    CS_fld = sum(CS, na.rm = TRUE),
    InnOuts = sum(InnOuts, na.rm = TRUE),
    PB = sum(PB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # User-visible calculated stats
    # Fielding Percentage = (PO + A) / (PO + A + E) - applies to all players
    FPCT = if_else((PO + A + E) > 0, (PO + A) / (PO + A + E), NA_real_),
    # CS% for catchers = CS / (CS + SB)
    CS_pct = if_else((CS_fld + SB_fld) > 0, CS_fld / (CS_fld + SB_fld), NA_real_),
    # PB/9 for catchers = (PB * 9) / (InnOuts / 3) = (PB * 27) / InnOuts
    # Note: If PB is 0, PB_9 will correctly be 0 (not NA)
    PB_9 = if_else(InnOuts > 0, (PB * 27) / InnOuts, NA_real_)
  )

# Build the hitters by-year dataset
hitters_by_year <- hitter_metadata$player_years %>%
  left_join(batting_year, by = c("playerID", "yearID")) %>%
  left_join(fielding_year, by = c("playerID", "yearID"), suffix = c("", "_fld")) %>%
  left_join(hitter_metadata$primary_pos, by = c("playerID", "yearID")) %>%
  left_join(hitter_metadata$teams, by = c("playerID", "yearID")) %>%
  left_join(player_info, by = "playerID") %>%
  arrange(playerID, yearID) %>%
  add_batting_stats()

cat("\nHitters dataset:", nrow(hitters_by_year), "rows\n")
cat("Note: Rate stats are calculated for each year, but can be recalculated from\n")
cat("      counting stats when multiple years are combined in the main program.\n")

# PITCHERS DATASET
# One row per pitcher per year (only years >= 2000)
# Includes: pitching stats only (no batting/fielding)

# Get metadata for pitchers
pitcher_metadata <- get_player_year_metadata(eligible_pitchers)

# Pitching stats per player-year (only years >= 2000)
# USER-VISIBLE STATS: W, G, GS, CG, SHO, SV, IP, SO, ERA, WHIP, K_9, BB_9, HR_9, BAA, K_BB, W_pct
# Supporting data: IPouts, H, ER, HR, BB, IBB, BFP, R, SH, SF, HBP, WP, BK, L
pitching_year <- pitching_raw %>%
  filter(playerID %in% eligible_pitchers,
         yearID >= min_year) %>%
  group_by(playerID, yearID) %>%
  summarise(
    # Supporting data (not user-visible)
    IPouts = sum(IPouts, na.rm = TRUE),
    H_pit = sum(H, na.rm = TRUE),
    ER = sum(ER, na.rm = TRUE),
    HR_pit = sum(HR, na.rm = TRUE),
    BB_pit = sum(BB, na.rm = TRUE),
    IBB_pit = sum(IBB, na.rm = TRUE),
    BFP = sum(BFP, na.rm = TRUE),
    R_pit = sum(R, na.rm = TRUE),
    SH_pit = sum(SH, na.rm = TRUE),
    SF_pit = sum(SF, na.rm = TRUE),
    HBP_pit = sum(HBP, na.rm = TRUE),
    WP = sum(WP, na.rm = TRUE),
    BK = sum(BK, na.rm = TRUE),
    L = sum(L, na.rm = TRUE),
    # User-visible stats
    W = sum(W, na.rm = TRUE),
    G_pit = sum(G, na.rm = TRUE),
    GS = sum(GS, na.rm = TRUE),
    CG = sum(CG, na.rm = TRUE),
    SHO = sum(SHO, na.rm = TRUE),
    SV = sum(SV, na.rm = TRUE),
    SO_pit = sum(SO, na.rm = TRUE),
    .groups = "drop"
  )

# Build the pitchers by-year dataset
pitchers_by_year <- pitcher_metadata$player_years %>%
  left_join(pitching_year, by = c("playerID", "yearID")) %>%
  left_join(pitcher_metadata$primary_pos, by = c("playerID", "yearID")) %>%
  left_join(pitcher_metadata$teams, by = c("playerID", "yearID")) %>%
  left_join(player_info, by = "playerID") %>%
  arrange(playerID, yearID) %>%
  add_pitching_stats()

cat("\nPitchers dataset:", nrow(pitchers_by_year), "rows\n")
cat("Note: Rate stats are calculated for each year, but can be recalculated from\n")
cat("      counting stats when multiple years are combined in the main program.\n")

# SAVE DATASETS

saveRDS(hitters_by_year,
        file = "data_processed/hitters_by_year.rds")

saveRDS(pitchers_by_year,
        file = "data_processed/pitchers_by_year.rds")

cat("\nDatasets saved successfully!\n")
cat("  - hitters_by_year.rds:", nrow(hitters_by_year), "rows\n")
cat("    Contains one row per hitter per year (years >= 2000)\n")
cat("    Includes: batting stats + fielding stats\n")
cat("  - pitchers_by_year.rds:", nrow(pitchers_by_year), "rows\n")
cat("    Contains one row per pitcher per year (years >= 2000)\n")
cat("    Includes: pitching stats only\n")
cat("\nNote: Two-way players (meeting both criteria) appear in both datasets\n")
cat("      All counting stats included for flexible year-range calculations\n")
