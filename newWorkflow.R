library(dplyr)
library(httr)
library(jsonlite)
library(readxl)
library(hoopR)
library(DBI)
library(RSQLite)
library(oddsapiR)

api_key <- toa_key()

# usethis::edit_r_environ()

fetch_events <- function(api_key, sport)

# Function to fetch odds from Odds API
  # The sport key obtained from calling the /sports endpoint. upcoming is always valid, returning any live games as well as the next 8 upcoming games across all sports
fetch_odds <- function(api_key, sport = "basketball_ncaab", region = "us", market = "spreads", date_from = NULL, date_to = NULL) {
  url <- paste0("https://api.the-odds-api.com/v4/sports/", sport, "/odds/")
  params <- list(
    apiKey = api_key,
    regions = region,
    markets = market,
    oddsFormat = "american",
    bookmakers = "fanduel",
    commenceTimeTo = 
    # might want to add commenceTimeTo, commenceTimeFrom, bookmakers, eventIDs
  )
  response <- GET(url, query = params)
  odds_data <- fromJSON(content(response, "text"))
  return(odds_data)
  # odds_data is a comma separated list of games - only listing 7 right now - maybe that's because other odds not released?
  # if (response$status_code == 200) {
  #   odds_data <- fromJSON(content(response, "text"))
  #   odds_df <- do.call(rbind, lapply(odds_data, as.data.frame))
  #   return(odds_df)
  # } else {
  #   message("Failed to fetch odds data. Status code: ", response$status_code)
  #   return(NULL)
  # }
}
toa_sports_odds(sport = "basketball_ncaab", regions = "us", markets = "spreads", odds_format = 'decimal', date_format = 'iso')
toa_event_odds()

fetch_odds(api_key = api_key)

# Function to map ESPN names to KenPom names using the lookup table
map_team_name <- function(espn_team, name_mapping) {
  mapped_name <- name_mapping$kenpom_name[match(espn_team, name_mapping$espn_name)]
  if (is.na(mapped_name)) {
    return(NA)
  } else {
    return(mapped_name)
  }
}

# Function to predict scores
simulate_game <- function(Team1, Team2, kenpom) {
  off_eff1 <- stat_finder(Team1, "AdjO", kenpom)
  def_eff1 <- stat_finder(Team1, "AdjD", kenpom)
  off_eff2 <- stat_finder(Team2, "AdjO", kenpom)
  def_eff2 <- stat_finder(Team2, "AdjD", kenpom)
  tempo1 <- stat_finder(Team1, "AdjT", kenpom)
  tempo2 <- stat_finder(Team2, "AdjT", kenpom)
  avg_eff <- mean(kenpom$AdjO, na.rm = TRUE)
  avg_tempo <- mean(kenpom$AdjT, na.rm = TRUE)
  
  o_diff1 <- off_eff1 - avg_eff
  o_diff2 <- off_eff2 - avg_eff
  d_diff1 <- def_eff1 - avg_eff
  d_diff2 <- def_eff2 - avg_eff
  merged_delta1 <- o_diff1 + d_diff2
  merged_delta2 <- o_diff2 + d_diff1
  exp_o_eff1 <- merged_delta1 + avg_eff
  exp_o_eff2 <- merged_delta2 + avg_eff
  t_diff1 <- tempo1 - avg_tempo
  t_diff2 <- tempo2 - avg_tempo
  merged_delta_tempo <- t_diff1 + t_diff2
  exp_tempo <- avg_tempo + merged_delta_tempo
  
  score1 <- (exp_o_eff1/100) * exp_tempo
  score2 <- (exp_o_eff2/100) * exp_tempo
  scores <- setNames(data.frame(score1, score2), c(Team1, Team2))
  return(scores)
}

# Connect to SQLite database
db <- dbConnect(SQLite(), "sports_data.db")

# Update daily data in database
update_daily_data <- function() {
  # Fetch today's data
  todays_date <- format(Sys.Date(), "%Y-%m-%d")
  date_from <- paste0(todays_date, "T00:00:00Z")
  date_to <- paste0(todays_date, "T23:59:59Z")
  
  # Fetch odds
  odds_df <- fetch_odds(api_key = "your_api_key", date_from = date_from, date_to = date_to)
  
  # Fetch daily matchups
  matchups <- get_daily_matchups(format(Sys.Date(), "%Y%m%d"))
  if (is.null(matchups)) {
    message("No matchups available for today.")
    return()
  }
  
  # Process each matchup
  results <- data.frame(Game = character(), Home_Team = character(), Away_Team = character(), 
                        Predicted_Home = numeric(), Predicted_Away = numeric(), Margin = numeric(), 
                        Spread = character(), Bet_Recommendation = character(), Edge = numeric(), 
                        Actual_Home_Score = numeric(), Actual_Away_Score = numeric(), Bet_Accuracy = logical(),
                        stringsAsFactors = FALSE)
  
  for (i in 1:nrow(matchups)) {
    home_team <- matchups[i, "home_team_location"]
    away_team <- matchups[i, "away_team_location"]
    
    # Map ESPN names to KenPom
    home_team_kenpom <- map_team_name(home_team, name_mapping)
    away_team_kenpom <- map_team_name(away_team, name_mapping)
    if (is.na(home_team_kenpom) || is.na(away_team_kenpom)) next
    
    # Simulate game and get predicted scores
    prediction <- simulate_game(home_team_kenpom, away_team_kenpom, kenpom)
    home_team_score <- prediction[1, 1]
    away_team_score <- prediction[1, 2]
    
    # Get odds for the game
    game_id <- matchups[i, "game_id"]
    odds <- get_odds(game_id)
    spreadText <- if (!is.null(odds) && nrow(odds) > 0) odds[1, 1] else "No Odds Available"
    
    # Calculate bet recommendation and edge
    predicted_margin <- home_team_score - away_team_score
    spreadNumeric <- as.numeric(sub(".*-([0-9.]+)", "\\1", spreadText))
    home_edge <- predicted_margin + spreadNumeric
    bet_recommendation <- if (home_edge > 0) paste("Bet on", home_team) else paste("Bet on", away_team)
    edge <- if (grepl(away_team, bet_recommendation)) -home_edge else home_edge
    
    # Update results
    results <- rbind(results, data.frame(Game = paste(home_team, "vs", away_team), 
                                         Home_Team = home_team, Away_Team = away_team, 
                                         Predicted_Home = home_team_score, Predicted_Away = away_team_score, 
                                         Margin = predicted_margin, Spread = spreadText, 
                                         Bet_Recommendation = bet_recommendation, Edge = edge))
  }
  
  # Save to SQL database
  dbWriteTable(db, "daily_results", results, append = TRUE, row.names = FALSE)
  message("Daily data updated successfully.")
}

# Schedule this script to run daily using cron or Task Scheduler
update_daily_data()

dbDisconnect(db)
