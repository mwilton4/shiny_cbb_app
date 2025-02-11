library(shiny)
library(rsconnect)
library(dplyr)
library(readxl)
library(hoopR)
library(jsonlite)
library(DBI)
library(RSQLite)
library(httr)
library(oddsapiR)
# comment out this line when deploying
# rsconnect::deployApp("/Users/matthewwilton/Desktop/Sports Analytics/Shiny_CBB")
# setwd("/Users/matthewwilton/Desktop/Sports Analytics/Shiny_CBB")


# Kenpom data from web for predictions
 kenpom <- read_excel("KenPom.xlsx", skip = 1, sheet = 1)
 kenpom <- na.omit(kenpom)

# getting rid of labels in the middle of the table
 kenpom$AdjO <- as.numeric(as.character(kenpom$ORtg...6))
 kenpom$AdjD <- as.numeric(as.character(kenpom$DRtg...8))
 kenpom$AdjT <- as.numeric(as.character(kenpom$AdjT))
 teams <- c(kenpom$Team)

# kenpom table just using HoopR
# kenpom <- tryCatch({
#  message("Attempting to fetch KenPom data via API...")
#  kp_efficiency(min_year = 2022, max_year = 2023)
#}, error = function(e) {
#  message("Error fetching KenPom efficiency data: ", e)
#  message("Attempting to read KenPom data from Excel as fallback...")
  
  # Check if file exists before attempting to read it
#  if (file.exists("KenPom.xlsx")) {
#    message("Found KenPom.xlsx. Reading data from Excel...")
#    read_excel("KenPom.xlsx", skip = 1, sheet = 1)
#  } else {
#    stop("KenPom data not available via API, and fallback Excel file is missing.")
#  }
#})

if (is.null(kenpom)) {
  stop("KenPom efficiency data could not be retrieved. Check the availability of the data for the current season.")
}

kenpom <- na.omit(kenpom)
kenpom$AdjO <- as.numeric(as.character(kenpom$AdjO))
kenpom$AdjD <- as.numeric(as.character(kenpom$AdjD))
kenpom$AdjT <- as.numeric(as.character(kenpom$AdjT))
teams <- kenpom$Team


# MAPPING
# Create a manual mapping between ESPN team names and KenPom team names
name_mapping <- data.frame(
  espn_name = c(
    "Abilene Christian", "Air Force", "Akron", "Alabama A&M", "Alabama", 
    "Alabama State", "Alcorn State", "American University", "App State", "Arizona State",
    "Arizona", "Arkansas", "Arkansas State", "Arkansas-Pine Bluff", "Army",
    "Auburn", "Austin Peay", "BYU", "Ball State", "Baylor",
    "Bellarmine", "Belmont", "Bethune-Cookman", "Binghamton", "Boise State",
    "Boston College", "Boston University", "Bowling Green", "Bradley", "Brown",
    "Bryant", "Bucknell", "Buffalo", "Butler", "Cal Poly",
    "Cal State Bakersfield", "Cal State Fullerton", "Cal State Northridge", "California Baptist", "California",
    "Campbell", "Canisius", "Central Arkansas", "Central Connecticut", "Central Michigan",
    "Charleston", "Charleston Southern", "Charlotte", "Chattanooga", "Chicago State",
    "Cincinnati", "Clemson", "Cleveland State", "Coastal Carolina", "Colgate",
    "Colorado", "Colorado State", "Columbia", "Coppin State", "Cornell",
    "Creighton", "Dartmouth", "Davidson", "Dayton", "DePaul",
    "Delaware", "Delaware State", "Denver", "Detroit Mercy", "Drake",
    "Drexel", "Duke", "Duquesne", "East Carolina", "East Tennessee State",
    "Eastern Illinois", "Eastern Kentucky", "Eastern Michigan", "Eastern Washington", "Elon",
    "Evansville", "Fairfield", "Fairleigh Dickinson", "Florida A&M", "Florida Atlantic",
    "Florida", "Florida Gulf Coast", "Florida International", "Florida State", "Fordham",
    "Fresno State", "Furman", "Gardner-Webb", "George Mason", "George Washington",
    "Georgetown", "Georgia", "Georgia Southern", "Georgia State", "Georgia Tech",
    "Gonzaga", "Grambling", "Grand Canyon", "Green Bay", "Hampton",
    "Harvard", "Hawai'i", "High Point", "Hofstra", "Holy Cross",
    "Houston Christian", "Houston", "Howard", "IU Indianapolis", "Idaho State",
    "Idaho", "Illinois", "Illinois State", "Incarnate Word", "Indiana",
    "Indiana State", "Iona", "Iowa", "Iowa State", "Jackson State",
    "Jacksonville", "Jacksonville State", "James Madison", "UMKC", "Kansas",
    "Kansas State", "Kennesaw State", "Kent State", "Kentucky", "LSU",
    "La Salle", "Lafayette", "Lamar", "Le Moyne", "Lehigh",
    "Liberty", "Lipscomb", "Little Rock", "Long Beach State", "Long Island University",
    "Longwood", "Louisiana", "Louisiana Tech", "Louisville", "Loyola Chicago",
    "Loyola Maryland", "Loyola Marymount", "Maine", "Manhattan", "Marist",
    "Marquette", "Marshall", "Maryland Eastern Shore", "Maryland", "Massachusetts",
    "McNeese", "Memphis", "Mercer", "Merrimack", "Mercyhurst",
    "Miami (OH)", "Miami", "Michigan State", "Michigan", "Middle Tennessee",
    "Milwaukee", "Minnesota", "Mississippi State", "Mississippi Valley State", "Missouri State",
    "Missouri", "Monmouth", "Montana", "Montana State", "Morehead State",
    "Morgan State", "Mount St. Mary's", "Murray State", "NC State", "NJIT",
    "Navy", "Nebraska", "Nevada", "New Hampshire", "New Mexico",
    "New Mexico State", "New Orleans", "Niagara", "Nicholls", "Norfolk State",
    "North Alabama", "North Carolina A&T", "North Carolina Central", "North Carolina", "North Dakota",
    "North Dakota State", "North Florida", "North Texas", "Northeastern", "Northern Arizona",
    "Northern Colorado", "Northern Illinois", "Northern Iowa", "Northern Kentucky", "Northwestern State",
    "Northwestern", "Notre Dame", "Oakland", "Ohio", "Ohio State",
    "Oklahoma", "Oklahoma State", "Old Dominion", "Ole Miss", "Omaha",
    "Oral Roberts", "Oregon", "Oregon State", "Pacific", "Penn State",
    "Pennsylvania", "Pepperdine", "Pittsburgh", "Portland", "Portland State",
    "Prairie View A&M", "Presbyterian", "Princeton", "Providence", "Purdue",
    "Purdue Fort Wayne", "Quinnipiac", "Radford", "Rhode Island", "Rice",
    "Richmond", "Rider", "Robert Morris", "Rutgers", "SE Louisiana",
    "SIU Edwardsville", "SMU", "Sacramento State", "Sacred Heart", "Saint Joseph's",
    "Saint Louis", "Saint Mary's", "Saint Peter's", "Sam Houston", "Samford",
    "San Diego State", "San Diego", "San Francisco", "San José State", "Santa Clara",
    "Seattle U", "Seton Hall", "Siena", "South Alabama", "South Carolina",
    "South Carolina State", "South Carolina Upstate", "South Dakota", "South Dakota State", "South Florida",
    "Southeast Missouri State", "Southern Illinois", "Southern", "Southern Miss", "Southern Utah",
    "St. Bonaventure", "St. Francis (PA)", "St. John's", "St. Thomas-Minnesota", "Stanford",
    "Stephen F. Austin", "Stetson", "Stonehill", "Stony Brook", "Syracuse",
    "TCU", "Tarleton State", "Temple", "Tennessee State", "Tennessee Tech",
    "Tennessee", "Texas A&M", "Texas A&M-Commerce", "Texas A&M-Corpus Christi", "Texas",
    "Texas Southern", "Texas State", "Texas Tech", "The Citadel", "Toledo",
    "Towson", "Troy", "Tulane", "Tulsa", "UAB",
    "UAlbany", "UC Davis", "UC Irvine", "UC Riverside", "UC San Diego",
    "UC Santa Barbara", "UCF", "UCLA", "UConn", "UIC",
    "UL Monroe", "UMBC", "UMass Lowell", "UNC Asheville", "UNC Greensboro",
    "UNC Wilmington", "UNLV", "USC", "UT Arlington", "UT Martin",
    "UT Rio Grande Valley", "UTEP", "UTSA", "Utah State", "Utah Tech",
    "Utah", "Utah Valley", "VCU", "VMI", "Valparaiso",
    "Vanderbilt", "Vermont", "Villanova", "Virginia", "Virginia Tech",
    "Wagner", "Wake Forest", "Washington", "Washington State", "Weber State",
    "West Georgia", "West Virginia", "Western Carolina", "Western Illinois", "Western Kentucky",
    "Western Michigan", "Wichita State", "William & Mary", "Winthrop", "Wisconsin",
    "Wofford", "Wright State", "Wyoming", "Xavier", "Yale",
    "Youngstown State"
  ),
  kenpom_name = c("Abilene Christian", "Air Force", "Akron", "Alabama A&M", "Alabama", 
                  "Alabama St.", "Alcorn St.", "American", "Appalachian St.", "Arizona St.", 
                  "Arizona", "Arkansas", "Arkansas St.", "Arkansas Pine Bluff", "Army", 
                  "Auburn", "Austin Peay", "BYU", "Ball St.", "Baylor", 
                  "Bellarmine", "Belmont", "Bethune Cookman", "Binghamton", "Boise St.", 
                  "Boston College", "Boston University", "Bowling Green", "Bradley", "Brown", 
                  "Bryant", "Bucknell", "Buffalo", "Butler", "Cal Poly", 
                  "Cal St. Bakersfield", "Cal St. Fullerton", "Cal St. Northridge", "Cal Baptist", "California", 
                  "Campbell", "Canisius", "Central Arkansas", "Central Connecticut", "Central Michigan", 
                  "Charleston", "Charleston Southern", "Charlotte", "Chattanooga", "Chicago St.", 
                  "Cincinnati", "Clemson", "Cleveland St.", "Coastal Carolina", "Colgate", 
                  "Colorado", "Colorado St.", "Columbia", "Coppin St.", "Cornell", 
                  "Creighton", "Dartmouth", "Davidson", "Dayton", "DePaul", 
                  "Delaware", "Delaware St.", "Denver", "Detroit Mercy", "Drake", 
                  "Drexel", "Duke", "Duquesne", "East Carolina", "East Tennessee St.", 
                  "Eastern Illinois", "Eastern Kentucky", "Eastern Michigan", "Eastern Washington", "Elon", 
                  "Evansville", "Fairfield", "Fairleigh Dickinson", "Florida A&M", "Florida Atlantic", 
                  "Florida", "Florida Gulf Coast", "FIU", "Florida St.", "Fordham", 
                  "Fresno St.", "Furman", "Gardner Webb", "George Mason", "George Washington", 
                  "Georgetown", "Georgia", "Georgia Southern", "Georgia St.", "Georgia Tech", 
                  "Gonzaga", "Grambling St.", "Grand Canyon", "Green Bay", "Hampton", 
                  "Harvard", "Hawaii", "High Point", "Hofstra", "Holy Cross", 
                  "Houston Christian", "Houston", "Howard", "IUPUI", "Idaho St.", 
                  "Idaho", "Illinois", "Illinois St.", "Incarnate Word", "Indiana", 
                  "Indiana St.", "Iona", "Iowa", "Iowa St.", "Jackson St.", 
                  "Jacksonville", "Jacksonville St.", "James Madison", "Kansas City", "Kansas", 
                  "Kansas St.", "Kennesaw St.", "Kent St.", "Kentucky", "LSU", 
                  "La Salle", "Lafayette", "Lamar", "Le Moyne", "Lehigh", 
                  "Liberty", "Lipscomb", "Little Rock", "Long Beach St.", "LIU", 
                  "Longwood", "Louisiana", "Louisiana Tech", "Louisville", "Loyola Chicago", 
                  "Loyola MD", "Loyola Marymount", "Maine", "Manhattan", "Marist", 
                  "Marquette", "Marshall", "Maryland Eastern Shore", "Maryland", "Massachusetts", 
                  "McNeese St.", "Memphis", "Mercer", "Merrimack", "Mercyhurst", 
                  "Miami OH", "Miami FL", "Michigan St.", "Michigan", "Middle Tennessee", 
                  "Milwaukee", "Minnesota", "Mississippi St.", "Mississippi Valley St.", "Missouri St.", 
                  "Missouri", "Monmouth", "Montana", "Montana St.", "Morehead St.", 
                  "Morgan St.", "Mount St. Mary's", "Murray St.", "N.C. State", "NJIT", 
                  "Navy", "Nebraska", "Nevada", "New Hampshire", "New Mexico", 
                  "New Mexico St.", "New Orleans", "Niagara", "Nicholls St.", "Norfolk St.", 
                  "North Alabama", "North Carolina A&T", "North Carolina Central", "North Carolina", "North Dakota", 
                  "North Dakota St.", "North Florida", "North Texas", "Northeastern", "Northern Arizona", 
                  "Northern Colorado", "Northern Illinois", "Northern Iowa", "Northern Kentucky", "Northwestern St.", 
                  "Northwestern", "Notre Dame", "Oakland", "Ohio", "Ohio St.", 
                  "Oklahoma", "Oklahoma St.", "Old Dominion", "Mississippi", "Nebraska Omaha", 
                  "Oral Roberts", "Oregon", "Oregon St.", "Pacific", "Penn St.", 
                  "Penn", "Pepperdine", "Pittsburgh", "Portland", "Portland St.", 
                  "Prairie View A&M", "Presbyterian", "Princeton", "Providence", "Purdue", 
                  "Purdue Fort Wayne", "Quinnipiac", "Radford", "Rhode Island", "Rice", 
                  "Richmond", "Rider", "Robert Morris", "Rutgers", "Southeastern Louisiana", 
                  "SIU Edwardsville", "SMU", "Sacramento St.", "Sacred Heart", "Saint Joseph's", 
                  "Saint Louis", "Saint Mary's", "Saint Peter's", "Sam Houston St.", "Samford", 
                  "San Diego St.", "San Diego", "San Francisco", "San Jose St.", "Santa Clara", 
                  "Seattle", "Seton Hall", "Siena", "South Alabama", "South Carolina", 
                  "South Carolina St.", "USC Upstate", "South Dakota", "South Dakota St.", "South Florida", 
                  "Southeast Missouri St.", "Southern Illinois", "Southern", "Southern Miss", "Southern Utah", 
                  "St. Bonaventure", "Saint Francis", "St. John's", "St. Thomas", "Stanford", 
                  "Stephen F. Austin", "Stetson", "Stonehill", "Stony Brook", "Syracuse", 
                  "TCU", "Tarleton St.", "Temple", "Tennessee St.", "Tennessee Tech", 
                  "Tennessee", "Texas A&M", "Texas A&M Commerce", "Texas A&M Corpus Chris", "Texas", 
                  "Texas Southern", "Texas St.", "Texas Tech", "The Citadel", "Toledo", 
                  "Towson", "Troy", "Tulane", "Tulsa", "UAB", 
                  "Albany", "UC Davis", "UC Irvine", "UC Riverside", "UC San Diego", 
                  "UC Santa Barbara", "UCF", "UCLA", "Connecticut", "Illinois Chicago", 
                  "Louisiana Monroe", "UMBC", "UMass Lowell", "UNC Asheville", "UNC Greensboro", 
                  "UNC Wilmington", "UNLV", "USC", "UT Arlington", "Tennessee Martin", 
                  "UT Rio Grande Valley", "UTEP", "UTSA", "Utah St.", "Utah Tech", 
                  "Utah", "Utah Valley", "VCU", "VMI", "Valparaiso", 
                  "Vanderbilt", "Vermont", "Villanova", "Virginia", "Virginia Tech", 
                  "Wagner", "Wake Forest", "Washington", "Washington St.", "Weber St.", 
                  "West Georgia", "West Virginia", "Western Carolina", "Western Illinois", "Western Kentucky 15", 
                  "Western Michigan", "Wichita St.", "William & Mary", "Winthrop", "Wisconsin", 
                  "Wofford", "Wright St.", "Wyoming", "Xavier", "Yale", 
                  "Youngstown St."),
  stringsAsFactors = FALSE
)

# Function to map ESPN names to KenPom names using the lookup table
map_team_name <- function(espn_team) {
  # Search for the ESPN name in the name_mapping table
  mapped_name <- name_mapping$kenpom_name[match(espn_team, name_mapping$espn_name)]
  
  # If the name isn't found in the lookup table, return the original ESPN name
  if (is.na(mapped_name)) {
    return(NA)  # You can modify this to return a specific value, e.g., NA or "Unknown"
  } else {
    return(mapped_name)
  }
}




# find the stat for the relevant team. mimics the vlookup in the excel version
stat_finder <- function(Team, stat) {
  # find the row of the team
  row_index <- which(kenpom$Team == Team)
  
  # Now use this index to get the corresponding stat value
  if(length(row_index) > 0) {
    stat_return <- kenpom[row_index, stat]
    return(stat_return)
  } else {
    return("Team not found in the dataset. Name might be incorrect")
  }
}

# function that predicts scores
simulate_game <- function(Team1, Team2) {
  # Getting data from the two teams
  off_eff1 <- stat_finder(Team1, "AdjO")
  def_eff1 <- stat_finder(Team1, "AdjD")
  off_eff2 <- stat_finder(Team2, "AdjO")
  def_eff2 <- stat_finder(Team2, "AdjD")
  tempo1 <- stat_finder(Team1, "AdjT")
  tempo2 <- stat_finder(Team2, "AdjT")
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
  # Generating the score predictions
  score1 <- (exp_o_eff1/100)*exp_tempo
  score2 <- (exp_o_eff2/100)*exp_tempo
  
  # Set the names in the output as the teams
  scores <- setNames(data.frame(score1, score2), c(Team1, Team2))
  
  return(scores)
}

# Function to get daily matchups from hoopR
# todaysDate should be in format "YYYYMMDD"
get_daily_matchups <- function(todaysDate) {
  # Get matchups for today
  matchups <- tryCatch({
    espn_mbb_scoreboard(season = todaysDate)
  }, error = function(e) {
    message("No games available or invalid date format.")
    return(NULL)
  })  
  
  # Print the structure of matchups to inspect the data
  print(str(matchups))  # This will print the structure of the matchups object to the console
  
  
  # ERROR HERE!!!!!! 
  # It is printing this message everytime
  # Check if matchups exists, and handle potential NA or missing game_id
  if (is.null(matchups) || any(is.na(matchups$game_id)) || !("game_id" %in% names(matchups))) {
    message("No valid matchups data found or game_id column is missing.")
    return(NULL)
  }
  
  # Check if the game_id column has any entries (using length() for vectors)
  if (length(matchups$game_id) == 0) {
    message("No games found for the selected date.")
    return(NULL)
  }
  
  #matchups <- matchups$matchup
  return(matchups)
  # will need to do some data cleaning to extract the team names and match it to the Kenpom names
}


# get upcoming events from odds API
api_key <- toa_key()


  
  # Function to fetch games from Odds API
  # The sport key obtained from calling the /sports endpoint. upcoming is always valid, returning any live games as well as the next 8 upcoming games across all sports
  fetch_games <- function(api_key, sport) {
    url <- paste0("https://api.the-odds-api.com/v4/sports/", sport, "/events?api_key=", api_key)
    response <- GET(url)
    events_data <- fromJSON(content(response, "text"))
    return(events_data)
    #return(response)
  }

 fetch_games(api_key, "basketball_ncaab")


# Function to get betting odds from ESPN for each game
get_odds <- function(gameID) {

  # If there are no odds, do something to prevent an error
  
  # Get Odds
  betting_odds <- tryCatch({
    espn_mbb_betting(game_id = gameID)$pickcenter
  }, error = function(e) {
    message("No betting information available for game ID: ", gameID)
    return(NULL)
  })
  
  # Check if betting_odds is NULL or empty
   if (is.null(betting_odds) || nrow(betting_odds) == 0) {
    message("No betting data found for the game.")
    return(NULL)
   }
  
  betting_odds <- betting_odds[1, ]

  # Return the odds data
  return(betting_odds)
}
 oddsTest <- get_odds(401486958)
 oddsTest[1,3]

 # Function to check if the recommended bet was accurate
 check_bet_accuracy <- function(predicted_home, predicted_away, actual_home, actual_away, spread, recommendation, home_team, away_team) {
   # Calculate the predicted and actual margins
   predicted_margin <- predicted_home - predicted_away
   actual_margin <- actual_home - actual_away
   
   
   # Determine if the recommended bet was correct based on the team name in the recommendation
   if (grepl(home_team, recommendation, fixed = TRUE)) {
     # Bet was recommended on home team
     return((predicted_margin > spread) == (actual_margin > spread))
   } else if (grepl(away_team, recommendation, fixed = TRUE)) {
     # Bet was recommended on away team
     return((predicted_margin < -spread) == (actual_margin < -spread))
   } else {
     return(NA)  # If no valid bet recommendation was given
   }
 }
 
 
ui <- fluidPage(
  titlePanel("College Basketball Score Prediction"),
  # Add tabs using tabsetPanel
  tabsetPanel(
      # First tab: predictions
      tabPanel("Matchup Creator",
        
        sidebarLayout(
          sidebarPanel(
            selectInput("team1", "Select Team 1", choices = teams),
            selectInput("team2", "Select Team 2", choices = teams),
            actionButton("predict", "Predict Scores")
          ),
          
          mainPanel(
            h3("Predicted Scores"),
            verbatimTextOutput("prediction1")
            
          )
        )
      ),
    
    
      # Second tab: About
      tabPanel("About", 
               h3("About this App"),
               p("This app predicts the scores of college basketball games based on adjusted offensive, defensive, and tempo ratings via Kenpom."),
               withMathJax(),
               h4("Prediction Calculation"),
               HTML("
          <p>The score prediction function combines each team's offensive and defensive ratings with the game's expected tempo to calculate final scores. Here is a step-by-step breakdown:</p>
          <ol>
            <li><b>Calculate Team Differences:</b> Calculate the offensive and defensive differences of each team from the league average:<br>
              $$ \\Delta O_{\\text{Team1}} = \\text{AdjO}_{\\text{Team1}} - \\overline{\\text{AdjO}} $$ 
              $$ \\Delta O_{\\text{Team2}} = \\text{AdjO}_{\\text{Team2}} - \\overline{\\text{AdjO}} $$ 
              $$ \\Delta D_{\\text{Team1}} = \\text{AdjD}_{\\text{Team1}} - \\overline{\\text{AdjO}} $$ 
              $$ \\Delta D_{\\text{Team2}} = \\text{AdjD}_{\\text{Team2}} - \\overline{\\text{AdjO}} $$
            </li>
            <li><b>Expected Offensive Ratings:</b> Adjust each team's offensive rating based on the opponent's defensive rating:<br>
              $$ \\text{ExpectedOff}_{\\text{Team1}} = \\Delta O_{\\text{Team1}} + \\Delta D_{\\text{Team2}} + \\overline{\\text{AdjO}} $$ 
              $$ \\text{ExpectedOff}_{\\text{Team2}} = \\Delta O_{\\text{Team2}} + \\Delta D_{\\text{Team1}} + \\overline{\\text{AdjO}} $$
            </li>
            <li><b>Calculate Expected Tempo:</b> Determine the tempo of the game based on each team's tempo rating relative to the league average:<br>
              $$ \\Delta T_{\\text{Team1}} = \\text{AdjT}_{\\text{Team1}} - \\overline{\\text{AdjT}} $$ 
              $$ \\Delta T_{\\text{Team2}} = \\text{AdjT}_{\\text{Team2}} - \\overline{\\text{AdjT}} $$ 
              $$ \\text{ExpectedTempo} = \\Delta T_{\\text{Team1}} + \\Delta T_{\\text{Team2}} + \\overline{\\text{AdjT}} $$
            </li>
            <li><b>Calculate Predicted Scores:</b> Combine the expected offensive ratings and tempo to predict final scores:<br>
              $$ \\text{Score}_{\\text{Team1}} = \\left( \\frac{\\text{ExpectedOff}_{\\text{Team1}}}{100} \\right) \\times \\text{ExpectedTempo} $$ 
              $$ \\text{Score}_{\\text{Team2}} = \\left( \\frac{\\text{ExpectedOff}_{\\text{Team2}}}{100} \\right) \\times \\text{ExpectedTempo} $$
            </li>
          </ol>
        "),
               h4("Goal:"),
               p("This app aims to predict CBB scores more accurately than sportsbooks over the long run. 
                 The idea is that sportsbooks will move their lines to account for the public's money and reduce their risk. This creates a potential opportunity to make a profitable betting system over the long run."),
               h4("Future:"),
               p("Add data to a SQL database where we can run queries on betting results.
                 Some questions to answer:
                 Is a higher edge between my predictions and the sportsbook more profitable? Or indicative of missing information in my predictions?
                 Which teams consistently cover spreads?
                 How important is home court?"),
               p("NOTE: The api to get odds does not work for games past 2023. Working on a workaround for this.")
      ),
      
      # Commented out: Simulated Games vs Sportsbook Lines tab
      # tabPanel("Simulated Games vs Sportsbook Lines", 
      #          sidebarLayout(
      #            sidebarPanel(
      #              dateInput("date", "Select Date", value = Sys.Date()),
      #              actionButton("simulate", "Simulate Today's Games")
      #            ),
      #            mainPanel(
      #              tableOutput("simulated_scores")
      #            )
      #          )),
      
      # Commented out: Today's Games tab
      # tabPanel(
      #   "Today's Games",
      #   sidebarLayout(
      #     sidebarPanel(
      #       actionButton("fetch_games", "Fetch Today's Games")
      #     ),
      #     mainPanel(
      #       h3("Games Scheduled for Today"),
      #       tableOutput("todays_games_table")
      #     )
      #   )
      # )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    # Extract the selected teams
    selected_team1 <- input$team1
    selected_team2 <- input$team2
    
    # Run the model
    prediction1 <- simulate_game(selected_team1,selected_team2)
    
    # Display the predictions
    output$prediction1 <- renderText({
      paste(selected_team1, prediction1[1],"\n",selected_team2, prediction1[2])
    })

  })
  
  # show all the games for today
  observeEvent(input$fetch_games, {
    # Get API key
    api_key <- toa_key() # Ensure this returns a valid API key
    
    # Run function to fetch today's games
    today_games <- tryCatch({
      fetch_games(api_key, "basketball_ncaab")
    }, error = function(e) {
      message("Error fetching games: ", e)
      return(NULL)
    })
    
    # Debugging: Check structure of today_games
    print(str(today_games))
    
    # Check if today_games is valid
    if (is.null(today_games) || nrow(today_games) == 0) {
      output$todays_games_table <- renderTable({
        data.frame(Message = "No games available for today.")
      })
      return()
    }
    
    
    # Function to extract the university name
    extract_university <- function(team_name) {
      # Split the name into parts
      name_parts <- strsplit(team_name, " ")[[1]]
      # Keep all parts except the last two
      university <- paste(name_parts[1:(length(name_parts) - 2)], collapse = " ")
      return(university)
    }
    
    # Apply the function to the `home_team` and `away_team` columns
    today_games <- today_games %>%
      mutate(
        Home_University = sapply(home_team, extract_university),
        Away_University = sapply(away_team, extract_university)
      )
    
    # Vectorize the map_team_name function
    vectorized_map_team_name <- Vectorize(map_team_name)
    
    # Apply the mapping to the new university columns
    today_games <- today_games %>%
      mutate(
        Home_KenPom = vectorized_map_team_name(Home_University),
        Away_KenPom = vectorized_map_team_name(Away_University)
      )
    
    # Select and transform relevant columns for display
    games_display <- today_games %>%
      dplyr::select(
        Home_Team = home_team,
        Away_Team = away_team,
        Home_KenPom,
        Away_KenPom,
        Game_Time = commence_time
      )
    
    # Render the games table in the UI
    output$todays_games_table <- renderTable({
      games_display
    }, rownames = FALSE)
  })
  
  
  
  
  
  
  
  # Simulate all games for the day and compare with sportsbook odds
  observeEvent(input$simulate, {
    
    # Get the selected date from the date input
    selected_date <- input$date
     
    # need to translate format to yyyymmdd from yyyy-mm-dd
    # Remove the dashes using gsub()
    selected_date <- gsub("-", "", selected_date)
    
    # Get the daily matchups using hoopR
    matchups <- get_daily_matchups(selected_date)
    
    # If no matchups are returned, stop further execution
    print(matchups)
    if (is.null(matchups)) {
      output$simulated_scores <- renderTable({
        data.frame(Message = "No games available for the selected date.")
      })
      return()
    }
    
    
    # Create a data frame to store results
    results <- data.frame(
      Game = character(),
      Home_Team = character(),
      Away_Team = character(),
      Predicted_Home = numeric(),
      Predicted_Away = numeric(),
      Margin = numeric(),
      Spread = character(),
      Bet_Recommendation = character(),
      Edge = numeric(),
      Actual_Home_Score = numeric(),
      Actual_Away_Score = numeric(),
      Bet_Accuracy = logical(),
      # Home_Spread = numeric(),
      # Spread_Odds = numeric(),
      # Over_Under = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Loop through each game and simulate
    for (i in 1:nrow(matchups)) {
      home_team <- matchups[i, "home_team_location"]
      away_team <- matchups[i, "away_team_location"]
      print(home_team)
      print(away_team)
      
      # right here is where we would need to implement the transformation of the names
      # transfer from ESPN format to kenpom
      # Map ESPN names to KenPom names
      home_team_kenpom <- map_team_name(home_team)
      away_team_kenpom <- map_team_name(away_team)
      print(home_team_kenpom)
      print(away_team_kenpom)
      
      # If either home_team_kenpom or away_team_kenpom is "NA", skip this iteration
      if (is.na(home_team_kenpom) || is.na(away_team_kenpom)) {
        next  # Skip to the next iteration of the loop
      }
      
      # Get the simulated scores
      prediction <- simulate_game(home_team_kenpom, away_team_kenpom)

      # Assuming you are working with predictions or team stats
      home_team_score <- prediction[1, 1]
      away_team_score <- prediction[1, 2]
      
      # Check if prediction is valid
      if (is.null(prediction)) {
        message("Prediction is NULL for the game.")
        next
      }
      
      # Check if the scores are available in the prediction
      if (is.null(home_team_score) || is.null(away_team_score) || 
          is.na(home_team_score) || is.na(away_team_score)) {
        message("Prediction scores are missing or invalid.")
        next  # Skip to the next iteration
      }
      
      # get the actual scores
      home_actual_score <- matchups[i, "home_score"][[1]]
      away_actual_score <- matchups[i, "away_score"][[1]]
        
      # Extract the game ID as an integer
      game_id <- matchups[i, "game_id"][[1]]  # This extracts the integer value from the tibble
      
      # print game ID before getting odds to make sure its valid
      print(game_id)
      # get the odds for the game
      odds <- get_odds(game_id)
      
      # print odds to check if its actually getting odds
      print(odds)
      
      # if odds are null return "no odds available"
      # else return the spread of the game in format (ABR -##)
      spreadText <- if (!is.null(odds) && nrow(odds) > 0) odds[1,1] else "No Odds Available"
     
            
      # get the predicted margin
      predicted_margin <- home_team_score - away_team_score
      
      if (!is.null(odds)) {
        
        # get the spread as numeric for the home team
        spreadNumeric <- odds[1,3]
        
        # calculate the edge of the home team by taking 
        # (home - away) and comparing to the home team spread
        # home_edge <- if ((odds[1, "home_team_odds_favorite"]) == "TRUE") odds[1,3] else (odds[1,3])*(-1) 
        home_edge <- predicted_margin + spreadNumeric
        
        
      bet_recommendation <- if (home_edge > 0) {
        paste("Bet on", home_team)
        
      } else {
        paste("Bet on", away_team)
      }
      
        
      # if the bet is on the away team make sure the edge shows as positive
      if (grepl(paste0("Bet on ", away_team), bet_recommendation)) {
        # Your logic when the recommendation is to bet on the away team
        edge <- home_edge*(-1)
        
      }
      else {
        # when the recommendation is on the home team
        edge <- home_edge
        
      }
      
    } else {
      bet_recommendation <- "No Odds Available"
      edge <- "No Odds Available"
    }
      

      # Check bet accuracy
      bet_accuracy <- check_bet_accuracy(home_team_score, away_team_score, 
                                         home_actual_score, away_actual_score, 
                                         spreadNumeric, bet_recommendation, home_team, away_team)
       
      # Add the results to the data frame
      results <- rbind(results, data.frame(
        Game = paste(home_team, "vs", away_team),
        Home_Team = home_team,
        Away_Team = away_team,
        Predicted_Home = home_team_score,
        Predicted_Away = away_team_score,
        Margin = predicted_margin,
        Spread = spreadText,
        Bet_Recommendation = bet_recommendation,
        Edge = edge,
        Actual_Home_Score = home_actual_score,
        Actual_Away_Score = away_actual_score,
        Bet_Accuracy = bet_accuracy
        # Home_Spread = home_spread,
        # Spread_Odds = home_spread_odds,
        # Over_Under = over_under
      ))
    }
    
    # Render the results in the main panel
    output$simulated_scores <- renderTable({
      results
    })
  })
  
}

shinyApp(ui = ui, server = server)
