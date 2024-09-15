library(shiny)
library(rsconnect)
library(dplyr)
library(readxl)
library(hoopR)
# rsconnect::deployApp("/Users/matthewwilton/Desktop/Sports Analytics/Shiny_CBB")

# get API key

# pull odds

# Kenpom data from web for predictions
kenpom <- read_excel("KenPom.xlsx", skip = 1, sheet = 1)
kenpom <- na.omit(kenpom)

# getting rid of labels in the middle of the table
kenpom$AdjO <- as.numeric(as.character(kenpom$AdjO))
kenpom$AdjD <- as.numeric(as.character(kenpom$AdjD))
kenpom$AdjT <- as.numeric(as.character(kenpom$AdjT))
teams <- c(kenpom$Team)



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
    "Jacksonville", "Jacksonville State", "James Madison", "Kansas City", "Kansas",
    "Kansas State", "Kennesaw State", "Kent State", "Kentucky", "LSU",
    "La Salle", "Lafayette", "Lamar", "Le Moyne", "Lehigh",
    "Liberty", "Lipscomb", "Little Rock", "Long Beach State", "Long Island University",
    "Longwood", "Louisiana", "Louisiana Tech", "Louisville", "Loyola Chicago",
    "Loyola Maryland", "Loyola Marymount", "Maine", "Manhattan", "Marist",
    "Marquette", "Marshall", "Maryland Eastern Shore", "Maryland", "Massachusetts",
    "McNeese", "Memphis", "Mercer", "Mercyhurst", "Merrimack",
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
  kenpom_name = c("Abilene Christian", "Air Force", "Akron 14", "Alabama A&M", "Alabama 4", 
                  "Alabama St.", "Alcorn St.", "American", "Appalachian St.", "Arizona St.", 
                  "Arizona 2", "Arkansas", "Arkansas St.", "Arkansas Pine Bluff", "Army", 
                  "Auburn 4", "Austin Peay", "BYU 6", "Ball St.", "Baylor 3", 
                  "Bellarmine", "Belmont", "Bethune Cookman", "Binghamton", "Boise St. 10", 
                  "Boston College", "Boston University", "Bowling Green", "Bradley", "Brown", 
                  "Bryant", "Bucknell", "Buffalo", "Butler", "Cal Poly", 
                  "Cal St. Bakersfield", "Cal St. Fullerton", "Cal St. Northridge", "Cal Baptist", "California", 
                  "Campbell", "Canisius", "Central Arkansas", "Central Connecticut", "Central Michigan", 
                  "Charleston 13", "Charleston Southern", "Charlotte", "Chattanooga", "Chicago St.", 
                  "Cincinnati", "Clemson 6", "Cleveland St.", "Coastal Carolina", "Colgate 14", 
                  "Colorado 10", "Colorado St. 10", "Columbia", "Coppin St.", "Cornell", 
                  "Creighton 3", "Dartmouth", "Davidson", "Dayton 7", "DePaul", 
                  "Delaware", "Delaware St.", "Denver", "Detroit Mercy", "Drake 10", 
                  "Drexel", "Duke 4", "Duquesne 11", "East Carolina", "East Tennessee St.", 
                  "Eastern Illinois", "Eastern Kentucky", "Eastern Michigan", "Eastern Washington", "Elon", 
                  "Evansville", "Fairfield", "Fairleigh Dickinson", "Florida A&M", "Florida Atlantic 8", 
                  "Florida 7", "Florida Gulf Coast", "FIU", "Florida St.", "Fordham", 
                  "Fresno St.", "Furman", "Gardner Webb", "George Mason", "George Washington", 
                  "Georgetown", "Georgia", "Georgia Southern", "Georgia St.", "Georgia Tech", 
                  "Gonzaga 5", "Grambling St. 16", "Grand Canyon 12", "Green Bay", "Hampton", 
                  "Harvard", "Hawaii", "High Point", "Hofstra", "Holy Cross", 
                  "Houston Christian", "Houston 1", "Howard 16", "IUPUI", "Idaho St.", 
                  "Idaho", "Illinois 3", "Illinois St.", "Incarnate Word", "Indiana", 
                  "Indiana St.", "Iona", "Iowa", "Iowa St. 2", "Jackson St.", 
                  "Jacksonville", "Jacksonville St.", "James Madison 12", "UMKC", "Kansas 4", 
                  "Kansas St.", "Kennesaw St.", "Kent St.", "Kentucky 3", "LSU", 
                  "La Salle", "Lafayette", "Lamar", "Le Moyne", "Lehigh", 
                  "Liberty", "Lipscomb", "Little Rock", "Long Beach St. 15", "LIU", 
                  "Longwood 16", "Louisiana", "Louisiana Tech", "Louisville", "Loyola Chicago", 
                  "Loyola MD", "Loyola Marymount", "Maine", "Manhattan", "Marist", 
                  "Marquette 2", "Marshall", "Maryland Eastern Shore", "Maryland", "Massachusetts", 
                  "McNeese St. 12", "Memphis", "Mercer", "Merrimack", "Mercyhurst", 
                  "Miami OH", "Miami FL", "Michigan St. 9", "Michigan", "Middle Tennessee", 
                  "Milwaukee", "Minnesota", "Mississippi St. 8", "Mississippi Valley St.", "Missouri St.", 
                  "Missouri", "Monmouth", "Montana", "Montana St. 16", "Morehead St. 14", 
                  "Morgan St.", "Mount St. Mary's", "Murray St.", "N.C. State 11", "NJIT", 
                  "Navy", "Nebraska 8", "Nevada 10", "New Hampshire", "New Mexico 11", 
                  "New Mexico St.", "New Orleans", "Niagara", "Nicholls St.", "Norfolk St.", 
                  "North Alabama", "North Carolina A&T", "North Carolina Central", "North Carolina 1", "North Dakota", 
                  "North Dakota St.", "North Florida", "North Texas", "Northeastern", "Northern Arizona", 
                  "Northern Colorado", "Northern Illinois", "Northern Iowa", "Northern Kentucky", "Northwestern St.", 
                  "Northwestern 9", "Notre Dame", "Oakland 14", "Ohio", "Ohio St.", 
                  "Oklahoma", "Oklahoma St.", "Old Dominion", "Mississippi", "Nebraska Omaha", 
                  "Oral Roberts", "Oregon 11", "Oregon St.", "Pacific", "Penn St.", 
                  "Penn", "Pepperdine", "Pittsburgh", "Portland", "Portland St.", 
                  "Prairie View A&M", "Presbyterian", "Princeton", "Providence", "Purdue 1", 
                  "Purdue Fort Wayne", "Quinnipiac", "Radford", "Rhode Island", "Rice", 
                  "Richmond", "Rider", "Robert Morris", "Rutgers", "Southeastern Louisiana", 
                  "SIU Edwardsville", "SMU", "Sacramento St.", "Sacred Heart", "Saint Joseph's", 
                  "Saint Louis", "Saint Mary's 5", "Saint Peter's 15", "Sam Houston St.", "Samford 13", 
                  "San Diego St. 5", "San Diego", "San Francisco", "San Jose St.", "Santa Clara", 
                  "Seattle", "Seton Hall", "Siena", "South Alabama", "South Carolina 6", 
                  "South Carolina St.", "USC Upstate", "South Dakota", "South Dakota St. 15", "South Florida", 
                  "Southeast Missouri St.", "Southern Illinois", "Southern", "Southern Miss", "Southern Utah", 
                  "St. Bonaventure", "Saint Francis", "St. John's", "St. Thomas", "Stanford", 
                  "Stephen F. Austin", "Stetson 16", "Stonehill", "Stony Brook", "Syracuse", 
                  "TCU 9", "Tarleton St.", "Temple", "Tennessee St.", "Tennessee Tech", 
                  "Tennessee 2", "Texas A&M 9", "Texas A&M Commerce", "Texas A&M Corpus Chris", "Texas 7", 
                  "Texas Southern", "Texas St.", "Texas Tech 6", "The Citadel", "Toledo", 
                  "Towson", "Troy", "Tulane", "Tulsa", "UAB 12", 
                  "Albany", "UC Davis", "UC Irvine", "UC Riverside", "UC San Diego", 
                  "UC Santa Barbara", "UCF", "UCLA", "Connecticut 1", "Illinois Chicago", 
                  "Louisiana Monroe", "UMBC", "UMass Lowell", "UNC Asheville", "UNC Greensboro", 
                  "UNC Wilmington", "UNLV", "USC", "UT Arlington", "Tennessee Martin", 
                  "UT Rio Grande Valley", "UTEP", "UTSA", "Utah St. 8", "Utah Tech", 
                  "Utah", "Utah Valley", "VCU", "VMI", "Valparaiso", 
                  "Vanderbilt", "Vermont 13", "Villanova", "Virginia 10", "Virginia Tech", 
                  "Wagner 16", "Wake Forest", "Washington", "Washington St. 7", "Weber St.", 
                  "West Georgia", "West Virginia", "Western Carolina", "Western Illinois", "Western Kentucky 15", 
                  "Western Michigan", "Wichita St.", "William & Mary", "Winthrop", "Wisconsin 5", 
                  "Wofford", "Wright St.", "Wyoming", "Xavier", "Yale 13", 
                  "Youngstown St."),
  stringsAsFactors = FALSE
)

# Function to map ESPN names to KenPom names using the lookup table
map_team_name <- function(espn_team) {
  # Search for the ESPN name in the name_mapping table
  mapped_name <- name_mapping$kenpom_name[match(espn_team, name_mapping$espn_name)]
  
  # If the name isn't found in the lookup table, return the original ESPN name
  if (is.na(mapped_name)) {
    return("NA")  # You can modify this to return a specific value, e.g., NA or "Unknown"
  } else {
    return(mapped_name)
  }
}
map_team_name("Mount St. Mary's")




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
 match <- get_daily_matchups(20240206)

# Function to get betting odds from ESPN for each game
get_odds <- function(gameID) {

  # If there are no odds, do something to prevent an error
  
  # Get Odds
  betting_odds <- tryCatch({
    espn_mbb_betting(game_id = gameID)
  }, error = function(e) {
    message("No betting information available for game ID: ", game_id)
    return(NULL)
  })
  
  print(betting_odds$game_id)
  # Check if betting_odds is NULL or if the matchups column exists
  if (is.null(betting_odds) || !("game_id" %in% colnames(betting_odds)) || any(is.na(betting_odds$game_id))) {
    message("No valid betting data or matchups column not found for the game.")
    return(NULL)
  }
  
  # Ensure betting_odds is a data frame and has no NA values
  if (!is.data.frame(betting_odds) || any(is.na(betting_odds))) {
    message("Betting data is not in a valid format or contains missing values.")
    return(NULL)
  }
  
  # Check if the betting_odds data frame has any rows
  if (nrow(betting_odds) == 0) {
    message("No betting data found for the game.")
    return(NULL)
  }
  
  # Only want pickcenter data frame
  betting_odds <- betting_odds$pickcenter
  
  # Filter to only keep rows where provider_name is 'consensus'
  consensus_odds <- betting_odds %>%
    filter(provider_name == "consensus")
  
  # If no consensus odds are found, return NULL
  if (nrow(consensus_odds) == 0) {
    message("No consensus betting data available.")
    return(NULL)
  }
  
  # Return the filtered odds data
  return(consensus_odds)
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
            verbatimTextOutput("prediction1"),
            
          )
        )
      ),
    
    
      # Second tab: About
      tabPanel("About", 
               h3("About this App"),
               p("This app predicts the scores of college basketball games.")
      ),
      
      # Third tab: About
      tabPanel("Simulated Games vs Sportsbook Lines", 
               sidebarLayout(
                 sidebarPanel(
                   dateInput("date", "Select Date", value = Sys.Date()),
                   actionButton("simulate", "Simulate Today's Games")
                 ),
                 mainPanel(
                   tableOutput("simulated_scores")
                 )
               ))
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
      Predicted_Home = numeric(),
      Predicted_Away = numeric(),
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
      print(prediction)
      # Assuming you are working with predictions or team stats
      home_team_score <- prediction[1, 1]
      away_team_score <- prediction[1, 2]
      
      print(home_team_score)
      print(away_team_score)
      
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
      
      print(paste("Adding result for", home_team, "vs", away_team))
      
      # Add the results to the data frame
      results <- rbind(results, data.frame(
        Game = paste(home_team, "vs", away_team),
        Predicted_Home = home_team_score,
        Predicted_Away = away_team_score
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
