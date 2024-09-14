library(shiny)
library(rsconnect)
library(oddsapiR)

# get API key

# pull odds

# Kenpom data from web for predictions
library(readxl)
kenpom <- read_excel("KenPom.xlsx", skip = 1, sheet = 1)

# getting rid of labels in the middle of the table
kenpom$AdjO <- as.numeric(as.character(kenpom$AdjO))
kenpom$AdjD <- as.numeric(as.character(kenpom$AdjD))
kenpom$AdjT <- as.numeric(as.character(kenpom$AdjT))


# collect user data
# want to make it as they choose from a list
teams <- c(kenpom$Team)

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



# get game data from an api


ui <- fluidPage(
  titlePanel("College Basketball Score Prediction"),
  
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
}

shinyApp(ui = ui, server = server)
