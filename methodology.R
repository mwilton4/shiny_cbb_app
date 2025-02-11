

# import kenpom data and transform appropriately
# map names from espn to kenpom and from oddsAPI to kenpom

# get the daily matchups from ESPN
# get odds for all matchups from the day (1 request) - store in table
# find the odds from those matchups from oddsAPI

# apply prediction function to matchups
# using spreads and predictions, give recommendation who to bet on

# at the end of the day, add in the scores from every matchup
# indicate if bet would've been a win or loss

# now, the columns should be home_team, away_team, home predicted score, away predicted score, spread, bet recommendation, edge margin, actual home score, actual away score, bet result 
# each row is a game
# add each row to a SQL database at the end of each day automatically (BEFORE KENPOM UPDATES)

# now, we can run querys on the SQL database
# examples:
# find overall win percentage
# find if a higher edge is more profitable
# find home-field advantage numbers (which home teams does the model consistently underrate compared to the sportsbook?)

# this is where we can make it an interface
# allow the user to customize views to analyze how the model performs
# kind of like slicerdicer
