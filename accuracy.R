library(lubridate)
library(dplyr)



#concat all game data

files <- list.files("game_data/",full.names = T)

game_data <- c( 
                readRDS("game_data/gamedata_2020.RDS"),
                readRDS("game_data/gamedata_2021.RDS"),
                readRDS("game_data/gamedata_2022.RDS"),
                readRDS("game_data/gamedata_2023.RDS")
                
                )
#Remove play off game, anything after Sept. 30

dates <- unlist(lapply(game_data,function(x) x$date ) )

m <- month(dates)

dates_to_keep <- m < 10


game_data <- game_data[dates_to_keep]



# Calculate accuracy score function
accuracy_score <- function(outcomes, predictions) {
  mean(outcomes == predictions) * 100
}

# Initialize lists to store data
outcomes <- vector()
predictions <- vector()
probabilities <- vector()

for (i in 1:length(game_data)) {
  
  
  d <- game_data[[i]]
  
  # Extract relevant data and handle incomplete cases
  tryCatch({
    moneyline <- as.integer(d$home_moneyline)
    home_score <- as.integer(d$home_score)
    away_score <- as.integer(d$away_score)
    
   # if (is.na(moneyline) || is.na(home_score) || is.na(away_score)) {
    if (length(moneyline) ==0 || length(home_score) ==0|| length(away_score) ==0) {  
    next
    }
    
    # Exclude missing moneyline
    if (is.na(moneyline)) {
      next
    }
    
    
    # Exclude tossups from calculations
    if (moneyline == 100  ) {
      next
    }
    
    # Convert moneyline odds to implied probabilities
    if (moneyline < 0) {
      probabilities <- c(probabilities, (-moneyline / (-moneyline + 100) ) )
    } else if (moneyline > 100) {
      probabilities <- c(probabilities, ( 100 / (moneyline + 100) ) )
    }
    
    #True if home team wins
    outcomes <- c(outcomes, home_score > away_score)
    
    #True if home team predicted to win
    predictions <- c(predictions, moneyline < 0)
  }, error = function(e) {
    # Handle any errors (e.g., incomplete data)
    next
  })
}

cat(sprintf("Sportsbook accuracy (excluding tossups): %.2f%%\n", accuracy_score(outcomes, predictions)))

