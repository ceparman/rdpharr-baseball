library(lubridate)
library(dplyr)
library(purrr)



#concat all game data

files <- list.files("game_data/",full.names = T)

game_data <- rbind( 
               readRDS("game_data/gamedata_2020.RDS"),
               readRDS("game_data/gamedata_2021.RDS"),
               readRDS("game_data/gamedata_2022.RDS"),
               readRDS("game_data/gamedata_2023.RDS")
                
                )







#remove any data with NA
game_data <- game_data |> drop_na()

#Remove play off game, anything after Sept. 30

game_data <- game_data |> 
             mutate( month = month(date),
                     year = year(date)) |> 
             filter(month < 10)



# Calculate accuracy score function
accuracy_score <- function(outcomes, predictions) {
  mean(outcomes == predictions) * 100
}



game_data <- game_data  |> 
              mutate_at(c("home_moneyline","home_score","away_score"),
                        as.integer) |>
              rename(moneyline = home_moneyline)  |> 
              drop_na()
    

# Exclude tossups from calculations

game_data <- game_data  |> filter(moneyline != 100)

# Convert moneyline odds to implied probabilities
game_data <- game_data  |> 
  mutate( probabilities = if_else( moneyline < 0,
                                   (-moneyline / (-moneyline + 100) ),
                                   ( 100 / (moneyline + 100) )
                                     ) ) |>
   mutate(outcomes =  home_score > away_score)  |>
   mutate(predictions = moneyline < 0 )



cat(sprintf("Sportsbook accuracy (excluding tossups): %.2f%%\n", 
            accuracy_score(game_data$outcomes, game_data$predictions)))

