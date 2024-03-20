# https://rdpharr.github.io/project_notes/baseball/benchmark/webscraping/brier/accuracy/calibration/machine%20learning/2020/09/20/baseball_project.html


library(rvest)
library(lubridate)

year <- 2020


url <- paste0('https://www.baseball-reference.com/leagues/MLB/',year,'-schedule.shtml')
page <- read_html(url)

# Extract all the H3 tags containing day names
days <- html_nodes(page, "h3") %>% html_text()

# Filter only the dates with year in them
days <- grep(as.character(year), days, value = TRUE)

# Parse the dates using lubridate
dates <- parse_date_time(days, "A, B d, Y")




cat(paste("Number of days MLB was played in", year,":", length(dates), "\n"))


game_data <- data.frame()

for (i in 1:length(dates)) {
 d <-dates[i]
  # get the web page with game data on it
#  game_day <- year(d) # Convert to Python datetime structure

  
    game_day <- paste0( year(d), "-",month(d), "-", day(d) )

  
  url <- paste0('https://www.covers.com/sports/mlb/matchups?selectedDate=', game_day)
page <- read_html(url)

# parse the games
scraped_games <- html_nodes(page, 'div.cmg_matchup_game_box')
for (g in scraped_games) {

  tryCatch({
  game <- data.frame(
            home_moneyline = html_attr(g, 'data-game-odd'),
            date = html_attr(g, 'data-game-date'),
            conference = html_attr(g, 'data-conference'),
            home_team =   html_attr(g, 'data-home-team-shortname-search'),
            away_team =   html_attr(g, 'data-away-team-shortname-search'),
            home_score = html_nodes(g, 'div.cmg_matchup_list_score_home') %>% html_text() %>% trimws(),
            away_score = html_nodes(g, 'div.cmg_matchup_list_score_away') %>% html_text() %>% trimws()
  )},
  
  error = function(e){
    
    game = NULL
  }
)

  game_data <- rbind(game_data,game)
}
 if (length(game_data) %% 500 == 0) {
   # show progress
   print(paste(Sys.time(), game_day, length(game_data)))
 }
}

cat("Done! Games downloaded:", length(game_data), "\n")



saveRDS(game_data,paste0("game_data/gamedata_",year,".RDS"))





