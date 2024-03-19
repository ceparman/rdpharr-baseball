library(ggplot2)
library(dplyr)

# Prepare data
brier_score_loss <- function(y_test, y_proba) {
  mean((y_test - y_proba) ^ 2)
}

# Data

bins = 10

data <- data.frame(outcomes = outcomes, 
             predictions = predictions, 
             probabilities = probabilities,
             name = "SportsBook"
)

data <- data |> mutate(pbin=round(probabilities,1))


# Compute fraction_of_positives and mean_predicted_value

#fraction_of_positives <- NULL

calibration <- data |>   mutate( match = as.integer(outcomes == predictions) ) |>
                        group_by(pbin) |> 
                summarise(fraction_of_positives = mean(outcomes),
                          mean_predicted_value = mean(probabilities),
                          n=n()) |> ungroup() 

ggplot(data=calibration,mapping=aes(x=pbin, y=fraction_of_positives,label=n)) + 
  geom_line(col="red") +   geom_point(col="red") +
          geom_abline(slope = 1)  + geom_text(hjust=1.5)

ggplot(data, aes( probabilities)) + geom_histogram(bins = 15)

mean_predicted_value <- NULL

mean_predicted_value <- mean(data$probabilities,na.rm = T)
#for (i in 1:length(data$outcomes)) {
#  fraction_of_positives <- c(fraction_of_positives, sum(data$outcomes[[i]]) / length(data$outcomes[[i]]))
#  mean_predicted_value <- c(mean_predicted_value, mean(data$probabilities[[i]]))
#}




# Compute Brier score
brier <- brier_score_loss(data$outcomes, data$probabilities)

