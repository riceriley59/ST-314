library(tidyverse)
library(openintro)

thorns <- read_csv(file.choose())

thorns_streak <- calc_streak(thorns$game)
glimpse(thorns_streak)
table(thorns_streak)

ggplot(thorns_streak, aes(x = length)) + 
  geom_histogram(bins = 5, fill = "green", color = "black") +
  labs(x = "Length of win Streaks", 
       y = "Frequency", 
       title = "Distribution of the Thorn's win streak length from 2019-2022")

dice_roll <- c(1, 2, 3, 4, 5, 6)

sim_fair_roll <- sample(dice_roll, size = 10, replace = TRUE)
table(sim_fair_roll)

sim_unfair_roll <- sample(dice_roll, size = 10, replace = TRUE, prob = c(0.5, 0.1, 0.1, 0.05, 0.2, 0.05))
table(sim_unfair_roll)

game_outcomes <- c('H', 'M')
sim_games <- sample(game_outcomes, size = 60, replace = TRUE)

sim_streak <- calc_streak(sim_games)
summary(sim_streak)
ggplot(sim_streak, aes(x = length)) + 
  geom_histogram(bins = 7, fill = "green", color = "black") +
  labs(x = "Length of win Streaks", 
       y = "Frequency", 
       title = "Distribution of the Sim win streak length 
       from 2019-2022")
