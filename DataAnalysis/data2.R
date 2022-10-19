library('tidyverse')
library('openintro')

thorns <- read_csv(file.choose())
glimpse(thorns)

thorns_streak <- calc_streak(thorns$game)
glimpse(thorns_streak)

ggplot(thorns_streak, aes(x = length)) + 
  geom_histogram(bins = 5, fill = "green", color = "black") +
  labs(x = "Length of Win Streaks", 
       y = "Frequency", 
       title = "Distribution of the Thorns’ win streak lengths from 2019-2022")

mean(thorns_streak$length)

