library(tidyverse)
library(infer)

microbrew <- read_csv(file.choose())
glimpse(microbrew$abv)

ggplot(microbrew, aes(x = abv)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(x = "Beer with ABV amount", 
       y = "Frequency", 
       title = "Distribution of ABV in craft beers")

samp_mean <- mean(microbrew$abv)
samp_sd <- sd(microbrew$abv)

t_stat <- (samp_mean - 5)/(samp_sd/sqrt(75))

p_val <- 2*(1 - pt(t_stat, 74))

t.test(microbrew$abv, mu=5, alternative="two.sided", conf.level=1-0.01)


population <- read_csv(file.choose())

mu_co2 <- mean(population$CombCO2)
n <- 45
sample <- population %>% 
  sample_n(size=n)

samp_mean <- mean(sample$CombCO2)
samp_sd <- sd(sample$CombCO2)

t_stat <- (samp_mean - mu_co2)/(samp_sd/sqrt(45))
p_val <- 2*(1 - pt(t_stat, 74))

sample_means45 <- population %>%
  rep_sample_n(size = n, reps = 10000, replace = TRUE) %>%
  group_by(replicate) %>%
  summarise(mean = mean(CombCO2), sd = sd(CombCO2))

ggplot(sample_means45, aes(x = t)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(x = "T-values of Samples", 
       y = "Frequency", 
       title = "Distribution of T-values From Our Population Samples")

sample_means45 <- sample_means45 %>%
  mutate(t = (mean-mu_co2)/(sd/sqrt(n)))

sample_means45 <- sample_means45 %>%
  mutate(p_val = 2*(1-pt(abs(t), 44)))

ggplot(sample_means45, aes(x = p_val)) + 
  geom_histogram(binwidth = 0.05, 
                 boundary = 0, 
                 color = "black",
                 fill = c(rep("#D55E00", 1), rep("#999999", 19))) + 
  labs(x = "p-values", 
       title = "Distribution of p-values from 10000 \nRandom Samples from the Population")

mean(sample_means45$p_val <= 0.05) * 100
