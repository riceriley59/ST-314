install.packages("infer")
library(tidyverse)
library(infer)


population <- tibble(
  hip = c(rep("Squeaking", 17500), rep("No squeaking", 232500))
)
ggplot(population, aes(x = hip)) + 
  geom_bar() + 
  labs(x = "", y = "Count", 
       title = "Ceramic hip patients that develop squeaking")
population %>%
  count(hip) %>%
  mutate(p = n/sum(n))


samp1 <- population %>%
  sample_n(200)
ggplot(samp1, aes(x = hip)) + 
  geom_bar() + 
  labs(x = "", y = "Count", 
       title = "Ceramic hip patients that develop squeaking")
samp1 %>% 
  count(hip) %>%
  mutate(p_hat = n/sum(n))


samp2 <- population %>%
  sample_n(200)
ggplot(samp2, aes(x = hip)) + 
  geom_bar() + 
  labs(x = "", y = "Count", 
       title = "Ceramic hip patients that develop squeaking")
samp2 %>% 
  count(hip) %>%
  mutate(p_hat = n/sum(n))


sample_props_small <- population %>%
  rep_sample_n(size = 100, reps = 10, replace = TRUE) %>%
  count(hip) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(hip == "Squeaking")

sample_props_small

sample_props200 <- population %>%
  rep_sample_n(size = 200, reps = 10000, replace = TRUE) %>%
  count(hip) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(hip == "Squeaking")

sample_props200

pnorm(0.04, 0.0701, 0.018)
(1 - pnorm(0.10, 0.0701, 0.018))
pnorm(0.10, 0.0701, 0.018) - pnorm(0.05, 0.0701, 0.018)
