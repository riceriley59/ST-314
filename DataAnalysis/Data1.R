install.packages("tidyverse")
install.packages("openintro")

library(tidyverse)
library(openintro)

data(present)

glimpse(present)

present$year

ggplot(data = present, aes(x = year, y = boys)) + 
  geom_line()

ggplot(data = present, aes(x = year, y = girls)) + 
  geom_line()

present <- present %>% mutate(total = boys + girls)

present %>% 
  arrange(desc(total))

loan50 <- read_csv(file.choose())

glimpse(loan50)

loan50$annual_income

options(scipen=999)

ggplot(loan50, aes(x = annual_income)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(x = "Annual Income(USD)", 
       y = "Frequency", 
       title = "Distribution of Annual Income")

ggplot(loan50, aes(x = annual_income)) + 
  geom_boxplot() +
  labs(x = "Annual Income(USD)", 
       title = "Distribution of Annual Income") + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

summary(loan50$annual_income)
sd(loan50$annual_income)
IQR(loan50$annual_income)

ggplot(loan50, aes(x = annual_income, y = loan_status)) + 
  geom_boxplot() +
  labs(x = "Annual Income (USD)", 
       y = "Loan Status", 
       title = "Distribution of people's annual income based
                           on loan status")
table(loan50$loan_status)
prop.table(table(loan50$loan_status))

ggplot(loan50, aes(x = loan_status)) + 
  geom_bar() + 
  labs(x = "Loan Status",
       title = "Distribution of current loan statuses for 50
                  Lending Club Loans")
