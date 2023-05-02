# load packages
library(tidyverse)
library(BART)
library(here)

# set seed
set.seed(1)

# load data
diabetes <- read_csv(here("data", "diabetes_binary_health_indicators_BRFSS2015.csv"))

# this data is too big for BART to be efficient, so we subset 5000 rows
diabetes <- diabetes %>% slice_sample(n= 5000)

# set the feature and target sets
x <- diabetes %>% 
    select(-c(Diabetes_binary)) %>%
    as.data.frame()
y <- diabetes %>% 
    select(Diabetes_binary)%>%
    deframe()

# set the smoker variable
x_treat1 <- x %>%
    mutate(Smoker = 1)

x_treat0 <- x %>%
    mutate(Smoker = 0)

# BART fit
bartfit <- gbart(x.train = x, y.train = y, type = 'pbart', ntree = 50,printevery = 1000)

# predictions
predict1 <- predict(bartfit, x_treat1)
predict0 <- predict(bartfit, x_treat0)

# ate estimation
e_ate <- mean(predict1$prob.test.mean - predict0$prob.test.mean)
print(e_ate)
