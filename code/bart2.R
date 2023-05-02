# load packages
library(tidyverse)
library(BART)
library(here)
library(tictoc)

# set seed
set.seed(1)

# load data
diabetes <- read_csv(here("data", "diabetes_binary_health_indicators_BRFSS2015.csv"))

# subset men over 30 for type 2 diabetes
diabetes2 <- diabetes %>% 
    filter((Sex == 1) & (Age >= 3)) %>% #age category 3 is 30-34
    select(-c(Sex))%>% # sex is no longer variable
    slice_sample(n= 5000) # subset data for computational efficiency

# set the feature and target sets
x2 <- diabetes2 %>% 
    select(-c(Diabetes_binary)) %>%
    as.data.frame()
y2 <- diabetes2 %>% 
    select(Diabetes_binary)%>%
    deframe()

# set the smoker variable
x2_treat1 <- x2 %>%
    mutate(Smoker = 1)

x2_treat0 <- x2 %>%
    mutate(Smoker = 0)
tic('BART')
# BART fit
bartfit <- gbart(x.train = x2, y.train = y2, type = 'pbart', ntree = 50, printevery = 1000)
toc()
# predictions
predict1 <- predict(bartfit, x2_treat1)
predict0 <- predict(bartfit, x2_treat0)

# ate estimation
e_ate <- mean(predict1$prob.test.mean - predict0$prob.test.mean)
print(e_ate)

