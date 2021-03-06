# Create a simple dataset where the outcome grows 0.75 units 
# on average for every increase in a predictor, using this code
library(rpart)
library(tidyverse)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# use rpart() to fit a regression tree and saves the result to fit
fit <- rpart(y ~ ., data = dat)

# visualise the splits, i.e., the tree shape
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# make a scatter plot of y versus x
# along with the predicted values based on the fit
dat %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

#  run Random Forests instead of a regression tree
library(randomForest)
fit <- randomForest(y ~ x, data = dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#  Use the plot() function to see if the Random Forest from Q4 has converged
  # or if we need more trees
  plot(fit)

# the default values for the Random Forest 
# result in an estimate that is too flexible (unsmooth) 
# Re-run the Random Forest but this time 
  # with a node size of 50 and a maximum of 25 nodes
  # Remake the plot
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = "red")
plot(fit)  
