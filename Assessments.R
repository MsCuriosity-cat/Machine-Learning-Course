library(tidyverse)
options(digits = 2)

# Predicting disease prevalence probability in a population
# set seed
set.seed(1, sample.kind = "Rounding")
disease <- sample(c(0,1), size = 1e6, replace = TRUE, prob = c(0.98, 0.02))
test <- rep(NA, 1e6)
view(test)

test[disease == 0] <- sample(c(0,1), size = sum(disease == 0), prob = c(0.90, 0.10), replace = T)
test[disease == 1] <- sample(c(0,1), size = sum(disease == 1), prob = c(0.15, 0.85), replace = T)


# the probability that a test is positive
mean(test == 1)

# the probability that an individual has the disease if the test is negative
mean(disease[test == 0])

# the probability that you have the disease if the test is positive
mean(disease[test == 1])

# If a patient's test is positive, by how many times does that increase their risk of having the disease
mean(disease[test == 1])/0.02

## Compute conditional probs for being male in the heights data
library(dslabs)
data("heights")

# Round the heights to the closest inch
# Plot the estimated conditional probability
heights %>% mutate(height = round(height)) %>%
  group_by(height) %>%
  summarise(p = mean(sex == "Male")) %>% 
  qplot(height, p, data = .)

# use the quantile  0.1,0.2,бн,0.9  and 
# the cut() function to assure each group has the same number of points
ps <- seq(0,1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps))) %>%
  group_by(height) %>%
  summarise(p = mean(sex == "Male")) %>%
  qplot(height, p, data = .)

# You can generate data from a bivariate normal distrubution 
# using the MASS package using the following code
Sigma <- 9*matrix(c(1, 0.5,0.5, 1), 2, 2)
view(Sigma)

dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
view(dat)
# make a quick plot of the data
plot(dat)

# estimate the conditional expectations and make a plot
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = T)) %>% 
  group_by(g) %>%
  summarise(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
