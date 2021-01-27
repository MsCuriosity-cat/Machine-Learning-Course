options(digits = 7)
library(tidyverse)

# An education expert is advocating for smaller schools
# The expert bases this recommendation on the fact that
# among the best performing schools, many are small schools
# simulate a dataset for 1000 schools
# let's simulate the number of students in each school, using the following code
# set.seed(1986) # if using R 3.5 or earlier
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent from size.
# This is the parameter we want to estimate in our analysis
# The true quality can be assigned using the following code:
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# We can see the top 10 schools using this code: 
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Now let's have the students in the school take a test
# There is random variability in test taking
# so we will simulate the test scores as normally distributed with the average determined
# by the school quality with a standard deviation of 30 percentage points
# This code will simulate the test scores:
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# What are the top schools based on the average score?
schools %>%
  arrange(desc(score)) %>%
  select(id, size, score) %>%
  slice(1:10)

# Compare the median school size to the median school size
# of the top 10 schools based on the score.

# What is the median school size overall?
median(schools$size)

#  What is the median school size of the of the top 10 schools based on the score?
schools %>%
  arrange(desc(score)) %>%
  select(id, size, score) %>%
  slice(1:10) %>%
  summarise(median_size = median(size))

# According to this analysis, it appears that small schools produce better test scores than large schools.
# Four out of the top 10 schools have 100 or fewer students.
# But how can this be? We constructed the simulation so that quality and size were independent.
# Repeat the exercise for the worst 10 schools
schools %>%
  arrange(score) %>%
  select(id, size, score) %>%
  slice(1:10) %>%
  .$size %>% 
  median()

# From this analysis, we see that the worst schools are also small.
# Plot the average score versus school size to see what's going on.
# Highlight the top 10 schools based on the true quality.
schools %>% 
  ggplot(aes(size, score))+
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank <= 10), col = 2)

#  several of the top 10 schools based on true quality
# are also in the top 10 schools based on the exam score
schools %>% 
  arrange(desc(score)) %>%
  select(id, size, score) %>%
  slice(1:10)

# use regularization to pick the best schools
# define the overall average for all schools, using the following code

overall <- mean(sapply(scores, mean))

#  define, for each school, how it deviates from that average
alpha <- 25
score_reg <- sapply(scores, function(x) overall + sum(x - overall)/(length(x) + alpha))
schools %>% 
  mutate(score_reg = score_reg) %>%
  arrange(desc(score_reg)) %>%
  slice(1:10)

# Using values of  ¦Á  from 10 to 250
# find the  ¦Á  that minimizes the RMSE
alphas <- seq(10, 250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

# Rank the schools based on the average obtained with the best alpha
alpha <- 135
score_reg <- sapply(scores, function(x) overall + sum(x - overall)/(length(x) + alpha))
schools %>% 
  mutate(score_reg = score_reg) %>%
  arrange(desc(score_reg)) %>%
  slice(1:10)

# A common mistake made when using regularization is shrinking values towards 0 that are not centered around 0
# if we don't subtract the overall average before shrinking, we actually obtain a very similar result
# Confirm this by re-running the code but without removing the overall mean
alphas <- seq(10, 250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
