# The createResample() function can be used to create bootstrap samples
# we can create the indexes for 10 bootstrap samples for 
# the mnist_27 dataset like this

library(dslabs)
library(caret)
data(mnist_27)

set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

# How many times do 3, 4, and 7 appear in the first resampled index?
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

# What is the total number of times that 3 appears 
# in all of the resampled indexes?
x = sapply(indexes, function(ind){
  sum(ind == 3)
  })
sum(x)
# Generate a random dataset using the following code

y <- rnorm(100, 0, 1)

# Estimate the 75th quantile, which we know is qnorm(0.75)
# with the sample quantile: quantile(y, 0.75)

quantile(y, 0.75)

# set the seed to 1

set.seed(1, sample.kind = 'Rounding')

# perform a Monte Carlo simulation with 10,000 repetitions
# generating the random dataset and estimating the 75th quantile each time
B <- 10^4
M <- replicate(B, {
  y <- rnorm(100, 0, 1)
  desired_q <- quantile(y, 0.75)
})
# What is the expected value 
# and standard error of the 75th quantile?
mean(M)
sd(M)

# set.seed(1) # if R 3.5 or earlier

set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

# Bootstrap

indices <- createResample(y, 10^4)

# expected value of the 75th quantile
Q = sapply(indices, function(ind){
  quantile(y[ind], 0.75)
})

mean(Q)
sd(Q)
