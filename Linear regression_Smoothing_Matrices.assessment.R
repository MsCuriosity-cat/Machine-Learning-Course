library(tidyverse)
library(caret)
options(digits = 3)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind = "Rounding")
predictions <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = F)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat -test_set$y)^2))
})

mean(predictions)
sd(predictions)


# write a function that takes a size n and builds a dataset using the code above
# but wthout the set seed 
dummydata_maker <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  predictions <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = F)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat -test_set$y)^2))
  })
  return (c(mean(predictions),sd(predictions)))
  }

n <- c(100, 500, 1000, 5000, 10000)
view(n)

set.seed(1, sample.kind = "Rounding")
sapply(n, dummydata_maker)

# repeat the exercises from the first one
# this time the correlation between x and y larger
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind = "Rounding")

predictions <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = F)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat -test_set$y)^2))
})

mean(predictions)
sd(predictions)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# correlation between x_1 and x_2
cor(dat)

test_index <- createDataPartition(dat$y, times = 1, p =0.5, list = F)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1, train_set)
y_hat <- predict(fit, test_set)

sqrt(mean((y_hat - test_set$y)^2))


fit <- lm(y ~ x_2, train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat -test_set$y)^2))


set.seed(1, sample.kind = "Rounding")
fit <- lm(y ~ x_1 + x_2, train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

# since the code I wrote is not giving right anser
# here is the solution from the course
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)

sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)

y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))


# now the x_1 and x_2 are highly correlated
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(dat$y, times = 1, p =0.5, list = F)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1, train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))


fit <- lm(y ~ x_2, train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat -test_set$y)^2))


set.seed(1, sample.kind = "Rounding")
fit <- lm(y ~ x_1 + x_2, train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

## Logistic Regression assessment
# Define a dataset using the following code
# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
view(dat)

dat$train %>% ggplot(aes(x, colour = y)) + geom_density()

# set seed to 1
set.seed(1, sample.kind = "Rounding")
mu_1 <- seq(0,3,len = 25)

v <- c(1:25)
ass.dats <- map(v, function(x) {return (make_data())})
view(ass.dats)

# perform logistic regression on each of the 25 datasets
predict_accuracy <- function(data){
  fit_glm <- glm(y ~ x, data$train, family = "binomial")
  p_hat <- predict(fit_glm, data$test)
  y_hat <- factor(ifelse(p_hat > 0.5, 1, 0))
  predicted.accuracy <- confusionMatrix(data = y_hat, reference = data$test$y)$overall[["Accuracy"]]
  return(predicted.accuracy)
}

ass.dat.accuracy <- sapply(ass.dats, predict_accuracy)
ass.dat.accuracy <- as.data.frame(ass.dat.accuracy, col.names = "Accuracy")
view(ass.dat.accuracy)
ass.dat.accuracy <- cbind(mu_1, ass.dat.accuracy)

# plot the predicted accuracy against mu 1
ass.dat.accuracy %>% ggplot(aes(mu_1,ass.dat.accuracy )) +
  geom_point() +
  ylim(0.1, 0.5)

# The answer code given by the teachers
set.seed(1) #if you are using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)
  