library(dslabs)
library(dplyr)
library(lubridate)
data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass", "online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% filter(type == "inclass" & sex == "Female") %>% nrow()
dat %>% filter(type == "inclass") %>% nrow()
26/39

dat %>% filter(type == "online" & sex == "Female") %>% nrow()
dat %>% filter(type == "online") %>% nrow()
42/111

# predict the sex of each class based on class type
mean(y_hat == dat$sex)
dat %>% group_by(type) %>% summarise(mean(sex == "Female"))
y_hat <- ifelse(x == "inclass", "Female", "Male")
y_hat <- factor(y_hat)
mean(y == y_hat)

# Confusion Matrix
# tabulate each combination of prediction and actual value
table(y, y_hat)

# what is the sensitivity of this prediction
sensitivity(y_hat,y)
specificity(y_hat, y)
mean(y == "Female")

## Part ii
library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set seed (2)
set.seed(2, sample.kind = "Rounding")

# can also use the following to create test and training data
test_index <- createDataPartition(y, times = 1, p = 0.5, list = F)

# line of code
test <- iris[test_index,]
train <- iris[-test_index,]



# find the singular feature in the dataset that yields the greatest overall accuracy when predicting species
train %>% group_by(Species) %>% summarise(mean(Sepal.Length), sd(Sepal.Length))
train %>% group_by(Species) %>% summarise(mean(Sepal.Width), sd(Sepal.Width))
cutoff.sl <- seq(5.43, 6.10, 0.1)
accuracy.sl <- map_dbl(cutoff.sl, function(x){
   y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
       factor(levels = levels(train$Species))
     mean(y_hat == train$Species)
   })

cutoff.sw <- seq(3.09, 3.12, 0.1)
accuracy.sw <- map_dbl(cutoff.sl, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

train %>% group_by(Species) %>% summarise(mean(Petal.Length), sd(Petal.Length))
cutoff.pl <- seq(4.67, 6.09, 0.1)
accuracy.pl <- map_dbl(cutoff.pl, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

train %>% group_by(Species) %>% summarise(mean(Petal.Width), sd(Petal.Width))
cutoff.pw <- seq(1.50, 2.28, 0.1)
accuracy.pw <- map_dbl(cutoff.pw, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

# find the accuracy of the test results using the smart cutoff value
best_cuttoff <- cutoff.pl[which.max(accuracy.pl)]
best_cuttoff

best_cuttoff.pw <- cutoff.pw[which.max(accuracy.pw)]
best_cuttoff.pw

y_hat <- ifelse(test$Petal.Length > best_cuttoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)

# Which feature best optimizes our overall accuracy?
test %>% group_by(Species) %>% summarise(mean(Sepal.Width), sd(Sepal.Width))
cutoff.sw.test <- seq(3.11, 3.22, 0.1)
accuracy.sw.test <- map_dbl(cutoff.sl.test, function(x){
  y_hat <- ifelse(test$Sepal.Width> x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

test %>% group_by(Species) %>% summarise(mean(Sepal.Length), sd(Sepal.Length))
cutoff.sl.test <- seq(6.62, 7.21, 0.1)
accuracy.sl.test <- map_dbl(cutoff.sl.test, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

test %>% group_by(Species) %>% summarise(mean(Petal.Width), sd(Petal.Width))
cutoff.pw.test <- seq(1.49, 2.26, 0.1)
accuracy.pw.test <- map_dbl(cutoff.pw.test, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

test %>% group_by(Species) %>% summarise(mean(Petal.Length), sd(Petal.Length))
cutoff.pl.test <- seq(4.71, 6.1, 0.1)
accuracy.pl.test <- map_dbl(cutoff.pl.test, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

# perform some explanatory analysis
plot(iris,pch=21,bg=iris$Species)

# optimize the cutoff based on both petal width and petal length 
# then calculate the overall accuracy of the test data
best_cuttoff
best_cuttoff.pw

y_hat <- ifelse(test$Petal.Length > best_cuttoff | test$Petal.Width > best_cuttoff.pw, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
mean(y_hat == test$Species)
