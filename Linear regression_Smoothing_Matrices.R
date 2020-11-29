library(tidyverse)
library(HistData)
library(caret)
options(digits = 2)

galton_heights <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = F)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)^2
avg



# mean square error formula
mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat -test_set$son)^2)

# we can also use predict function
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

## code: regression for a categorical outcome
library(dslabs)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding")

test_index <- createDataPartition(y, times = 1, p = 0.5, list = F)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height) == 66) %>%
  summarise(y_hat = mean(sex == "Female"))

heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
  lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)

y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]

## Logistic regression
# the linrear regression for categorical variables can become negative
heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

# fit logistic regeression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data =., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response") 

tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(sex == "Female"))

logistic_curve <-data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))

tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat))

y_hat_logit <- ifelse(p_hat_logit >0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

## Code: case study 2 or 7
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1))]
titles <- c("smallest", "largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row = 1:28, Column = 1:28) %>%
    mutate(label = titles[i],
           value = mnist$train$images[is[i],])
})

tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill = value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, colour = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2))]
titles <- c("smallest", "largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row = 1:28, Column = 1:28) %>%
    mutate(label = titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill = value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

fit_glm <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm >0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall[["Accuracy"]]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill = p)) +
  geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colours = c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks = c(0.5), colour = "black")

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>% 
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2, z = p_hat, fill = p_hat)) +
  geom_raster() + 
  scale_fill_gradientn(colours = c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks = c(0.5), colour = "black")

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

# Code : Smoothing
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_smooth()

# Code: Bin smoothing and kernels
# bin smmothers
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points = day, kernel = "box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, colour = "grey") +
  geom_line(aes(day, smooth), colour = "red")

# kernel
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points = day, kernel = "normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, colour = "grey") +
  geom_line(aes(day, smooth), colour = "red")

# Local Weighted regress
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, colour = "grey") +
  geom_line(aes(day, smooth), colour = "red")

# comparison of fitting parabolas and lines with loess
total_days <- diff(range(polls_2008$day))
span <- 28/total_days

fit_1 <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)
fit_2 <- loess(margin ~ day, span= span, data = polls_2008)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, colour = "grey") +
  geom_line(aes(day, smooth_1), colour = "red", lty = 2)+
  geom_line(aes(day, smooth_2), colour = "red", lty = 1)

# Beware of default smoothing parameters
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()

# the line in the above graph is not appropriate as it is not optimal for the data
# change it conveniently as follows:
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.15, method.args = list(degree = 1))
