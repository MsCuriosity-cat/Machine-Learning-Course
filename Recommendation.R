# movielens data
library(tidyverse)
library(dslabs)
data("movielens")
options(digits = 3)

movielens %>% as_tibble()

movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)

tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)

tab %>% knitr::kable()

# not all movies are reviewed by every user
# plot the matrix to see how sparse the use-movie interactions are
users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# how many movies are getting a review
movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# how many users give reviews
movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# create a test set to assess the accuracy of the models we implement
library(caret)
set.seed(755, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

# Make sure we don¡¯t include users and movies in the test set 
# that do not appear in the training set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# create a loss function
# in this case residual mean square error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Least square estimation of mu
mu_hat <- mean(train_set$rating)
mu_hat

# predict all unknown ratings with ¦Ì-hat we obtain the following RMSE
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

# if you plug in any other number, you get a higher RMSE
predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

# create a results table with this naive approach
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

# movie effects
# fit <- lm(rating ~ as.factor(userId), data = movielens)
# different movies are rated differently
# hence we are adding a bias b_i that represents rating bias of ith movie
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# We can see that these estimates vary substantially:
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

# Let's see how much our prediction improves once we include bias
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
RMSE(predicted_ratings, test_set$rating)

# User effects
# Let¡¯s compute the average rating for user u
# for those that have rated over 100 movies
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))
# compute an approximation by computing mu_hat and b_i
# and estimate b_hat_u as an avegae of y_u_i - mu_hat_b_hat_i
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#> `summarise()` ungrouping output (override with `.groups` argument)

# We can now construct predictors and see how much the RMSE improves
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()
