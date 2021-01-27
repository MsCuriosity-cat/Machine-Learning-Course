library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# Compute the number of ratings for each movie
# then plot it against the year the movie came out using a boxplot for each year
# Use the square root transformation on the y-axis (number of ratings)
movielens %>% 
  group_by(year) %>%
  summarise(n_rating = sqrt(n_distinct(userId))) %>%
  arrange(desc(n_rating)) %>%
  head(1) %>% 
  pull(year)

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Among movies that came out in 1993 or later,
# select the top 25 movies with the highest average number of ratings per year (n/year)
# caculate the average rating of each of them
# To calculate number of ratings per year, use 2018 as the end year
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

#  most frequently rated movies tend to have above average ratings
# This is not surprising: more people watch popular movies
# to confirm this, stratify the post-1993 movies by ratings per year and compute their average ratings.
movielens %>%
  filter(year >1993) %>%
  group_by(movieId) %>%
  summarise(n = n(), years = 2018 - first(year),
            title = title[1],
            avg_rating = mean(rating)) %>%
  mutate(rate = round(n/years, 1)) %>%
  ggplot(aes(avg_rating, rate)) +
  geom_point() +
  geom_abline()

# Create a new column date with the date
library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp))

# Compute the average rating for each week and plot this average against date
movielens %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(rating, date)) +
  geom_point() +
  geom_abline()
