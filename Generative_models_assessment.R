# Create a dataset of samples from just cerebellum and hippocampus
# and a predictor matrix with 10 randomly selected columns
# Q1
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Use the train() function to estimate the accuracy of LDA
fit_lda <- train(x, y, method = "lda")
fit_lda$results

# Look at the fitted model 
# by looking at the finalModel component of the result of train()
# Plot the mean vectors against each other 
# determine which predictors (genes) appear to be driving the algorithm
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Repeat the exercise in Q1 with QDA
fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]

# Which TWO genes drive the algorithm when using QDA 

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Re-run LDA with preProcess = "center"
fit_lda <- train(x, y, method = "lda", preProcess = "center")

# Which TWO genes drive the algorithm after performing the scaling?
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Repeat the LDA analysis from Q5 but using all tissue types
set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

# What is the accuracy using LDA?
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]
