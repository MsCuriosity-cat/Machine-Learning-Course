library(tidyverse)
library(caret)
library(dslabs)
library(purrr)


# define the outcome and predictors
y <- heights$sex
x <- heights$height
# generate training and test datasets
# Set seed to 1
set.seed(1, sample.kind = 'Rounding')
test_index <- createDataPartition(y, times = 1, p = 0.5, list = F)
heights_test <- heights[test_index,]
heights_train <- heights[-test_index,]

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = heights_train, k = k)
  y_hat <- predict(fit, heights_train, type = "class")
  ftrain <- F_meas(data = y_hat, reference = heights_train$sex)
  y_hat <- predict(fit, heights_test, type = 'class')
  ftest <- F_meas(data = y_hat, reference = heights_test$sex)

  
  return(ftest)
})

# What is the max value of F_1?
ks[which.max(F_1)]

## perform the knn like above on the tissue gene expression dataset
data("tissue_gene_expression") 

# generate training and test datasets
# Set seed to 1
set.seed(1, sample.kind = 'Rounding')
test_index <- createDataPartition(y, times = 1, p = 0.5, list = F)
heights_test <- heights[test_index,]
heights_train <- heights[-test_index,]