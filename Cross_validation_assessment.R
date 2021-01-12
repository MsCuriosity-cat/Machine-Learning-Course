# Generate a set of random predictors and outcomes using the following code:
library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# Which code correctly performs this cross-validation?
fit <- train(x_subset, y, method = "glm")
fit$results


library(genefilter)
tt <- colttests(x, y)

# Which of the following lines of code correctly creates 
# a vector of the p-values called pvals?
pvals <- tt$p.value

# Create an index ind with the column numbers of the predictors
# that were "statistically significantly" associated with y
# Use a p-value cutoff of 0.01 to define "statistically significantly
ind <- which(pvals <= 0.01)
length(ind)

# redefine the subset x_subset using columns that show 
# statistically significant association with y
x_subset <- x[,ind]

# rerun cross validation on the redefined x_subset
# What is the accuracy now?
fit <- train(x_subset, y, method = "glm")
fit$results

# Re-run the cross-validation again, but this time using kNN
# try k = seq(101, 301, 25) grid for tuning the parameters
# Make a plot of the resulting accuracies.
fit <- train(x_subset, y, method = 'knn', tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

library(dslabs)
data("tissue_gene_expression")

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
fit$results
