library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# matrices and vector notation
length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)

my_vectors <- 1:15

# fill the matrix by column
mat <- matrix(my_vectors, 5, 3)
mat

# fill by row
mat_t <- matrix(my_vectors, 3, 5, byrow = T)
mat_t

identical(t(mat),mat_t)
matrix(my_vectors, 5,5)

grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# to flip the image back :
image(1:28, 1:28, grid[, 28:1])

sums <- rowSums(x)
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>% 
  qplot(labels, row_averages, data = ., geom = "boxplot")

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)

# Code: filtering columns based on summaries
library(matrixStats)

sds <- colSds(x)
qplot(sds, bins = "30", colour = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

# extract columns and rows
x[, c(351, 352)]
x[c(2,3),]
new_x <- x[, colSds(x) >60]
dim(new_x)

class(x[,1])
dim(x[1,])

# preserve the matrix class
class(x[, 1, drop = FALSE])
dim(x[, 1, drop = F])

# indexing with matrices
# use logical operations with matrices
mat <- matrix(1:15, 5, 3)
as.vector(mat)
mat[mat >6 & mat < 12] <- 0

# histogram of all our predictor data
qplot(as.vector(x), bins = 30, colour = I("black"))

# if the values below 50 are smudges, we can covert them to 0
new_x <- x
new_x[new_x< 50] <- 0

# since the histogram suggest that the data mostly binary, i.e., 
# pixels are either with ink or without
# hence
# binarize the data using just matrix operations
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1

# we can also convert the data to a matrix of logicals
# and then coerce to numbers as follows
bin_X <- (x > 255/2) *1

# scale each row of a matrix 
(x - rowMeans(x)) / rowSds(x)

# scale each column
t(t(x) -colMeans(x))

# take each entry of a vector and substracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))

# divide by the standard deviation
x_mean_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")
