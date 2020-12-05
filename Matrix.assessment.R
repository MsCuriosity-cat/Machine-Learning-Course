# generate a 100 by 10 matrix of randomly generated normal numbers 
# assign the matrix to x
x<- matrix(rnorm(100*10), 100, 10)

# dimension of x
dim(x)
nrow(x)
ncol(x)

# add scalar 1 to row 1, scalar 2 to row 2, and so on to matrix x
x <- x + seq(nrow(x))

# or
x <- sweep(x, 1, 1:nrow(x), "+")

# add scalar 1 to col 1, scalar 2 to col 2, and so on to matrix x
x <- sweep(x, 2, 1:ncol(x), FUN = "+")

# For each observation in the mnist training data
# compute the proportion of pixels that are in the grey area
# grey area is defined as values between 50 and 205, but not including 50 and 205
options(digits = 3)
library(dslabs)
x <- mnist$train$images
mean(x > 50 & x < 205)

     