library(dslabs)
data(tissue_gene_expression)

# This dataset includes a matrix x:
dim(tissue_gene_expression$x)

# The tissue type is stored in y:
table(tissue_gene_expression$y)

# Which of the following lines of code computes the Euclidean distance
# between each observation and stores it in the object d?
d <- dist(tissue_gene_expression$x)
class(d)
as.matrix(d[1:3])

# compare the distances between observations 1 and 2 (both cerebellum)
cb1 <- tissue_gene_expression$x[1,]
cb2 <- tissue_gene_expression$x[2,]

sqrt(crossprod(cb1 - cb2))

# observations 39 and 40 (both colon)
co1 <- tissue_gene_expression$x[39,]
co2 <- tissue_gene_expression$x[40,]

sqrt(crossprod(co1 - co2))

# observations 73 and 74 (both endometrium)
ed1 <- tissue_gene_expression$x[73,]
ed2 <- tissue_gene_expression$x[74,]

sqrt(crossprod(ed1 - ed2))

# compare the different tissue types
sqrt(crossprod(cb1 - co1))
sqrt(crossprod(cb2 - co2))
sqrt(crossprod(cb1 - ed1))
sqrt(crossprod(co1 - ed2))

# Make a plot of all the distances using the image() function 
# to see if the pattern you observed in Q2 is general

image(as.matrix(d))
