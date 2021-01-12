library(tidyverse)
library(rpart)
library(dslabs)
library(caret)
data("tissue_gene_expression")

tge <- as.data.frame(tissue_gene_expression)
set.seed(1991, sample.kind = "Rounding")

#  fit a classification tree to the tissue_gene_expression dataset
train_rpart <- train(y ~ ., method = "rpart", data = tge,
             tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))

# Plot the accuracies to report the results of the best model
ggplot(train_rpart, highlight = T)

# there are only 6 placentas in the dataset
# rpart requires 20 observations before splitting a node
# Rerun the analysis 
# with the argument control = rpart.control(minsplit = 0)

train_rpart <- train(y ~., method = "rpart", data = tge,
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                     control = rpart.control(minsplit = 0))
confusionMatrix(train_rpart)

# Plot the tree from the best fitting model of the analysis
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# try to predict the tissue type with even fewer genes 
# using a Random Forest
# to permit small nodesize to grow
# use nodesize as 1
set.seed(1991, sample.kind = "Rounding")

fit_rf <- train(x, y, method = "rf", data = tge, nodesize = 1,
                tuneGrid = data.frame(mtry = seq(50, 200, 25)))

which.max(fit_rf$results$Accuracy)
fit_rf$results$mtry[ind]

# Use the function varImp() on the output of train()
# and save it to an object called imp
imp <- varImp(fit_rf)
imp

# The rpart() model we ran above in Q2 produced a tree that used just seven predictors.
# the following syntax can be used to extract the tree names
tree_terms <- as.character(unique(train_rpart$finalModel$frame$var[!(train_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

# Calculate the variable importance in the rf 
# for these seven predictors and examine where they rank
gene_imp <- as.data.frame(imp$importance)
view(gene_imp)
imp$importance
