# Use the training set to build a model with several of the models available from the caret package
# We will test out 10 of the most common machine learning models
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

# Run the following code to train the various models
library(caret)
library(dslabs)
library(tidyverse)
options(digits = 3)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# create a matrix of predictions for the test set
y_hat <- sapply(fits, function(model)
  predict(model, mnist_27$test), simplify = "matrix")

dim(y_hat)

# compute accuracy for each model on the test set

cm_glm <- confusionMatrix(as.factor(y_hat[,"glm"]), as.factor(mnist_27$test$y))$overall["Accuracy"]
cm_lda <- confusionMatrix(as.factor(y_hat[,"lda"]), as.factor(mnist_27$test$y))$overall["Accuracy"]
cm_bayes <- confusionMatrix(as.factor(y_hat[,"naive_bayes"]), as.factor(mnist_27$test$y))$overall["Accuracy"]
cm_knn <- confusionMatrix(as.factor(y_hat[,"knn"]), as.factor(mnist_27$test$y))$overall["Accuracy"]
cm_loess <- confusionMatrix(as.factor(y_hat[,"knn"]), as.factor(mnist_27$test$y))$overall["Accuracy"]
cm_nom <- confusionMatrix(as.factor(y_hat[,"multinom"]), as.factor(mnist_27$test$y))$overall["Accuracy"]
cm_qda <- confusionMatrix(as.factor(y_hat[,"qda"]), as.factor(mnist_27$test$y))$overall["Accuracy"]
cm_rf <- confusionMatrix(as.factor(y_hat[,"rf"]), as.factor(mnist_27$test$y))$overall["Accuracy"]
cm_adaboost <- confusionMatrix(as.factor(y_hat[,"adaboost"]), as.factor(mnist_27$test$y))$overall["Accuracy"]

# Report the mean accuracy across all models
mean(cm_adaboost, cm_bayes, cm_glm, cm_knn, cm_lda, cm_loess, cm_nom,
     cm_qda, cm_rf)

# Another way to do it 
acc <- colMeans(y_hat == mnist_27$test$y)
acc
mean(acc)

# build an ensemble prediction by majority vote 
# and compute the accuracy of the ensemble
rowAcc <- rowMeans(y_hat == 7)
Votes <- ifelse(rowAcc > 0.5, 7, 2)
Votes
sum(Votes == 2)
sum(Votes == 7)
acc2 <- confusionMatrix(data = as.factor(Votes), as.factor(mnist_27$test$y))$overall["Accuracy"]

# How many of the individual methods do better than the ensemble?
which(acc > acc2)

# Obtain the minimum accuracy estimates
# from cross validation with the training data for each model
# save the accuracy estimates in an object

glm_acc <- min(fits[["glm"]]$results$Accuracy)
lda_acc <- min(fits[["lda"]]$results$Accuracy)
bay_acc <- min(fits[["naive_bayes"]]$results$Accuracy)
svm_acc <- min(fits[["svmLinear"]]$results$Accuracy)
knn_acc <- min(fits[["knn"]]$results$Accuracy)
loess_acc <- min(fits[["gamLoess"]]$results$Accuracy)
multi_acc <- min(fits[["multinom"]]$results$Accuracy)
qda_acc <- min(fits[["qda"]]$results$Accuracy)
rf_acc <- min(fits[["rf"]]$results$Accuracy)
ada_acc <- min(fits[["adaboost"]]$results$Accuracy)

mean(glm_acc, lda_acc, bay_acc, svm_acc, knn_acc, loess_acc, multi_acc,
     qda_acc, rf_acc, ada_acc)

# Another way to do it 
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

# only consider the methods with a minimum accuracy estimate of greater than or equal to 0.8
# when constructing the ensemble
ind <- acc_hat >= 0.8
y_hat <- y_hat[, -c(1, 2, 3, 4, 7, 9)]
view(y_hat)

rowAcc <- rowMeans(y_hat == 7)
Votes <- ifelse(rowAcc > 0.5, 7, 2)
Votes
sum(Votes == 2)
sum(Votes == 7)
acc3 <- confusionMatrix(data = as.factor(Votes), as.factor(mnist_27$test$y))$overall["Accuracy"]
acc3
