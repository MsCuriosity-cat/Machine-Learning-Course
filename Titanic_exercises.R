 # Libraries and data
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# set seed to 42
set.seed(42, sample.kind = "Rounding")

y <- titanic_clean$Survived
# produce train and teset sets based on the survived column
test_index <- createDataPartition(y, times = 1, p = 0.2,list = FALSE)
train_set <- titanic_clean[-test_index,]
test_set <- titanic_clean[test_index,]

# How many observations are in the training set?
dim(train_set)

# How many observations are in the test set?
dim(test_set)

# What proportion of individuals in the training set survived?
mean(train_set$Survived == "1")

# Baseline prediction by guessing the outcome
set.seed(3, sample.kind = "Rounding")
y_hat <- sample(c('0','1'), length(test_index), replace = T ) %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)

# What proportion of training set females survived?
f_surv <- train_set %>% filter(Sex == "female") 
mean(f_surv$Survived == "1")

# What proportion of training set males survived?
m_surv <- train_set %>% filter(Sex == "male") 
mean(m_surv$Survived == "1")

# predict the survival of individuals using sex
y_hat <- ifelse(test_set$Sex == "female", "1", "0") %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)

# Predicting survival by passenger class
train_set %>%
  group_by(Pclass) %>%
  summarise(survival_rate = mean(Survived == "1")) %>%
  select(Pclass, survival_rate)

# predict survival using passenger class
y_hat <- ifelse(test_set$Pclass == 1, "1", "0") %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)

# Use the training set to group passengers by both sex and passenger clas
# Which sex and class combinations were more likely to survive than die?
train_set %>%
  group_by(Sex, Pclass) %>%
  summarise(survival_rate = mean(Survived == "1")) %>%
  select(Sex, Pclass, survival_rate)

# predict survival of test set using sex and pclass
y_hat <- ifelse(test_set$Sex == "female" & (test_set$Pclass == 1 | test_set$Pclass == 2),
                "1", "0") %>% factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)

# Confusion matrix - sex model
y_hat <- ifelse(test_set$Sex == "female", "1", "0") %>%
  factor(levels = levels(test_set$Survived))
cm_sex <- confusionMatrix(data = y_hat, reference = test_set$Survived)
sensitivity(data = y_hat, reference = test_set$Survived)
specificity(data = y_hat, reference = test_set$Survived)

#confusion matrix - pclass model
y_hat <- ifelse(test_set$Pclass == 1, "1", "0") %>%
  factor(levels = levels(test_set$Survived))
cm_pclass <- confusionMatrix(data = y_hat, reference = test_set$Survived)
sensitivity(data = y_hat, reference = test_set$Survived)
specificity(data = y_hat, reference = test_set$Survived)

# confusion matrix - sex + pclass model
y_hat <- ifelse(test_set$Sex == "female" & (test_set$Pclass == 1 | test_set$Pclass == 2),
                "1", "0") %>% factor(levels = levels(test_set$Survived))
cm_both <- confusionMatrix(data = y_hat, reference = test_set$Survived)
sensitivity(data = y_hat, reference = test_set$Survived)
specificity(data = y_hat, reference = test_set$Survived)

# Which model has the highest balanced accuracy?
cm_sex
cm_pclass
cm_both

# F1 scores 
# sex model
y_hat <- ifelse(test_set$Sex == "female", "1", "0") %>%
  factor(levels = levels(test_set$Survived))
F_1_sex <- F_meas(data = y_hat, reference = test_set$Survived)
F_1_sex

# pclass model
y_hat <- ifelse(test_set$Pclass == 1, "1", "0") %>%
  factor(levels = levels(test_set$Survived))
F_1_pc <- F_meas(data = y_hat, reference = test_set$Survived)
F_1_pc

# both
y_hat <- ifelse(test_set$Sex == "female" & (test_set$Pclass == 1 | test_set$Pclass == 2),
                "1", "0") %>% factor(levels = levels(test_set$Survived))
F_1_both <- F_meas(data = y_hat, reference = test_set$Survived)

F_1_both

# Survival by fare - LDA and QDA
set.seed(1, sample.kind = "Rounding")
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
y_hat_lda <- predict(train_lda, test_set)
confusionMatrix(data = y_hat_lda, reference = test_set$Survived)$overall["Accuracy"]

train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
y_hat_qda <- predict(train_qda, test_set)
confusionMatrix(data = y_hat_qda, reference = test_set$Survived)$overall["Accuracy"]


# Survival by age - glm 
set.seed(1, sample.kind = "Rounding")
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
y_hat_glm <- predict(train_glm, test_set)
confusionMatrix(data = y_hat_glm, reference = test_set$Survived)$overall["Accuracy"]

# Set the seed to 1
# Train a logistic regression model with
# four predictors: sex, class, fare, and age
set.seed(1, sample.kind = "Rounding")
train_fourp <- train(Survived ~ Sex + Pclass + Age + Fare,
                     method = "glm", data = train_set)
y_hat_fourp <- predict(train_fourp, test_set)
confusionMatrix(data = y_hat_fourp, reference = test_set$Survived)$overall["Accuracy"]

# Train a logistic regression model using all predictors
set.seed(1, sample.kind = "Rounding")
train_allp <- train(Survived ~ ., method = "glm", data = train_set)
y_hat <- predict(train_allp, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

# Train a kNN model on the training set
# Try tuning with k = seq(3, 51, 2)
set.seed(6, sample.kind = "Rounding")
train_knn <- train(Survived ~ ., method = "knn", data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))

# What is the optimal value of the number of neighbors k?
train_knn$bestTune

# Plot the kNN model to investigate the relationship 
# between the number of neighbors and accuracy on the training set.
ggplot(train_knn, highlight = T)

# What is the accuracy of the kNN model on the test set?
y_hat_knn <- predict(train_knn, test_set)
confusionMatrix(data = y_hat_knn, reference = test_set$Survived)$overall["Accuracy"]

# Cross Validation
set.seed(8, sample.kind = "Rounding")

# train a new kNN model with training control 10-fold cross-validation
# where each partition consists of 10% of the total
control <- trainControl(method = "cv", number = 10) 
train_knn_cv <- train(Survived ~ ., method = "knn", data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)
train_knn_cv$bestTune

y_hat_knn_cv <- predict(train_knn_cv, test_set)
confusionMatrix(data = y_hat_knn_cv, reference = test_set$Survived)$overall["Accuracy"]

# Classification tree model
set.seed(10, sample.kind = "Rounding")

# train a decision tree with the rpart method
# Tune the complexity parameter with cp = seq(0, 0.05, 0.002)
train_rpart <- train(Survived ~ ., method = "rpart", data = train_set,
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))

# What is the optimal value of the complexity parameter (cp)?
train_rpart$bestTune

# What is the accuracy of the decision tree model on the test set?
y_hat_rpart <- predict(train_rpart, test_set)
confusionMatrix(data = y_hat_rpart, 
                reference = test_set$Survived)$overall["Accuracy"]

# Classification tree model
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# predict if certain individuals would survive 
# based on the final decision tree model 
library(rattle)
fancyRpartPlot(train_rpart$finalModel)

# Random Foreest model
set.seed(14, sample.kind = "Rounding")
train_rf <- train(Survived ~ ., method = "rf", data = train_set,
          ntree = 100,tuneGrid = data.frame(mtry = seq(1:7)))

# What mtry value maximizes accuracy?
train_rf$bestTune

# What is the accuracy of the random forest model on the test set?

y_hat_rf <- predict(train_rf, test_set)
mean(y_hat_rf == test_set$Survived)

# Use varImp() on the random forest model object
# to determine the importance of various predictors to the random forest model
# What is the most important variable?
varImp(train_rf)
