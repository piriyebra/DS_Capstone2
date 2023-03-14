# Install all needed libraries if not present
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(caret)) install.packages("caret") 
if(!require(gam)) install.packages("gam") 


# Loading all needed libraries
library(tidyverse)
library(caret)
library(gam)

# Retriving the Wine Quality dataset 
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
dat <- read.csv(url, sep=";")

# Analizing dataset
str(dat)
sum(is.na(dat))

# Dividing the original data set in train / test sets. 10 percent of the data set goes to the test set. 90 percent to the train set   
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(dat$quality,times=1, p=.1,  list = FALSE)
wine_test <- dat[test_index, ]
wine_train <- dat[-test_index, ]

#RMSE function
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Analizing train set
wine_train %>% ggplot(aes(x = quality)) + geom_histogram()
plot(wine_train)

# Generalized Linear Model - GLM algorith
train <- train(quality ~ ., method= "glm", data = wine_train)
# Predict quality
y_hat <- round(predict(train, wine_test))
# Model's Confusion matrix
confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))
# Model's Accuracy
accuracy <- confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$overall[["Accuracy"]]
# Model's RMSE
RMSE <- RMSE(wine_test$quality, y_hat)
# Model RMSE and Accuracy are stored in a data set for model comparison
results <- data.frame(model="GLM", RMSE=RMSE, Accuracy=accuracy)
# Model results are stored in a data set for model comparison via a graph
t <- as.data.frame(confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$table)
graphs <- data.frame(model="GLM", graph = t)
# Display model RMSE and Accuracy
results

# Generalized Linear Model - GLM algorith - just 5 variables
train <- train(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + alcohol, method= "glm", data = wine_train)
# Predict quality
y_hat <- round(predict(train, wine_test))
# Model's Confusion matrix
confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))
# Model's Accuracy
accuracy <- confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$overall[["Accuracy"]]
# Model's RMSE
RMSE <- RMSE(wine_test$quality, y_hat)
# Model RMSE and Accuracy are stored in a data set for model comparison
results <- results %>% add_row(model="GLM 5 vars", RMSE=RMSE, Accuracy=accuracy)
# Model results are stored in a data set for model comparison via a graph
t <- as.data.frame(confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$table)
graphs <- graphs %>% add_row(model = "GLM 5 vars", graph.Prediction = t$Prediction, graph.Reference = t$Reference, graph.Freq = t$Freq)
# Display model RMSE and Accuracy
results

# KNN algorithm - default
train <- train(quality ~ ., method = "knn", data=wine_train)
# Predict quality
y_hat <- round(predict(train, wine_test))
# Model's Confusion matrix
confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))
# Model's Accuracy
accuracy <- confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$overall[["Accuracy"]]
# Model's RMSE
RMSE <- RMSE(wine_test$quality, y_hat)
# Model RMSE and Accuracy are stored in a data set for model comparison
results <- results %>% add_row(model="KNN", RMSE=RMSE, Accuracy=accuracy)
# Model results are stored in a data set for model comparison via a graph
t <- as.data.frame(confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$table)
graphs <- graphs %>% add_row(model = "KNN", graph.Prediction = t$Prediction, graph.Reference = t$Reference, graph.Freq = t$Freq)
# Display model RMSE and Accuracy
results

# KNN algorithm - tuned
train <- train(quality ~ ., method = "knn", data=wine_train, tuneGrid = data.frame(k=seq(9, 71,2)))
# KNN best tune
ggplot(train, highlight = TRUE)
train$bestTune
# Predict quality
y_hat <- round(predict(train, wine_test))
# Model's Confusion matrix
confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))
# Model's Accuracy
accuracy <- confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$overall[["Accuracy"]]
# Model's RMSE
RMSE <- RMSE(wine_test$quality, y_hat)
# Model RMSE and Accuracy are stored in a data set for model comparison
results <- results %>% add_row(model="KNN_T", RMSE=RMSE, Accuracy=accuracy)
# Model results are stored in a data set for model comparison via a graph
t <- as.data.frame(confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$table)
graphs <- graphs %>% add_row(model = "KNN_T", graph.Prediction = t$Prediction, graph.Reference = t$Reference, graph.Freq = t$Freq)
# Display model RMSE and Accuracy
results

# Loess algorithm
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
train <- train(quality ~ ., 
                     method = "gamLoess", 
                     tuneGrid=grid,
                     data = wine_train)
# Best tune
ggplot(train, highlight = TRUE)
train$bestTune
# Predict quality
y_hat <- round(predict(train, wine_test))
# Model's Confusion matrix
confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))
# Model's Accuracy
accuracy <- confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$overall[["Accuracy"]]
# Model's RMSE
RMSE <- RMSE(wine_test$quality, y_hat)
# Model RMSE and Accuracy are stored in a data set for model comparison
results <- results %>% add_row(model="Loess", RMSE=RMSE, Accuracy=accuracy)
# Model results are stored in a data set for model comparison via a graph
t <- as.data.frame(confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$quality))$table)
graphs <- graphs %>% add_row(model = "Loess", graph.Prediction = t$Prediction, graph.Reference = t$Reference, graph.Freq = t$Freq)
# Display model RMSE and Accuracy
results

#Analysis
results
# Best RMSE
results$model[which.min(results$RMSE)]
# Best Accuracy
results$model[which.max(results$Accuracy)]
# Graph to compare results among models
graphs %>% ggplot(aes(x=graph.Reference, y=graph.Prediction, size=graph.Freq, color=model)) + geom_jitter() + geom_abline(aes(intercept=0, slope=1)) + facet_wrap(vars(model))

