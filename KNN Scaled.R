set.seed(1)
library(dplyr)
library(class)
library(pROC)

red_wine <- read.csv("winequality-red.csv", header = TRUE, sep = ";")
red_wine["type"] = 1
white_wine <- read.csv("winequality-white.csv", header = TRUE, sep = ";")
white_wine["type"] = 0
wine <- bind_rows(red_wine, white_wine)
wine$good <- ifelse(wine$quality > 5, 1, 0)

a <- sample(1:6497, 1299)
b <- sample(setdiff(1:6497, a), 1299)
c <- sample(setdiff(1:6497, c(a, b)), 1299)
d <- sample (setdiff(1:6497, c(a, b, c)), 1300)
e <- setdiff(1:6497, c(a, b, c, d))

wine_knn_cv <- function(k) {
  train <- c(b, c, d, e)
  
  tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                              residual.sugar = wine$residual.sugar[train],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                              sulfates = wine$sulphates[train],
                              alcohol = wine$alcohol[train]))
  
  knn_a <- knn(tscaled,
               scale(data.frame(volatile.acidity = wine$volatile.acidity[a],
                                residual.sugar = wine$residual.sugar[a],
                                free.sulfur.dioxide = wine$free.sulfur.dioxide[a],
                                total.sulfur.dioxide = wine$total.sulfur.dioxide[a],
                                sulfates = wine$sulphates[a],
                                alcohol = wine$alcohol[a]),
                     center = attr(tscaled, "scaled:center"),
                     scale = attr(tscaled, "scaled:scale")),
               wine$good[train], k = k)
  
  error_a <- mean(knn_a != wine$good[a])
  
  train <- c(a, c, d, e)
  
  tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                              residual.sugar = wine$residual.sugar[train],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                              sulfates = wine$sulphates[train],
                              alcohol = wine$alcohol[train]))
  
  knn_b <- knn(tscaled,
               scale(data.frame(volatile.acidity = wine$volatile.acidity[b],
                                residual.sugar = wine$residual.sugar[b],
                                free.sulfur.dioxide = wine$free.sulfur.dioxide[b],
                                total.sulfur.dioxide = wine$total.sulfur.dioxide[b],
                                sulfates = wine$sulphates[b],
                                alcohol = wine$alcohol[b]),
                     center = attr(tscaled, "scaled:center"),
                     scale = attr(tscaled, "scaled:scale")),
               wine$good[train], k = k)
  
  error_b <- mean(knn_b != wine$good[b])
  
  train <- c(a, b, d, e)
  
  tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                              residual.sugar = wine$residual.sugar[train],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                              sulfates = wine$sulphates[train],
                              alcohol = wine$alcohol[train]))
  
  knn_c <- knn(tscaled,
               scale(data.frame(volatile.acidity = wine$volatile.acidity[c],
                                residual.sugar = wine$residual.sugar[c],
                                free.sulfur.dioxide = wine$free.sulfur.dioxide[c],
                                total.sulfur.dioxide = wine$total.sulfur.dioxide[c],
                                sulfates = wine$sulphates[c],
                                alcohol = wine$alcohol[c]),
                     center = attr(tscaled, "scaled:center"),
                     scale = attr(tscaled, "scaled:scale")),
               wine$good[train], k = k)
  
  error_c <- mean(knn_c != wine$good[c])
  
  train <- c(a, b, c, e)
  
  tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                              residual.sugar = wine$residual.sugar[train],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                              sulfates = wine$sulphates[train],
                              alcohol = wine$alcohol[train]))
  
  knn_d <- knn(tscaled,
               scale(data.frame(volatile.acidity = wine$volatile.acidity[d],
                                residual.sugar = wine$residual.sugar[d],
                                free.sulfur.dioxide = wine$free.sulfur.dioxide[d],
                                total.sulfur.dioxide = wine$total.sulfur.dioxide[d],
                                sulfates = wine$sulphates[d],
                                alcohol = wine$alcohol[d]),
                     center = attr(tscaled, "scaled:center"),
                     scale = attr(tscaled, "scaled:scale")),
               wine$good[train], k = k)
  
  error_d <- mean(knn_d != wine$good[d])
  
  train <- c(a, b, c, d)
  
  tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                              residual.sugar = wine$residual.sugar[train],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                              sulfates = wine$sulphates[train],
                              alcohol = wine$alcohol[train]))
  
  knn_e <- knn(tscaled,
               scale(data.frame(volatile.acidity = wine$volatile.acidity[e],
                                residual.sugar = wine$residual.sugar[e],
                                free.sulfur.dioxide = wine$free.sulfur.dioxide[e],
                                total.sulfur.dioxide = wine$total.sulfur.dioxide[e],
                                sulfates = wine$sulphates[e],
                                alcohol = wine$alcohol[e]),
                     center = attr(tscaled, "scaled:center"),
                     scale = attr(tscaled, "scaled:scale")),
               wine$good[train], k = k)
  
  error_e <- mean(knn_e != wine$good[e])
  
  mean(error_a, error_b, error_c, error_d, error_e)
}

train <- c(b, c, d, e)

tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                            residual.sugar = wine$residual.sugar[train],
                            free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                            total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                            sulfates = wine$sulphates[train],
                            alcohol = wine$alcohol[train]))

knn_a <- knn(tscaled,
             scale(data.frame(volatile.acidity = wine$volatile.acidity[a],
                              residual.sugar = wine$residual.sugar[a],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[a],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[a],
                              sulfates = wine$sulphates[a],
                              alcohol = wine$alcohol[a]),
                   center = attr(tscaled, "scaled:center"),
                   scale = attr(tscaled, "scaled:scale")),
             wine$good[train], k = 1)

cm <- table(knn_a, wine$good[a])

TP <- cm[2, 2]
TN <- cm[1, 1]
FP <- cm[2, 1]
FN <- cm[1, 2]

accuracy_a  <- (TP + TN) / sum(cm)
precision_a <- TP / (TP + FP)
recall_a <- TP / (TP + FN)

error_a <- mean(knn_a != wine$good[a]); error_a

train <- c(a, c, d, e)

tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                            residual.sugar = wine$residual.sugar[train],
                            free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                            total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                            sulfates = wine$sulphates[train],
                            alcohol = wine$alcohol[train]))

knn_b <- knn(tscaled,
             scale(data.frame(volatile.acidity = wine$volatile.acidity[b],
                              residual.sugar = wine$residual.sugar[b],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[b],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[b],
                              sulfates = wine$sulphates[b],
                              alcohol = wine$alcohol[b]),
                   center = attr(tscaled, "scaled:center"),
                   scale = attr(tscaled, "scaled:scale")),
             wine$good[train], k = 1)

cm <- table(knn_b, wine$good[b])

TP <- cm[2, 2]
TN <- cm[1, 1]
FP <- cm[2, 1]
FN <- cm[1, 2]

accuracy_b  <- (TP + TN) / sum(cm)
precision_b <- TP / (TP + FP)
recall_b <- TP / (TP + FN)

error_b <- mean(knn_b != wine$good[b])

train <- c(a, b, d, e)

tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                            residual.sugar = wine$residual.sugar[train],
                            free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                            total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                            sulfates = wine$sulphates[train],
                            alcohol = wine$alcohol[train]))

knn_c <- knn(tscaled,
             scale(data.frame(volatile.acidity = wine$volatile.acidity[c],
                              residual.sugar = wine$residual.sugar[c],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[c],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[c],
                              sulfates = wine$sulphates[c],
                              alcohol = wine$alcohol[c]),
                   center = attr(tscaled, "scaled:center"),
                   scale = attr(tscaled, "scaled:scale")),
             wine$good[train], k = 1)

cm <- table(knn_c, wine$good[c])

TP <- cm[2, 2]
TN <- cm[1, 1]
FP <- cm[2, 1]
FN <- cm[1, 2]

accuracy_c  <- (TP + TN) / sum(cm)
precision_c <- TP / (TP + FP)
recall_c  <- TP / (TP + FN)

error_c <- mean(knn_c != wine$good[c])

train <- c(a, b, c, e)

tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                            residual.sugar = wine$residual.sugar[train],
                            free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                            total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                            sulfates = wine$sulphates[train],
                            alcohol = wine$alcohol[train]))

knn_d <- knn(tscaled,
             scale(data.frame(volatile.acidity = wine$volatile.acidity[d],
                              residual.sugar = wine$residual.sugar[d],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[d],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[d],
                              sulfates = wine$sulphates[d],
                              alcohol = wine$alcohol[d]),
                   center = attr(tscaled, "scaled:center"),
                   scale = attr(tscaled, "scaled:scale")),
             wine$good[train], k = 1)

cm <- table(knn_d, wine$good[d])

TP <- cm[2, 2]
TN <- cm[1, 1]
FP <- cm[2, 1]
FN <- cm[1, 2]

accuracy_d <- (TP + TN) / sum(cm)
precision_d <- TP / (TP + FP)
recall_d <- TP / (TP + FN)

error_d <- mean(knn_d != wine$good[d])

train <- c(a, b, c, d)

tscaled <- scale(data.frame(volatile.acidity = wine$volatile.acidity[train],
                            residual.sugar = wine$residual.sugar[train],
                            free.sulfur.dioxide = wine$free.sulfur.dioxide[train],
                            total.sulfur.dioxide = wine$total.sulfur.dioxide[train],
                            sulfates = wine$sulphates[train],
                            alcohol = wine$alcohol[train]))

knn_e <- knn(tscaled,
             scale(data.frame(volatile.acidity = wine$volatile.acidity[e],
                              residual.sugar = wine$residual.sugar[e],
                              free.sulfur.dioxide = wine$free.sulfur.dioxide[e],
                              total.sulfur.dioxide = wine$total.sulfur.dioxide[e],
                              sulfates = wine$sulphates[e],
                              alcohol = wine$alcohol[e]),
                   center = attr(tscaled, "scaled:center"),
                   scale = attr(tscaled, "scaled:scale")),
             wine$good[train], k = 1)

cm <- table(knn_e, wine$good[e])

TP <- cm[2, 2]
TN <- cm[1, 1]
FP <- cm[2, 1]
FN <- cm[1, 2]

accuracy_e  <- (TP + TN) / sum(cm)
precision_e <- TP / (TP + FP)
recall_e <- TP / (TP + FN)

error_e <- mean(knn_e != wine$good[e])

mean(error_a, error_b, error_c, error_d, error_e)
mean(accuracy_e, accuracy_b, accuracy_c, accuracy_d, accuracy_e)
mean(precision_a, precision_b, precision_c, precision_d, precision_e)
mean(recall_a, recall_b, recall_c, recall_d, recall_e)

cv_error <- numeric(100)
for (i in 1:100) {
  cv_error[i] <- wine_knn_cv(i)
}

min(cv_error)
which.min(cv_error)

# this minimum error rate comes when k = 1

par(mfrow = c(2, 3))

plot(roc(wine$good[a], as.numeric(knn_a)), main = "ROC Curve", xlab = "False Positive Rate", ylab =  "True Positive Rate")

plot(roc(wine$good[b], as.numeric(knn_b)), main = "ROC Curve", xlab = "False Positive Rate", ylab =  "True Positive Rate")

plot(roc(wine$good[c], as.numeric(knn_c)), main = "ROC Curve", xlab = "False Positive Rate", ylab =  "True Positive Rate")

plot(roc(wine$good[d], as.numeric(knn_d)), main = "ROC Curve", xlab = "False Positive Rate", ylab =  "True Positive Rate")

plot(roc(wine$good[e], as.numeric(knn_e)), main = "ROC Curve", xlab = "False Positive Rate", ylab =  "True Positive Rate")

#Finding the area under the curve for the ROC curve.
auc(roc(wine$good[a], as.numeric(knn_a)))
auc(roc(wine$good[b], as.numeric(knn_b)))
auc(roc(wine$good[c], as.numeric(knn_c)))
auc(roc(wine$good[d], as.numeric(knn_d)))
auc(roc(wine$good[e], as.numeric(knn_e)))
par(mfrow = c(2, 3))

boxplot(wine$volatile.acidity, wine$good, ylab = "Volatile Acidity", xlab = "Wine Quality")

boxplot(wine$residual.sugar, wine$good, ylab = "Residual Sugar", xlab = "Wine Quality")

boxplot(wine$total.sulfur.dioxide, wine$good, ylab = "Total Sulfur Dioxide", xlab = "Wine Quality")

boxplot(wine$free.sulfur.dioxide, wine$good, ylab = "Free Sulfur Dioxide", xlab = "Wine Quality")

boxplot(wine$sulphates, wine$good, ylab = "Sulphates", xlab = "Wine Quality")

boxplot(wine$alcohol, wine$good, ylab = "Alcohol", xlab = "Wine Quality")
