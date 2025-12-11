library(dplyr)
library(car)
library(pls)
library(cvms)
library(ggplot2)
library(pROC)

set.seed(1)

red_wine <- read.csv("winequality-red.csv", header = TRUE, sep = ";")
red_wine["type"] = 1
white_wine <- read.csv("winequality-white.csv", header = TRUE, sep = ";")
white_wine["type"] = 0
wine <- bind_rows(red_wine, white_wine)
wine$good <- ifelse(wine$quality > 5, 1, 0)

hist(wine$quality, main = "Histogram of Wine Quality", xlab = "Wine Quality")

train <- sample(6497, 5198)
test <- wine[-train,]

sum(wine$good) / nrow(wine)

log_mod <- glm(good ~ volatile.acidity + residual.sugar + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol, data = wine, family = binomial, subset = train)
vif(log_mod)

glm.probs <- predict(log_mod, test, type = "response")
glm.pred <- rep(0, 1299)
glm.pred[glm.probs > 0.5] <- 1

mean(glm.pred != test$good)

pls.fit <- plsr(good ~ volatile.acidity + residual.sugar + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol, data = wine , subset = train , scale = TRUE, validation = "CV")
summary(pls.fit)

plot(RMSEP(pls.fit), legendpos = "topright", xlab = "Components", main = "CV Error by Number of PLS Components")

scores <- scores(pls.fit)

scores_train <- data.frame(good = wine[train,]$good, c1 = scores[,1], c2 = scores[,2], c3 = scores[,3])
logit_pls <- glm(good ~ c1 + c2 + c3, family = binomial, data = scores_train)
summary(logit_pls)

test_scores <- predict(pls.fit, newdata = test, type = "scores")
scores_test <- data.frame(c1 = test_scores[,1], c2 = test_scores[,2], c3 = test_scores[,3])

pls.probs <- predict(logit_pls, scores_test, type = "response")
pls.pred <- rep(0, 1299)
pls.pred[pls.probs > 0.5] <- 1

mean(pls.pred != test$good)

cm <- table(pls.pred, test$good)

TP <- cm[2, 2]
TN <- cm[1, 1]
FP <- cm[2, 1]
FN <- cm[1, 2]

accuracy  <- (TP + TN) / sum(cm)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

conf_matrix <- confusion_matrix(targets = factor(test$good, levels = c(0, 1), labels = c("Bad", "Good")),
                                factor(pls.pred, levels = c(0, 1), labels = c("Bad", "Good")))

plot_confusion_matrix(conf_matrix) +
  ggplot2::labs(
    x = "True Value",
    y = "Predicted Value",
    title = "PLS Logistic Confusion Matrix"
  ) +
  theme(
    plot.title.position = "panel",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(size = 12), # Adjust x-axis labels size
    axis.text.y = ggplot2::element_text(size = 12), # Adjust y-axis labels size
    legend.text = ggplot2::element_text(size = 20), # Adjust legend text size
    legend.title = ggplot2::element_text(size = 16) # Adjust legend title size
  )

logit_pls_2 <- glm(good ~ c1 + c2, family = binomial, data = scores_train)

test_scores_2 <- predict(pls.fit, newdata = test, type = "scores")

pls.probs_2 <- predict(logit_pls_2, scores_test[, -3], type = "response")
pls.pred_2 <- rep(0, 1299)
pls.pred_2[pls.probs_2 > 0.5] <- 1

mean(pls.pred_2 != test$good)

plot(roc(test$good, as.numeric(pls.pred)), main = "PLS Logistic ROC Curve", xlab = "False Positive Rate", ylab =  "True Positive Rate")

auc(roc(test$good, as.numeric(pls.pred)))

a <- sample(1:6497, 1299)
b <- sample(setdiff(1:6497, a), 1299)
c <- sample(setdiff(1:6497, c(a, b)), 1299)
d <- sample (setdiff(1:6497, c(a, b, c)), 1300)
e <- setdiff(1:6497, c(a, b, c, d))

folds <- list(a = a, b = b, c = c, d = d, e = e)

plot(seq(0,1, length.out = 5),seq(1,0,length.out = 5),xlim = c(1.2,-0.2),ylim = c(0,1), type = "l", col = "grey",
     main = "5-Fold CV ROC Curve for PLS Logistic Regression",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)"
)

cv <- c()
accuracy <- c()
precision <- c()
recall <- c()
auc <- c()

for (i in 1:5) {
  test <- folds[[i]]
  train <- setdiff(1:nrow(wine), folds[[i]])
  pls.fit <- plsr(good ~ volatile.acidity + residual.sugar + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol, data = wine , subset = train, scale = TRUE)
  scores <- scores(pls.fit)
  scores_train <- data.frame(good = wine[train,]$good, c1 = scores[,1], c2 = scores[,2], c3 = scores[,3])
  logit_pls <- glm(good ~ c1 + c2 + c3, family = binomial, data = scores_train)
  test_scores <- predict(pls.fit, newdata = wine[test,], type = "scores")
  scores_test <- data.frame(c1 = test_scores[,1], c2 = test_scores[,2], c3 = test_scores[,3])
  pls.probs <- predict(logit_pls, scores_test, type = "response")
  pls.pred <- rep(0, length(test))
  pls.pred[pls.probs > 0.5] <- 1
  cv <- c(cv, wine[test,]$good != pls.pred)
  cm <- table(pls.pred, wine[test,]$good)
  TP <- cm[2, 2]
  TN <- cm[1, 1]
  FP <- cm[2, 1]
  FN <- cm[1, 2]
  accuracy  <- c(accuracy, (TP + TN) / sum(cm))
  precision <- c(precision, TP / (TP + FP))
  recall <- c(recall, TP / (TP + FN))
  roc_obj <- roc(as.numeric(factor(wine[test,]$good, levels = c(0,1))), as.numeric(factor(pls.pred, levels = c(0, 1))))
  lines(roc_obj, col = i + 1)
  auc <- c(auc, auc(roc(wine[test,]$good, as.numeric(pls.pred))))
}
