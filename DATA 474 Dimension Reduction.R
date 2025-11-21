library(dplyr)
library(car)
library(pls)

set.seed(1)

red_wine <- read.csv("winequality-red.csv", header = TRUE, sep = ";")
red_wine["type"] = 1
white_wine <- read.csv("winequality-white.csv", header = TRUE, sep = ";")
white_wine["type"] = 0
wine <- bind_rows(red_wine, white_wine)
wine$good <- ifelse(wine$quality > 5, 1, 0)

train <- sample(6497, 5198)
test <- wine[-train,]

log_mod <- glm(good ~ volatile.acidity + residual.sugar + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol, data = wine, family = binomial, subset = train)
vif(log_mod)

glm.probs <- predict(log_mod, test, type = "response")
glm.pred <- rep(0, 1299)
glm.pred[glm.probs > 0.5] <- 1

mean(glm.pred != test$good)

pls.fit <- plsr(good ~ volatile.acidity + residual.sugar + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol, data = wine , subset = train , scale = TRUE, validation = "CV")
summary(pls.fit)

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