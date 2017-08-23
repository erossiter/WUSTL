## Loading my cleaned data
rm(list = ls())
# library(plyr)
# library(parallel)
# library(doMC)
# library(glmnet)
# library(e1071)
setwd("~/dropbox/github/wustl/hw/hw4/")
load("../../StudentDrinking.RData")

## PART 1: Comparing Coefficients ---------------------------------------------------

## i.) Linear model
lm_alcohol <- lm(alcohol ~ ., data = data.frame(X))

## ii.) LASSO
lasso_alcohol <- cv.glmnet(x = X, y = alcohol, alpha = 1)

## iii.) Ridge
ridge_alcohol <- cv.glmnet(x = X, y = alcohol, alpha = 0)

## iv.) Elastic-net
## When alpha = .5 we are fitting an elastic-net model because
## we are balancing between the penalty's of lasso (a = 0) and
## ridge (a = 1)
en_alcohol <- cv.glmnet(x = X, y = alcohol, alpha = 0.5)

## v.) Comparing 'male' coefficients
coef_lasso <- coef(lasso_alcohol, s = lasso_alcohol$lambda)
coef_ridge <- coef(ridge_alcohol, s = ridge_alcohol$lambda)
coef_en <- coef(en_alcohol, s = en_alcohol$lambda)

plot(y = coef_lasso[which(rownames(coef_lasso) == "male"), ],
     x = lasso_alcohol$lambda,
     type = "l")
abline(h = lm_alcohol$coefficients["male"], lty = 2)

plot(y = coef_ridge[which(rownames(coef_ridge) == "male"), ],
      x = ridge_alcohol$lambda,
      type = "l")
abline(h = lm_alcohol$coefficients["male"], lty = 2)

plot(y = coef_en[which(rownames(coef_en) == "male"), ],
      x = en_alcohol$lambda,
      type = "l")
abline(h = lm_alcohol$coefficients["male"], lty = 2)


## PART 2: Cross-Validation, Super Learning, Ensembles ------------------------------------------

