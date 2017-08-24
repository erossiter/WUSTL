## Loading my cleaned data
rm(list = ls())
library(glmnet)
library(randomForest)
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

pdf("male_coefs.pdf", height = 4, width = 8)
layout(matrix(1:3, nrow = 1))
plot(y = coef_lasso[which(rownames(coef_lasso) == "male"), ],
     x = lasso_alcohol$lambda,
     type = "l",
     ylim = c(0,1),
     ylab = "Male Coef",
     xlab = "Lambda used in Model Fit",
     main = "LASSO")
abline(h = lm_alcohol$coefficients["male"], lty = 2)

plot(y = coef_ridge[which(rownames(coef_ridge) == "male"), ],
     x = ridge_alcohol$lambda,
     type = "l",
     ylim = c(0,1),
     ylab = "Male Coef",
     xlab = "Lambda used in Model Fit",
     main = "Ridge")
abline(h = lm_alcohol$coefficients["male"], lty = 2)

plot(y = coef_en[which(rownames(coef_en) == "male"), ],
     x = en_alcohol$lambda,
     type = "l",
     ylim = c(0,1),
     ylab = "Male Coef",
     xlab = "Lambda used in Model Fit",
     main = "Elastic-Net")
abline(h = lm_alcohol$coefficients["male"], lty = 2)
dev.off()

##' Figure plots the male coefficient from the three models, with
##' the dotted line being the coef from the linear model.  While
##' the scale of the y-axis is the same across plots, notice that the
##' scale of the x-axis for the Ridge plot differs greatly from the other
##' two.  The lambda values plotted are the values of lambda used in the
##' fits of the model.  Clearly, as lambda increases, the coefficient
##' moves sharply from being close to the linear model coefficient at
##' approximately .84, down to zero.  The Ridge  coefficient does not
##' begin at the linear model coefficient like the other two models,
##' however....

## PART 2: Cross-Validation, Super Learning, Ensembles ------------------------------------------
## i.)
train <- X[-c(1:20), ]
train_y <- alcohol[-c(1:20)]
validation <- X[1:20, ]
validation_y <- alcohol[1:20]

## ii.)
## folds have either 37 or 38 obs
folds <- seq(1, nrow(train), by = nrow(train)/10)
folds <- c(floor(folds), nrow(train))

lm_preds <- lasso_preds <- ridge_preds <- en_preds <- rf_preds <- rep(NA, nrow(train))
for(i in 1:10){
  valid_obs <- folds[i]:folds[i+1]
  
  fold_validx <- train[valid_obs, ]
  
  fold_trainx <- train[-valid_obs, ]
  fold_trainy <- train_y[-valid_obs]
  
  lm_train <- lm(fold_trainy ~ ., data = data.frame(fold_trainx))
  lm_preds[valid_obs] <- predict(lm_train, data.frame(fold_validx))
  
  lasso_train <- cv.glmnet(x = fold_trainx,
                           y = fold_trainy, 
                           nfolds = 10,
                           alpha = 1,
                           type.measure = "mse")
  lasso_preds[valid_obs] <-  predict(lasso_train,
                                     newx = fold_validx,
                                     s = lasso_train$lambda.min)
  
  ridge_train <- cv.glmnet(x = fold_trainx,
                           y = fold_trainy, 
                           nfolds = 10,
                           alpha = 0,
                           type.measure = "mse")
  ridge_preds[valid_obs] <-  predict(ridge_train,
                                     newx = fold_validx,
                                     s = ridge_train$lambda.min)
  
  en_train <- cv.glmnet(x = fold_trainx,
                        y = fold_trainy, 
                        nfolds = 10,
                        alpha = .5,
                        type.measure = "mse")
  en_preds[valid_obs] <-  predict(en_train,
                                  newx = fold_validx,
                                  s = en_train$lambda.min)
  
  rf_train <- randomForest(x = fold_trainx, y = fold_trainy)
  rf_preds[valid_obs] <- predict(rf_train, newdata = data.frame(fold_validx))
}

## getting weights
w <- coef(lm(train_y ~ 0 + lm_preds + lasso_preds + ridge_preds + en_preds + rf_preds))


## iii.)
lm_train <- lm(train_y ~ ., data = data.frame(train))
lm_preds2 <- predict(lm_train, data.frame(validation))

lasso_train <- cv.glmnet(x = train,
                         y = train_y, 
                         nfolds = 10,
                         alpha = 1,
                         type.measure = "mse")
lasso_preds2 <-  predict(lasso_train,
                         newx = validation,
                         s = lasso_train$lambda.min)

ridge_train <- cv.glmnet(x = train,
                         y = train_y, 
                         nfolds = 10,
                         alpha = 0,
                         type.measure = "mse")
ridge_preds2 <-  predict(ridge_train,
                         newx = validation,
                         s = ridge_train$lambda.min)

en_train <- cv.glmnet(x = train,
                      y = train_y, 
                      nfolds = 10,
                      alpha = .5,
                      type.measure = "mse")
en_preds2 <-  predict(en_train,
                      newx = validation,
                      s = en_train$lambda.min)

rf_train <- randomForest(x = train, y = train_y)
rf_preds2 <- predict(rf_train, newdata = data.frame(validation))



## iv.)
## N = 375
## unweighted average of modeling training data with 10-fold validation,
## predicting out of sample folds
unweight_10fold <- rowMeans(cbind(lm_preds, lasso_preds, ridge_preds, en_preds, rf_preds))

## weighted version
weight_10fold <- apply(cbind(lm_preds, lasso_preds, ridge_preds, en_preds, rf_preds), 1,
                       function(row) (row%*%w) / 5)

# ## N = 20
# ## unweighted average of modeling entire training data, predicting validation
# unweight_valid <- rowMeans(cbind(lm_preds2, lasso_preds2, ridge_preds2, en_preds2, rf_preds2))
# 
# ## weighted version
# weight_valid <- apply(cbind(lm_preds2, lasso_preds2, ridge_preds2, en_preds2, rf_preds2), 1,
#                        function(row) (row%*%w) / 5)


## v.) 
## storing all 7 prediction types in a matrix
pred_mat <- cbind(weight_10fold, unweight_10fold, lm_preds,
                  lasso_preds, ridge_preds, en_preds, rf_preds)
pred_cor <- cor(pred_mat)

##' We see that, overall, the correlations are high (> .84 in all cases).
##' The second things I notice is that the random forrest predictions share
##' the lowest correlations with other methods across the board.  The unweighted
##' average is correlated the highest with each other measure, which
##' makes sense.  The unweighted average isn't correlated with the predictions
##' as highly, but again this makes sense as we are leveraging information
##' we have about which methods are better at predicting our outcome.  So the
##' low correlations of the weighted average may be conveying its superior performance.



## vi.)



