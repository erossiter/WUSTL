## Loading my cleaned data
library(plyr)
library(parallel)
library(doMC)
library(glmnet)
library(e1071)
setwd("~/dropbox/github/wustl/hw/hw3/")
nyt <- read.csv("doc_term_mat.csv")

## Clustering Methods

## 1.) -------------------------------------------------------------------------
## normalizing
nyt_norm <- adply(nyt[,-1], 1, function(x) x/sum(x))

## function that executes objective function for clusters = n_clusters
one_kmeans <- function(n_clusters){
  km <- kmeans(nyt_norm, centers = n_clusters)
  
  obj_func <- function(i){
    sum((nyt_norm[i, ] - km$centers[km$cluster[i], ])^2)
  }
  
  sum_me <- aaply(.data = 1:nrow(nyt),
                  .margins = 1,
                  .fun = obj_func,
                  .parallel = TRUE)
  
  return(sum(sum_me))
}

# Running in parallel *within* function for two through N-1 clusters
registerDoMC(detectCores() - 1)
set.seed(143490)
pts <- aaply(.data = 2:(nrow(nyt_norm)-1),
             .margins = 1,
             .fun = one_kmeans,
             .parallel = FALSE)

pdf("kmeans_plot.pdf")
plot(pts, type = "l",
     main = "kmeans Objective Function Varies\nin Response to the No. of Clusters",
     xlab = "No. of Clusters",
     ylab = "Objective Function")
dev.off()

## 2.) -------------------------------------------------------------------------
set.seed(480184)
km <- kmeans(nyt_norm, centers = 6)

## 3.) -------------------------------------------------------------------------

## part i.) finding top ten words per cluster
diff_func <- function(c){
  theta_notk <- colSums(km$centers[-c, ]) / (nrow(km$centers) - 1)
  return(km$centers[c, ] - theta_notk)
}

diff_k <- t(sapply(1:6, diff_func))

top_ten <- sapply(1:6, function(k){
  names(sort(diff_k[k, ], decreasing = TRUE)[1:10])
})

##' Labels I conclude from top words
##' 1: baseball trades
##' 2: presidential campaign
##' 3: voter polls
##' 4: the arts
##' 5: sports
##' 6: business

## part ii.) samplings 2 texs per cluster and assigning my own label
## cluster 1 has only 1 document: #185
set.seed(293)
sample_docs <- sapply(2:6, function(k) sample(which(km$cluster == k), 2))
colnames(sample_docs) <- paste0("cluster", 2:6)

##' Labels I conclude from sampling and reading
##' 1: sports transactions
##' 2: global politics
##' 3: U.S. politics (presidential election)
##' 4: the arts
##' 5: sports
##' 6: business


##' Overall, I'm pleased with my analysis, what I deemed the "label"
##' given the top 10 words, and what I deemed the "label" given
##' the reading of a few documents in each cluster.  One problem with
##' the results is that one documents is in cluster 1.  It is a unique
##' document in a way, but it should be in the sports cluster.  Otherwise,
##' clusters 3, 4, 5, and 6 were easy to label off the top ten words.
##' Cluster 2 was a little odd.  I think it was conflating global politics,
##' and elections with U.S. elections.



## Supervised learning with Naive Bayes

## Part C: Comparing
nyt_comparing <- nyt[nyt$desk == "Business/Financial Desk" | nyt$desk == "National Desk", ]

## Running naiveBayes because my function doesn't work...
pred <- rep(NA, nrow(nyt_comparing))
for(i in 1:nrow(nyt_comparing)){
  fit <- naiveBayes(nyt_comparing$desk[-i] ~., data = nyt_comparing[-i, ])
  pred[i] <- as.character(predict(fit, nyt_comparing[i, -1]))
}

sum(pred == nyt_comparing$desk)/nrow(nyt_comparing) ## 88% accuracy!



## Leaving out 1/3 of data to predict with for LASSO and Ridge
set.seed(1487)
test <- sample(1:nrow(nyt_comparing), 44)
test_desks <- ifelse(as.numeric(nyt_comparing$desk[test]) == 6, 1, 0)

# Now running lasso
lasso_train <- cv.glmnet(x = as.matrix(nyt_comparing[-test,-1]),
                         y = factor(nyt_comparing$desk, labels = c("B", "N"))[-test],
                         nfolds = 10,
                         alpha = 1,
                         family = "binomial",
                         type.measure = "mse")

lasso_preds <- predict(lasso_train, newx = as.matrix(nyt_comparing[test, -1]), s = lasso_train$lambda.min)
lasso_preds <- 1/(1 + exp(-lasso_preds))
lasso_preds <- ifelse(lasso_preds >= .5, 1, 0)
sum(lasso_preds == test_desks)/length(test_desks) ## 88% as well... weird

## Now running ridge
ridge_train <- cv.glmnet(x = as.matrix(nyt_comparing[-test,-1]),
                         y = factor(nyt_comparing$desk, labels = c("B", "N"))[-test], 
                         nfolds = 10,
                         alpha = 0,
                         family = "binomial",
                         type.measure = "mse")

ridge_preds <- predict(ridge_train, newx = as.matrix(nyt_comparing[test, -1]), s = ridge_train$lambda.min)
ridge_preds <- 1/(1 + exp(-ridge_preds))
ridge_preds <- ifelse(ridge_preds >= .5, 1, 0)
sum(ridge_preds == test_desks)/length(test_desks) ## 93% now, an improvement.





