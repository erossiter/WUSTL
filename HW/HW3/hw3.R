## Loading my cleaned data
library(plyr)
library(parallel)
library(doMC)
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
registerDoMC(2)
set.seed(143490)
pts <- aaply(.data = 2:10,#(nrow(nyt_norm)-1),
             .margins = 1,
             .fun = one_kmeans,
             .parallel = FALSE)

plot(pts, type = "l",
     main = "kmeans Objective Function Varies in Response to the No. of Clusters",
     xlab = "No. of Clusters",
     ylab = "Objective Function")


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







