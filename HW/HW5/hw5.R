## Loading my cleaned data
rm(list = ls())
library(plyr)
library(stm)
setwd("~/dropbox/github/wustl/hw/hw5/")
nyt <- read.csv("../hw3/doc_term_mat.csv")


## 1. Using the STM Package ------------------------------------------------------------

## Numeric representation of each word, if I need it later
word_rep <- cbind(colnames(nyt[,-1]), 1:ncol(nyt[,-1]))

## Changing word column names to numbers to use in my function
colnames(nyt)[-1] <- 1:ncol(nyt[,-1])

## Function changes each document (row) to a list
doc_to_matrix <- function(row){
  doc <- matrix(NA, ncol = length(row[row != 0]), nrow = 2)
  doc[1,] <- as.integer(names(row)[row!=0])
  doc[2,] <- as.integer(row[row!=0])
  return(doc)
}

corpus_list <- alply(.data = nyt[, -1],
                     .margins = 1,
                     .fun = doc_to_matrix)

nyt_bydesk <- stm(corpus_list, vocab = word_rep[,1], K = 8, prevalence = ~nyt$desk)
nyt_vanilla <- stm(corpus_list, vocab = word_rep[,1], K = 8)

## checking out the labels for the model with desk for prevalence
labelTopics(nyt_bydesk)
colMeans(nyt_bydesk$theta)

labelTopics(nyt_vanilla)
colMeans(nyt_vanilla$theta)

##' There's an election topic in both models.  By desk, it has
##' an average of .14 in documents.  With the vanilla model, it
##' has an average of .12.
##' There's also a sports topic in both models.  When conditioning
##' by desk, it has a proportion of .12.  With the vanilla model,
##' it has an average proportioin of .14.
##' They don't differ by much.




## 2. Machiavelli's Prince  ------------------------------------------------------------

## reading in data preprocessed in Python script
mach <- read.csv("doc_term_mat.csv", row.names = 1)

# ## implementing the suggestion
# vcov_mat <- (t(as.matrix(mach)) %*% as.matrix(mach))/500
# eig_vals <- eigen(vcov_mat)$values
# eig_var <- (1/length(eig_vals)) * (sum((eig_vals - mean(eig_vals))^2))
# 
# ## 
# pca <- prcomp(mach, scale = T)
# 
# plot(pca$sdev)
