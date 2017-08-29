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

## PCA

## reading in data preprocessed in Python script
## creating scree plot
mach_raw <- read.csv("doc_term_mat.csv", row.names = 1)
mach <- adply(mach_raw, 1, function(row) row/sum(row))
rownames(mach) <- rownames(mach_raw)
pca <- prcomp(mach, scale = T)
pdf("scree_plot.pdf", width = 10, height = 10)
plot(pca, type = "l", main = "Scree Plot, The Prince")
dev.off()

##' The "elbow test" is not super useful here as the "elbow" is not
##' clearly defined.  I think the variance explained does clearly drop
##' after 2 principle components.  But this is fairly subjective.
##' Is the drop from 4 to 5 componenets big enough to care about that
##' additional explained variance?  Maybe.  But here I'll procede with
##' just the two principle components.


pdf("twodim_mach.pdf", width = 10, height = 10)
mach_names <- sub(pattern = "MachText/Mach_", x = rownames(pca$x), replacement = "")
mach_names <- sub(pattern = ".txt", x = mach_names, replacement = "")
plot(1, 1, type = "n", xlim = range(pca$x[,1]), ylim = range(pca$x[,2]),
     main = "Two-Dimensional Embedding of The Prince Documents",
     xlab = "First Dimension",
     ylab = "Second Dimension",
     cex.main = 2,
     cex.lab = 1.5)
text(x = pca$x[,1],
     y = pca$x[,2],
     cex = 1,
     labels = mach_names)
dev.off()

sort(pca$rotation[,1], decreasing = TRUE)[1:10]
sort(pca$rotation[,2], decreasing = TRUE)[1:10]

##' We see from the plot that there aren't really any documents with
##' "high" loadings from both dimensions.  This makes sense as we'd expect
##' the two highest dimensions to explain different things.
##' We see from the variable loadings that the first dimension dimension
##' deals with nobility and the second dimension deals with ruling.  So the
##' primary variation stems from if the document talks about how to
##' be a proper noble person or being a good protector.



## MDS

## euclidean distance
DX = dist(mach, method = "euclidean")

## classic mds
mds_scale <- cmdscale(DX, k = 2)

## pca, but with out scaling option
pca_noscale <- prcomp(mach, scale = FALSE)

## correlatoin between 1st dimension of embeddings
cor(mds_scale[,1], pca_noscale$x[,1])  ## literally 1.0?

## now the manhattan method
DX_manhattan <- dist(mach, method = "manhattan")

## mds with manhattan
mds_manhattan <- cmdscale(DX_manhattan, k = 2)

## pca, but with out scaling option
pca_manhattan <- prcomp(mach, scale = FALSE)

## correlatoin between 1st dimension of embeddings
cor(mds_manhattan[,1], pca_manhattan$x[,1])  ## -.94

##' The distance measure is different in that PCA preserves
##' the covariance of the data when minimizing dimensions
##' and MDS preserves the distance between the data when minimizing
##' dimensions.  As we see above, PCA is a special case of MDS
##' when the covariance equals the euclidean distance.



