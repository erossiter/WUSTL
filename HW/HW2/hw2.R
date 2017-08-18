## Loading my data
library(plyr)
setwd("~/dropbox/github/wustl/hw/hw2/")
unigrams <- read.csv("unigram_counts.csv")
trigrams <- read.csv("trigram_counts.csv", row.names = 1)

## making another dataset of word usage rates per document
unigram_rates <- data.frame(t(apply(unigrams[,-1], 1, function(x) x/sum(x))))
unigram_rates <- cbind(unigrams$author, unigram_rates)
colnames(unigram_rates)[1] <- "author"
trigram_rates <- data.frame(t(apply(trigrams[,-1], 1, function(x) x/sum(x))))
trigram_rates <- cbind(unigrams$author, trigram_rates)
colnames(trigram_rates)[1] <- "author"




## Word separating algorithms ####################################################################

## 1.)  Creating measures
## i. Independent linear discriminant ------------------------------------------------------------

## for unigrams
mu_shelby <- apply(unigram_rates[unigram_rates$author == "shelby", -1], 2, mean)
mu_sessions <-  apply(unigram_rates[unigram_rates$author == "sessions", -1], 2, mean)

var_shelby <- apply(unigram_rates[unigram_rates$author == "shelby", -1], 2, var)
var_sessions <- apply(unigram_rates[unigram_rates$author == "sessions", -1], 2, var)

lda_uni <- (mu_shelby - mu_sessions) / (var_shelby + var_sessions)

## for trigrams
mu_shelby_tri <- apply(trigram_rates[trigram_rates$author == "shelby", -1], 2, mean)
mu_sessions_tri <-  apply(trigram_rates[trigram_rates$author == "sessions", -1], 2, mean)

var_shelby_tri <- apply(trigram_rates[trigram_rates$author == "shelby", -1], 2, var)
var_sessions_tri <- apply(trigram_rates[trigram_rates$author == "sessions", -1], 2, var)

lda_tri <- (mu_shelby_tri - mu_sessions_tri) / (var_shelby_tri + var_sessions_tri)



## ii. Standardized mean difference ------------------------------------------------------------

## function to do it for both datasets
std_diff_func <- function(col, authors = unigrams$author){
  shel <- col[authors == "shelby"]
  sess <- col[authors == "sessions"]

  top <- mean(shel) - mean(sess)
  bottom <- sqrt(var(shel)/length(shel) + var(sess)/length(sess))
  
  return(top/bottom)
}

## for unigrams
stdiff_uni <- apply(unigram_rates[,-1], 2, std_diff_func)

## for trigrams
stdiff_tri <- apply(trigram_rates[,-1], 2, std_diff_func)


## iii. Standardized log odds -------------------------------------------------------------------
## this uses the counts (not rates) data

## for unigrams

## N_shelby, N_sessions - sum of column sums
N_shelby <- sum(apply(unigrams[unigrams$author == "shelby", -1], 2, sum))
N_sessions <- sum(apply(unigrams[unigrams$author == "sessions", -1], 2, sum))

## pi star values - (sum of word counts across documents + 1) / (N_author + 1000)
alpha_j <- 1
pi_shelby <- ((apply(unigrams[unigrams$author == "shelby", -1], 2, sum) + alpha_j) /
                (N_shelby + 1000))
pi_sessions <- ((apply(unigrams[unigrams$author == "sessions", -1], 2, sum) + alpha_j) /
                (N_sessions + 1000))

## log odds ratio and standardized version
ratio <- log(pi_shelby/(1-pi_shelby)) - log(pi_sessions/(1-pi_sessions))
ratio_var <- ((1 / (apply(unigrams[unigrams$author == "shelby", -1], 2, sum) + 1)) + 
                (1 / (apply(unigrams[unigrams$author == "sessions", -1], 2, sum) + 1)))
std_ratio_uni <- ratio / sqrt(ratio_var)


## for trigrams
N_shelby_tri <- sum(apply(trigrams[trigrams$author == "shelby", -1], 2, sum))
N_sessions_tri <- sum(apply(trigrams[trigrams$author == "sessions", -1], 2, sum))

pi_shelby_tri <- ((apply(trigrams[trigrams$author == "shelby", -1], 2, sum) + alpha_j) /
                (N_shelby_tri + 500))
pi_sessions_tri <- ((apply(trigrams[trigrams$author == "sessions", -1], 2, sum) + alpha_j) /
                  (N_sessions_tri + 500))

## log odds ratio and standardized version
ratio_tri <- log(pi_shelby_tri/(1-pi_shelby_tri)) - log(pi_sessions_tri/(1-pi_sessions_tri))
ratio_var_tri <- ((1 / (apply(trigrams[trigrams$author == "shelby", -1], 2, sum) + 1)) + 
                (1 / (apply(trigrams[trigrams$author == "sessions", -1], 2, sum) + 1)))
std_ratio_tri <- ratio_tri / sqrt(ratio_var_tri)



## 2.) Plots ---------------------------------------------------------------------------------------

## unigrams ----
pdf("unigram_plots.pdf", height = 2.5, width = 4.5)
layout(matrix(c(1,2,3), nrow = 1, byrow = T))

par(mar = c(3,1,1,1), oma=c(0,0,0,0))
lda_uni_plot <- sort(lda_uni)[c(1:15, 985:1000)]
clr <- ifelse(lda_uni_plot < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(1,length(lda_uni_plot)),
     xlim = lda_uni_plot[c(1, length(lda_uni_plot))]+c(-50,50))
text(x = lda_uni_plot,
     y = 1:length(lda_uni_plot),
     labels = names(lda_uni_plot),
     col = clr,
     cex = .5)
axis(side = 1, at = seq(-350, 1200, 250), cex.axis = .5)
mtext(side = 1, text = "Word Weight", line = 2, cex = .5)
mtext(side = 3, text = "Independent Linear Discriminant", cex = .5)


stdiff_uni_plot <- sort(stdiff_uni)[c(1:15, 985:1000)]
clr <- ifelse(stdiff_uni_plot < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(1,length(stdiff_uni_plot)),
     xlim = stdiff_uni_plot[c(1, length(stdiff_uni_plot))] + c(-5,5))
text(y = 1:length(stdiff_uni_plot),
     x = stdiff_uni_plot, 
     labels = names(stdiff_uni_plot),
     col = clr,
     cex = .5)
axis(side = 1, at = seq(-28, 32, 10), cex.axis = .5)
mtext(side = 1, text = "Word Weight", line = 2, cex = .5)
mtext(side = 3, text = "Standardized Mean Difference", cex = .5)

ratio_uni_plot <- sort(std_ratio_uni)[c(1:15, 985:1000)]
clr <- ifelse(ratio_uni_plot < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(1,length(ratio_uni_plot)),
     xlim = ratio_uni_plot[c(1, length(ratio_uni_plot))] + c(-5,5))
text(y = 1:length(ratio_uni_plot),
     x = ratio_uni_plot, 
     labels = names(ratio_uni_plot),
     col = clr,
     cex = .5)
axis(side = 1, at = seq(-26, 19, 9), cex.axis = .5)
mtext(side = 1, text = "Word Weight", line = 2, cex = .5)
mtext(side = 3, text = "Standardized Log Odds", cex = .5)

dev.off()


## trigrams ----
pdf("trigram_plots.pdf", height = 2.5, width = 4.5)
layout(matrix(c(1,2,3), nrow = 1, byrow = T))

par(mar = c(3,1,1,1), oma=c(0,0,0,0))
lda_tri_plot <- sort(lda_tri)[c(1:15, 485:500)]
clr <- ifelse(lda_tri_plot < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(1,length(lda_tri_plot)),
     xlim = lda_tri_plot[c(1, length(lda_tri_plot))] + c(-100,100))
text(y = 1:length(lda_tri_plot),
     x = lda_tri_plot, 
     labels = names(lda_tri_plot),
     col = clr,
     cex = .4)
axis(side = 1, at = seq(-60, 500, 100), cex.axis = .5)
mtext(side = 1, text = "Word Weight", line = 2, cex = .5)
mtext(side = 3, text = "Independent Linear Discriminant", cex = .5)


stdiff_tri_plot <- sort(stdiff_tri)[c(1:15, 485:500)]
clr <- ifelse(stdiff_tri_plot < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(1,length(stdiff_tri_plot)),
     xlim = stdiff_tri_plot[c(1, length(stdiff_tri_plot))]+ c(-10,20))
text(y = 1:length(stdiff_tri_plot),
     x = stdiff_tri_plot, 
     labels = names(stdiff_tri_plot),
     col = clr,
     cex = .4)
axis(side = 1, at = seq(-40, 40, 20), cex.axis = .5)
mtext(side = 1, text = "Word Weight", line = 2, cex = .5)
mtext(side = 3, text = "Standardized Mean Difference", cex = .5)

ratio_tri_plot <- sort(std_ratio_tri)[c(1:15, 485:500)]
clr <- ifelse(ratio_tri_plot < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(1,length(ratio_tri_plot)),
     xlim = ratio_tri_plot[c(1, length(ratio_tri_plot))]+ c(-10,10))
text(y = 1:length(ratio_tri_plot),
     x = ratio_tri_plot, 
     labels = names(ratio_tri_plot),
     col = clr,
     cex = .4)
axis(side = 1, at = seq(-15, 15, 5), cex.axis = .5)
mtext(side = 1, text = "Word Weight", line = 2, cex = .5)
mtext(side = 3, text = "Standardized Log Odds", cex = .5)

dev.off()

## 3.) Comparison ------------------------------------------------------

##' I plot the 15 most discriminanting words (in either direction) for the
##' processed top unigrams and trigrams.  
##' 
##' UNIGRAM OBSERVATIONS
##' Of the three measures, independent linear discriminant returns the
##' widest range in the discriminant words.  That is, among the most
##' discriminating words, we have the most variation.  We see that Sessions
##' talks a lot about Bush in his press releases.  He also often mentions the 
##' day of the week, which is not very informative in what he's talking about.
##' Although, we can deduce he's probably not talking a lot about policy if he's
##' often mentioning a specific day.  Shelby talks a lot about funds, appropriations,
##' and "million" is an important word.  It seems his press releases are often about
##' funding for government programs.
##' 
##' TRIGRAM OBSERVATIONS
##' There's a different trend here for Sessions.  We see he's talking a lot about
##' his state, Alabama, which makes sense. He's also making annocements in his press
##' releases.  Shelby's plot, on the other hand, isn't as informative here as it 
##' was with unigrams.  I think I should have included press release words.  Regardless,
##' this is language that puts him apart from Sessions.  His press releases talk
##' about state news.  Also, the independent linear discriminant analysis is very different
##' from the other two. Here was see he is talking about risk in children, cyber crime,
##' and drug problems.  It's interesting the other two measures are so different.



## Comparing Document Similarity ###############################################################

## 1.) Sampling 100 from each senator ------------------------------------------------------
set.seed(13489)
shelby_100 <- trigrams[sample(which(trigrams$author == "shelby"), 100, replace = FALSE), -1]
sessions_100 <- trigrams[sample(which(trigrams$author == "sessions"), 100, replace = FALSE), -1]

## 2.) Creating matrices---------------------------------------------------------------------

## i.) Euclidean distance between documents

## shelby is rows, sessions is columns
## (I realize I double the computations necessary here...)
ecl_dist <- as.matrix(dist(rbind(shelby_100, sessions_100),
                           method = "euclidean"))
diag(ecl_dist) <- NA
write.table(ecl_dist, file = "doc_similarity/ecl_dist.txt")

## ii.) Euclidean distance with tf-idf

idf_func <- function(word){
  N <- 200
  n_shel <- sum(shelby_100[,word] != 0)
  n_sess <- sum(sessions_100[,word] != 0)
  n_j <- n_shel + n_sess
  return(log(N / n_j))
}
idf_scores <- sapply(1:500, idf_func)

shelby_idf <- adply(shelby_100, 1, function(row) row*idf_scores)
sessions_idf <- adply(sessions_100, 1, function(row) row*idf_scores)

tfidf_dist <- as.matrix(dist(rbind(shelby_idf, sessions_idf),
                             method="euclidean"))
diag(tfidf_dist) <- NA
write.table(tfidf_dist, file = "doc_similarity/tfidf_ecl_dist.txt")


## iii.) Cosine similarity
cos_func <- function(i, j){
  s1 <- unlist(sample_200[i, ])
  s2 <- unlist(sample_200[j, ])
  return( sum(s1*s2) / sqrt(sum(s1^2) * sum(s2^2)))
}
sample_200 <- rbind(shelby_100, sessions_100)
cos_mat <- apply(expand.grid(1:200, 1:200), 1, cos_func)
cos_mat2 <- matrix(cos_mat, nrow = 200, byrow = TRUE)

diag(cos_mat2) <- NA
write.table(cos_mat2, file = "doc_similarity/cos_sim.txt")


## iv.) Cosine similarity with tfidf
cos_func_idf <- function(i, j){
  s1 <- unlist(sample_200_idf[i, ])
  s2 <- unlist(sample_200_idf[j, ])
  return( sum(s1*s2) / sqrt(sum(s1^2) * sum(s2^2)))
}
sample200_idf <- rbind(shelby_idf, sessions_idf)
cos_mat_idf <- apply(expand.grid(1:200, 1:200), 1, cos_func_idf)
cos_mat_idf2 <- matrix(cos_mat_idf, nrow = 200, byrow = TRUE)

diag(cos_mat_idf2) <- NA
write.table(cos_mat_idf2, file = "doc_similarity/cos_sim_tfidf.txt")

## v.) Normalize, Gaussian kernal
normalized <- matrix(NA, nrow = 200, ncol = 500)
for (i in 1:200){
  normalized[i,1:500] <- unlist(sample_200[i,]/sum(sample_200[i,]))
}

sigma <- 100
gauss_mat <- exp(-(as.matrix(dist(normalized)))/sigma)
diag(gauss_mat)<- NA
write.table(gauss_mat, file = "doc_similarity/normalized_gaussian.txt")


## vi.) Normalize, Gaussian kernal, tfidf weights
idf_func <- function(x){
  N <- 200
  n_j <- sum(x != 0)
  return(log(N / n_j))
}
idf_scores <- apply(normalized, 2,  idf_func)

idf_normalized <- as.matrix(t(apply(normalized, 1, function(x) x*idf_scores)))
gauss_norm <- exp(-(as.matrix(dist(idf_normalized)))/sigma)
diag(gauss_norm)<- NA
write.table(gauss_norm, "doc_similarity/normalized_gaussian_tfidf.txt")



## 3.) Similar and Dissimilar Docs --------------------------------------
which(ecl_dist == max(ecl_dist, na.rm=T), arr.ind=T)
which(ecl_dist == min(ecl_dist, na.rm=T), arr.ind=T)


which(tfidf_dist == max(tfidf_dist, na.rm=T), arr.ind=T)
which(tfidf_dist == min(tfidf_dist, na.rm=T), arr.ind=T)


which(cos_mat2 == max(cos_mat2, na.rm=T), arr.ind=T)
which(cos_mat2 == min(cos_mat2, na.rm=T), arr.ind=T)


which(cos_mat_idf2 == max(cos_mat_idf2, na.rm=T), arr.ind=T)
which(cos_mat_idf2 == min(cos_mat_idf2, na.rm=T), arr.ind=T)


which(gauss_mat == max(gauss_mat, na.rm=T), arr.ind=T)
which(gauss_mat == min(gauss_mat, na.rm=T), arr.ind=T)


which(gauss_norm == max(gauss_norm, na.rm=T), arr.ind=T)
which(gauss_norm == min(gauss_norm, na.rm=T), arr.ind=T)
