## Loading my data
setwd("~/dropbox/github/wustl/hw/hw2/")
unigrams <- read.csv("word_counts.csv")
trigrams <- read.csv("trigram_counts.csv")


## Word separating algorithms ####################################################################

## 1.)  Creating measures
## i. Independent linear discriminant ------------------------------------------------------------
top_shelby <- apply(unigrams[unigrams$author == "shelby", -1], 2, sum)
bottom_shelby <- sum(top_shelby)
mu_shelby <-  top_shelby/bottom_shelby 

top_sessions <- apply(unigrams[unigrams$author == "sessions", -1], 2, sum)
bottom_sessions <- sum(top_sessions)
mu_sessions <-  top_sessions/bottom_sessions 

var_shelby <- apply(unigrams[unigrams$author == "shelby", -1], 2, var)
var_sessions <- apply(unigrams[unigrams$author == "sessions", -1], 2, var)

## the measure for each word
theta_js <- (mu_shelby - mu_sessions) / (var_shelby + var_sessions)

## ii. Standardized mean difference ------------------------------------------------------------
std_diff_func <- function(word){
  shel <- unigrams[unigrams$author == "shelby", word]
  sess <- unigrams[unigrams$author == "sessions", word]

  top <- mean(shel) - mean(sess)
  bottom <- sqrt(var(shel)/length(shel) + var(sess)/length(sess))
  
  return(top/bottom)
}

## the measure for each word
std_diff <- sapply(colnames(unigrams)[-1], std_diff_func)

## iii. Standardized log odds -------------------------------------------------------------------
## N_shelby, N_sessions - sum of column sums
N_shelby <- sum(apply(unigrams[unigrams$author == "shelby", -1], 2, sum))
N_sessions <- sum(apply(unigrams[unigrams$author == "sessions", -1], 2, sum))

## pi star values - (sum of word counts across documents + 1) / (N_author + 1000)
alpha_j <- 1
pi_shelby <- ((apply(unigrams[unigrams$author == "shelby", -1], 2, sum) + alpha_j) /
                (N_shelby + 1000))
pi_sessions <- ((apply(unigrams[unigrams$author == "sessions", -1], 2, sum) + alpha_j) /
                (N_sessions + 1000))

## the measure for each word
logodds_ratio <- log(pi_shelby/(1-pi_shelby)) - log(pi_sessions/(1-pi_sessions))
std_logodds_ratio <- logodds_ratio / sd(logodds_ratio)

## 2.) Plots ---------------------------------------------------------------------------------------
clr <- ifelse(theta_js < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlim = c(-.1, .1),
     ylim = c(0, max(abs(theta_js))))
text(x = jitter(rep(0, length(theta_js))),
     y = abs(theta_js), 
     labels = names(theta_js),
     col = clr)


clr <- ifelse(std_diff < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlim = c(-.1, .1),
     ylim = c(0, max(abs(std_diff))))
text(x = jitter(rep(0, length(std_diff))),
     y = abs(std_diff), 
     labels = names(std_diff),
     col = clr)


clr <- ifelse(logodds_ratio < 0, "red", "blue")
plot(x = 0,
     y = 0,
     type = "n",
     col = clr,
     axes = FALSE,
     xlim = c(-.1, .1),
     ylim = c(0, max(abs(logodds_ratio))))
text(x = jitter(rep(0, length(logodds_ratio))),
     y = abs(logodds_ratio), 
     labels = names(logodds_ratio),
     col = clr)



## Comparing Document Similarity ###############################################################
## Sampling 100 from each senator
set.seed(13489)
shelby_100 <- sample(which(trigrams$author == "shelby"), 100, replace = FALSE)
sessions_100 <- sample(which(trigrams$author == "sessions"), 100, replace = FALSE)
sample_200 <- trigrams[c(shelby_100, sessions_100), ]

## 

