## Loading my data
setwd("~/dropbox/github/wustl/hw/hw2/")
unigrams <- read.csv("word_counts.csv")
trigrams <- read.csv("trigram_counts.csv")


## Word separating algorithms ----------------------------------
## 1. Independent linear discriminant

## 2. Standardized mean difference
std_diff_func <- function(word){
  shel <- unigrams[unigrams$author == "shelby", word]
  sess <- unigrams[unigrams$author == "sessions", word]

  top <- mean(shel) - mean(sess)
  bottom <- sqrt(var(shel)/length(shel) + var(sess)/length(sess))
  
  return(top/bottom)
}

std_diff <- sapply(colnames(unigrams)[-1], std_diff_func)
plot(std_diff, pch = "")
text(x = std_diff, labels = names(std_diff), cex = .5)

## 3. Standardized log odds





## Comparing Document Similarity ----------------------------------
