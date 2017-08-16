## Loading my data
setwd("~/dropbox/github/wustl/hw/hw1/")
debate <- read.csv("debate_data.csv")

## Creating rate variables
debate$pos_rate <- debate$pos/debate$nonstopwords
debate$neg_rate <- debate$neg/debate$nonstopwords

## Comparing overall positive and negative word rate
pdf("hw1_rates.pdf", height = 4, width = 4)
layout(matrix(1:3), heights = c(1,1,.2))
par(oma = c(0,0,1,1), mar = c(3,3,1,0), cex.axis = .8, las = 1)

plot(ifelse(debate$speaker == "lehrer", debate$pos_rate, 0),
     type = "l", col = "black", xlab = "", ylab = "")
lines(ifelse(debate$speaker == "obama", debate$pos_rate, 0), type = "l", col = "blue")
lines(ifelse(debate$speaker == "romney", debate$pos_rate, 0), type = "l", col = "red")
mtext("Positive Word Rate", side = 2, line = 2, cex = .7, las = 0)
mtext("Turn in Debate", side = 1, line = 2, cex = .6, las = 0)
mtext("Positive and Negative Word Rates in the First 2012 Presidential Debate",
      side = 3, cex = .7, line = .5)

plot(ifelse(debate$speaker == "lehrer", debate$neg_rate, 0), type = "l", col = "black",
     ylim = c(0, .5), xlab = "", ylab = "")
lines(ifelse(debate$speaker == "obama", debate$neg_rate, 0), type = "l", col = "blue")
lines(ifelse(debate$speaker == "romney", debate$neg_rate, 0), type = "l", col = "red")
mtext("Negative Word Rate", side = 2, line = 2, cex = .7, las = 0)
mtext("Turn in Debate", side = 1, line = 2, cex = .6, las = 0)

par(mar = c(0,0,0,0))
plot(1,1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
legend("center", col = c("black", "blue", "red"), legend = c("Lehrer", "Obama", "Romney"),
       horiz = TRUE, lty = 1, cex = .8)
dev.off()

#' Comments:
#' The top plot is the positive word rate (the number of positive words divided by the
#' number of non-stopwords) for each speaker as the debate advances.  The blue line
#' is then President Obama's rate, the red line is candidate Romney's rate, and the black
#' line is debate moderator Lehrer's rate.  The bottom plot is the negative word rate.
#' Both plots have turn on the x-axis and rate (from 0 to .5) on the y-axis.  Since
#' the plots are on the same scale, we can first see that there is much more activity 
#' on the positive word plot.  That is, a candidate's turn is more likely to be filled
#' with positive than negative words.  Across candidates, we see that Romney has some
#' turns that are very positive and Obama has a few turns that are very negative (look
#' to the spikes in the plots).  Generally though, we see that Obama's turns are usually
#' more positive and Romney's are more negative.  Lastly, it is interesting to note
#' that the Lehrer's turns are filled with more "extreme" language than the candidates
#' (the black spikes are notable on both plots).

