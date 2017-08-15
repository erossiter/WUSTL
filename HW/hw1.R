## Loading my data
setwd("~/dropbox/github/wustl/hw")
debate <- read.csv("debate_data.csv")

## Creating rate variables
debate$pos_rate <- debate$pos/debate$nonstopwords
debate$neg_rate <- debate$neg/debate$nonstopwords


## Comparing overall positive and negative word rate
pdf("hw1_rates.pdf", height = 4, width = 4)
layout(matrix(1:3), heights = c(1,1,.2))
par(oma = c(0,0,0,1), mar = c(3,3,2,0), cex.axis = .8, las = 1)

plot(debate$pos_rate[debate$speaker == "lehrer"], type = "l", col = "black",
     xlab = "", ylab = "")
lines(debate$pos_rate[debate$speaker == "obama"], type = "l", col = "blue")
lines(debate$pos_rate[debate$speaker == "romney"], type = "l", col = "red")
mtext("Positive Word Rate", side = 2, line = 2, cex = .7, las = 0)

plot(debate$neg_rate[debate$speaker == "lehrer"], type = "l", col = "black",
     ylim = c(0, .5), xlab = "", ylab = "")
lines(debate$neg_rate[debate$speaker == "obama"], type = "l", col = "blue")
lines(debate$neg_rate[debate$speaker == "romney"], type = "l", col = "red")
mtext("Negative Word Rate", side = 2, line = 2, cex = .7, las = 0)

par(mar = c(0,0,0,0))
plot(1,1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
legend("center", col = c("black", "blue", "red"), legend = c("Lehrer", "Obama", "Romney"),
       horiz = TRUE, lty = 1)
dev.off()
