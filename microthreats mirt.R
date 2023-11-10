## Set working directory - set your own working directory path
setwd("E:/Microthreats (for GitHub)")

## Load Libraries
library(mirt)
library(psych)
library(ggplot2)

## Read data file
data <- read.csv("microthreats.csv", header = TRUE, sep=",")
scale <- data[c(1:9)]

## Fit a GRM
(mod1 <- (mirt(scale, 1, verbose = FALSE, itemtype = 'graded', SE = TRUE)))

## Assess model fit
M2(mod1, type = "M2*", calcNULL = TRUE)

## Examine IRT and FA parameters
coef(mod1, IRTpars = TRUE, simplify = TRUE)
coef(mod1, IRTpars = TRUE, printSE = TRUE)
coef(mod1, IRTpars = TRUE, printSE = FALSE)
summary(mod1)

## EAP Scoring
fullscores <- fscores(mod1, method='EAP', full.scores=T, full.scores.SE = T)
scale$sum <- rowSums(scale)
score.dat <- cbind(scale, fullscores)
score.dat$rxx <- 1 - ((score.dat$SE_F1)**2)
describe(score.dat)

## IRT Reliability
empirical_rxx(fullscores)
marginal_rxx(mod1)

## Various plots
## Category characteristic curves
plot(mod1, type='trace', facet_items=T, as.table = TRUE,
                auto.key=list(points=F, lines=T, columns=3, space = 'top', cex = .8), 
                theta_lim = c(-3, 3), main="", layout = c(3,3))

## Item information curves
plot(mod1, type='infotrace', facet_items = TRUE, as.table = TRUE,
                auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
                theta_lim = c(-3, 3), main="", layout = c(3,3)) 

## Information and standard error curves
plot(mod1, type = 'infoSE', theta_lim = c(-3, 3), main = "")

## Scale Level Reliability
plot(mod1, type = 'rxx', theta_lim = c(-3,3), main = "")

## Test characteristic curve
plot(mod1, type = 'score', theta_lim = c(-3, 3), main = "")

## Advanced graphic - Using ggplot for TCC plot
## Do not edit

Theta <- matrix(seq(-3,3, .01))
expected <- expected.test(mod1, Theta)

X <- Theta
Y <- expected

DF <- data.frame(X, Y)

# vertical line
vsegment <- function(x, X, Y) {
  geom_segment(aes(x = x, xend = x, y = -Inf, yend = approx(X, Y, x)$y),
               linetype = 2, linewidth = 1)  
}

# horizontal line
hsegment <- function(x, X, Y) {
  geom_segment(aes(x = -Inf, xend = x, y = approx(X, Y, x)$y, 
                   yend = approx(X, Y, x)$y),
               linetype = 2, linewidth = 1)  
}

myplot <- ggplot(data = DF,
                 aes(x = X,
                     y = Y)) +
  geom_point(size = 1) +
  geom_line(colour = "black", linewidth = 1) +
  vsegment(1, X, Y) +
  hsegment(1, X, Y) +
  labs(x = expression(theta), y = "Expected True Score")
myplot + theme_bw()

