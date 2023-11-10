## Set working directory - set your own working directory path
setwd("E:/Microthreats (for GitHub)")

## Load Libraries
library(CTT)
library(summarytools)
library(psych)

## Read Data
data <- read.csv("microthreats.csv", header = TRUE, sep=",")
scale <- data[c(1:9)]

## Compute a scale(sum) score - histogram
sumScore <- rowSums(scale)
scale.all <- cbind(scale, sumScore)
hist(sumScore, breaks = 30)

## Compute item frequencies
freq(scale)
freq(sumScore)
describe(scale.all, ranges=FALSE)

## CTT Analysis
itemAnalysis(scale, itemReport=TRUE, NA.Delete=TRUE)

## CTT using psych package
alpha(scale)

