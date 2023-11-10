## Set working directory - set your own working directory path
setwd("E:/Microthreats (for GitHub)")

## Load Libraries
library(lavaan)
library(dplyr) 
library(tidyr)
library(knitr)

data <- read.csv("microthreats.csv", header = TRUE, sep=",")
scale <- data[c(1:9)]

## Specify Model
scale.model <- 'scale = ~ Item.1 + Item.2 + Item.3 + Item.4 + Item.5 + Item.6 + 
                          Item.7 + Item.8 + Item.9'

## Fit Model
fit.cat <- cfa(scale.model, 
               data=scale, 
               estimator = "ULSMV",
               ordered = TRUE)

## Assess model fit
fitMeasures(fit.cat, c("chisq.scaled", "df.scaled", "pvalue.scaled"))
fitMeasures(fit.cat,c("rmsea.scaled", "rmsea.pvalue.scaled", 
                      "rmsea.ci.lower.scaled",
                      "rmsea.ci.upper.scaled"))
fitMeasures(fit.cat, c("srmr", "crmr", "cfi.scaled"))

## Examine model details
summary(fit.cat, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
parameterestimates(fit.cat, standardized = TRUE)
standardizedSolution(fit.cat)

## Assess residuals
lavResiduals(fit.cat)

lavResiduals(fit.cat, se = TRUE, type = "cor", output= "table", 
             add.labels = TRUE)

## Present loadings, SEs, etc in a table
parameterEstimates(fit.cat, standardized=TRUE) %>%
  filter(op == "=~") %>%
  mutate(stars = ifelse(pvalue < .001, "***",
                        ifelse(pvalue < .01, "**",
                               ifelse(pvalue < .05, "*", "")))) %>%
  select(Indicator=rhs,
         Beta=std.all,
         SE=se, Z=z,
         'p-value'=pvalue,
         sig=stars) %>%
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

## Examine a lavaan EFA
fit.efa <- efa(data = scale, nfactors = 1:2)
summary(fit.efa)
fitMeasures(fit.efa, measures = "rmsea")
print(fit.efa)

