##### Survey GLM

# Libraries
library(tidyverse)
library(survey)

# Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

## Functions
std_error <- function(x) {
  x <- na.omit(x)
  a <- mean(x)
  b <- sd(x)/sqrt(length(x))
  print(c(a, b))
}

# Function to make exposure and outcome as factors, but doesn't seem to fix anything
# format.logistic <- function(x){
#   x <- x %>% mutate(targetex = as.factor(targetex), targetslp = as.factor(targetslp))
#   return(x)
# }

##### Read in and format datasets
## Read in datasets
unimputed.00.comp <- read_csv("unimputed.00.comp.csv")
imputed.01.comp <- read_csv("imputed.01.comp.csv")
imputed.02.comp <- read_csv("imputed.02.comp.csv")
imputed.03.comp <- read_csv("imputed.03.comp.csv")
imputed.04.comp <- read_csv("imputed.04.comp.csv")
imputed.05.comp <- read_csv("imputed.05.comp.csv")
imputed.06.comp <- read_csv("imputed.06.comp.csv")
imputed.07.comp <- read_csv("imputed.07.comp.csv")
imputed.08.comp <- read_csv("imputed.08.comp.csv")
imputed.09.comp <- read_csv("imputed.09.comp.csv")
imputed.10.comp <- read_csv("imputed.10.comp.csv")
imputed.11.comp <- read_csv("imputed.11.comp.csv")
imputed.12.comp <- read_csv("imputed.12.comp.csv")
long.imputed.comp <- read_csv("long.imputed.comp.csv")


## Survey Weights
# Based on: https://stackoverflow.com/questions/64474714/run-svymean-on-all-variables
# Here we use "svydesign" to assign the weights. We will use this new design
# variable "slpex.design" when running our analyses.

# unimputed.00.comp <- format.logistic((unimputed.00.comp))
# str(unimputed.00.comp)
slpex.design.unimp <- svydesign(id      = ~SDMVPSU,
                               strata  = ~SDMVSTRA,
                               weights = ~WTMEC2YR,
                               nest    = TRUE,
                               data    = unimputed.00.comp)
slpex.design.unimp.sub <- subset(slpex.design.unimp, inAnalysis==1)

slpex.design.imp.7 <- svydesign(id      = ~SDMVPSU,
                                strata  = ~SDMVSTRA,
                                weights = ~WTMEC2YR,
                                nest    = TRUE,
                                data    = imputed.07.comp)
slpex.design.imp.7.sub <- subset(slpex.design.imp.7, inAnalysis==1)

slpex.design.long <- svydesign(id      = ~SDMVPSU,
                          strata  = ~SDMVSTRA,
                          weights = ~WTMEC2YR,
                          nest    = TRUE,
                          data    = long.imputed.comp)
slpex.design.long.sub <- subset(slpex.design.long, inAnalysis==1)

# These next steps already done
# # Here we use "subset" to tell "nhanesDesign" that we want to only look at a
# # specific subpopulation (i.e., those age between 20-64 years).
# ageDesign <- subset(nhanesDesign, age > 19 & age < 65)

## Statistics
# We will use "svymean" to calculate the population mean for age. The na.rm
# argument "TRUE" excludes missing values from the calculation. We see that
# the mean age is 41.58 and the standard error is 0.281.

svymean(~age, slpex.design.unimp.sub, na.rm = TRUE)
svymean(~age, slpex.design.unimp, na.rm = TRUE)
std_error(unimputed.00.comp$age)
svymean(~age, slpex.design.imp.7.sub, na.rm = TRUE)
svymean(~age, slpex.design.imp.7, na.rm = TRUE)
std_error(imputed.07.comp$age)
svymean(~age, slpex.design.long.sub, na.rm = TRUE)
svymean(~age, slpex.design.long, na.rm = TRUE)
std_error(long.imputed.comp$age)

svymean(~bmi, slpex.design.unimp.sub, na.rm = TRUE)
svymean(~bmi, slpex.design.unimp, na.rm = TRUE)
std_error(unimputed.00.comp$bmi)
svymean(~bmi, slpex.design.imp.7.sub, na.rm = TRUE)
svymean(~bmi, slpex.design.imp.7, na.rm = TRUE)
std_error(imputed.07.comp$bmi)
svymean(~bmi, slpex.design.long.sub, na.rm = TRUE)
svymean(~bmi, slpex.design.long, na.rm = TRUE)
std_error(long.imputed.comp$bmi)

# Generally standard errors are 2-10 times larger in unweighted samples

# Now we will run a general linear model (glm) with a binomial link function.
# We tell svyglm that nhanesAnalysis is the dataset to use and to apply the
# "svydesign" object "slpex.design.unimp.sub" and others
# Unimputed subset
unimp.sub.wt.final.svy <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + 
                                   factor(marital) + bmi + waist + factor(depressed), 
                                 family = binomial(), 
                                 data   = unimputed.00.comp, 
                                 design = slpex.design.unimp.sub)
summary(unimp.sub.wt.final.svy)
exp(cbind(OR = coef(unimp.sub.wt.final.svy), confint(unimp.sub.wt.final.svy)))

# Imputed dataset #7
imp.7.sub.wt.final.svy <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + 
                                   factor(marital) + bmi + waist + factor(depressed), 
                                 family = binomial(), 
                                 data   = imputed.07.comp, 
                                 design = slpex.design.imp.7.sub)
summary(imp.7.sub.wt.final.svy)
exp(cbind(OR = coef(imp.7.sub.wt.final.svy), confint(imp.7.sub.wt.final.svy)))

# Long imputed dataset
long.sub.wt.final.svy <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + 
                                   factor(marital) + bmi + waist + factor(depressed), 
                                 family = binomial(), 
                                 data   = long.imputed.comp, 
                                 design = slpex.design.long.sub)
summary(long.sub.wt.final.svy)
exp(cbind(OR = coef(long.sub.wt.final.svy), confint(long.sub.wt.final.svy)))

