##### Survey GLM

# Libraries
library(tidyverse)
library(survey)

# Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

##### Read in and format datasets
## Read in datasets
unimputed.0.comp <- read_csv("unimputed.0.comp.csv")
imputed.1.comp <- read_csv("imputedData.1.comp.csv")
imputed.2.comp <- read_csv("imputedData.2.comp.csv")
imputed.3.comp <- read_csv("imputedData.3.comp.csv")
imputed.4.comp <- read_csv("imputedData.4.comp.csv")
imputed.5.comp <- read_csv("imputedData.5.comp.csv")
imputed.6.comp <- read_csv("imputedData.6.comp.csv")
imputed.7.comp <- read_csv("imputedData.7.comp.csv")
imputed.8.comp <- read_csv("imputedData.8.comp.csv")
imputed.9.comp <- read_csv("imputedData.9.comp.csv")
imputed.10.comp <- read_csv("imputedData.10.comp.csv")
imputed.11.comp <- read_csv("imputedData.11.comp.csv")
imputed.12.comp <- read_csv("imputedData.12.comp.csv")

### To do : survey glm, SuperLearner, Laura email

# 
# ## Survey Weights
# # Here we use "svydesign" to assign the weights. We will use this new design
# # variable "nhanesDesign" when running our analyses.
# 
# nhanesDesign <- svydesign(id      = ~SDMVPSU,
#                           strata  = ~SDMVSTRA,
#                           weights = ~WTMEC2YR,
#                           nest    = TRUE,
#                           data    = clean_exslp_1517wts)
# 
# # Here we use "subset" to tell "nhanesDesign" that we want to only look at a
# # specific subpopulation (i.e., those age between 20-64 years).
# 
# ageDesign <- subset(nhanesDesign, age > 19 & age < 65)
# 
# ## Statistics
# # We will use "svymean" to calculate the population mean for age. The na.rm
# # argument "TRUE" excludes missing values from the calculation. We see that
# # the mean age is 41.58 and the standard error is 0.281.
# 
# svymean(~age, ageDesign, na.rm = TRUE)
# 
# # Now we will run a general linear model (glm) with a binomial link function.
# # We tell svyglm that nhanesAnalysis is the dataset to use and to apply the
# # "svydesign" object "ageDesign."
# 
# output.wt <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + 
#                    factor(marital) + bmi + waist + factor(depressed),
#                  family = binomial(),
#                  data   = clean_exslp_1517wts,
#                  design = ageDesign)
# summary(output.wt)
# exp(cbind(OR = coef(output.wt), confint(output.wt)))
# 


