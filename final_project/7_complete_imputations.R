## Imputing clean dataset

# Libraries
library(tidyverse)
# library(nhanesA)
# library(survey)
# library(gtools)

# Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

##### Read in variables
clean_exslp_1517 <- read_csv("clean_exslp_1517.csv")
# Index column is repeated twice
impute_exslp_1517 <- clean_exslp_1517 %>% 
  select(-c(1:2))
# 11,268 obs of 37 variables


##### EXERCISE
### Rename exercise variables
# clean_exslp_1517 <- clean_exslp_1517 %>% 
#   mutate(
#     vigex = PAQ650,
#     daysvigex = PAQ655,
#     minvigex = PAD660,
#     modex = PAQ665,
#     daysmodex = PAQ670,
#     minmodex = PAD675
#   ) %>% 
#   select(-PAQ650, -PAQ655, -PAD660, -PAQ665, -PAQ670, -PAD675)
# names(clean_exslp_1517)



# ##### - Old attempt
# 
# clean_exslp_15171517 <- read_csv("clean_exslp_15171517.csv")
# clean_exslp_15171517 <- clean_exslp_15171517 %>%
#   mutate(
#     targetslp = as.integer(targetslp),
#     targetex = as.integer(targetex),
#     age = as.integer(age),
#     raceeth = as.factor(raceeth),
#     educ = as.factor(educ),
#     marital = as.factor(marital),
#     household = as.factor(household), # factored because categories are bound 0 - 6
#     income = as.factor(income),
#     snoring = as.factor(snoring),
#     apnea = as.factor(apnea),
#     smoke = as.factor(smoke),
#     alcohol = as.factor(alcohol),
#     phq9 = as.integer(phq9),
#     depressed = as.factor(depressed)
#   )
# clean_exslp_15171517.factor <- clean_exslp_15171517 %>%
#   mutate(
#     targetslp = as.factor(targetslp),
#     targetex = as.factor(targetex))
# 
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
