## Adding weights to dataset
# Libraries
library(tidyverse)
library(nhanesA)
library(survey)
# 
# setwd("final_project")
getwd()
set.seed(252)

slpexcov1517 <- read_csv("slpexcov1517.csv")
slpexcov1517 <- slpexcov1517 %>%
  mutate(
    targetslp = as.integer(targetslp),
    targetex = as.integer(targetex),
    age = as.integer(age),
    raceeth = as.factor(raceeth),
    educ = as.factor(educ),
    marital = as.factor(marital),
    household = as.factor(household), # factored because categories are bound 0 - 6
    income = as.factor(income),
    snoring = as.factor(snoring),
    apnea = as.factor(apnea),
    smoke = as.factor(smoke),
    alcohol = as.factor(alcohol),
    phq9 = as.integer(phq9),
    depressed = as.factor(depressed)
  )
slpexcov1517.factor <- slpexcov1517 %>%
  mutate(
    targetslp = as.factor(targetslp),
    targetex = as.factor(targetex))

# As per NHANES guidelines, use wtmec2yr since we're using BMI
# Obtain weights from DEMO files
# 2015
demogconf2015.wt <- nhanes('DEMO_I') %>% 
  dplyr::select(c('SEQN', 'WTINT2YR', 'WTMEC2YR', 'SDMVPSU', 'SDMVSTRA'))
# 2017
demogconf2017.wt <- nhanes('DEMO_J') %>% 
  dplyr::select(c('SEQN', 'WTINT2YR', 'WTMEC2YR', 'SDMVPSU', 'SDMVSTRA'))
# Combine all confounder tables
demogconf1517.wt <- dplyr::bind_rows(demogconf2015.wt, demogconf2017.wt) # 19,225 obs of 11 variables
summary(demogconf1517.wt)
# Merge slpexcov1517 and weights
slpexcovwts <- merge(slpexcov1517, demogconf1517.wt, by = 'SEQN') # 3,981 obs of 17 variables
summary(slpexcovwts)
head(slpexcovwts)
tail(slpexcovwts)

## Survey Weights
# Here we use "svydesign" to assign the weights. We will use this new design
# variable "nhanesDesign" when running our analyses.

nhanesDesign <- svydesign(id      = ~SDMVPSU,
                          strata  = ~SDMVSTRA,
                          weights = ~WTMEC2YR,
                          nest    = TRUE,
                          data    = slpexcovwts)

# Here we use "subset" to tell "nhanesDesign" that we want to only look at a
# specific subpopulation (i.e., those age between 20-64 years).

ageDesign <- subset(nhanesDesign, age > 19 & age < 65)

## Statistics
# We will use "svymean" to calculate the population mean for age. The na.rm
# argument "TRUE" excludes missing values from the calculation. We see that
# the mean age is 41.58 and the standard error is 0.281.

svymean(~age, ageDesign, na.rm = TRUE)

# Now we will run a general linear model (glm) with a binomial link function.
# We tell svyglm that nhanesAnalysis is the dataset to use and to apply the
# "svydesign" object "ageDesign."

output.wt <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                   bmi + waist + factor(depressed),
                 family = binomial(),
                 data   = slpexcovwts,
                 design = ageDesign)
summary(output.wt)
exp(cbind(OR = coef(output.wt), confint(output.wt)))




