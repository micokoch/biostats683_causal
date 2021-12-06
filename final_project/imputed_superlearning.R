## Imputing missing values
# Libraries
library(tidyverse)
library(SuperLearner)
library(ltmle)

setwd("final_project")
getwd()

## Reading in dataset
set.seed(252)
ObsDataPrimary <- read.csv("slpexcov1517.csv") # 3,792 obs of 20 variables
# Decided to use primary variables (not derived) with full cleaned set for imputations
names(ObsDataPrimary)
ForImputing <- ObsDataPrimary %>% select(-SEQN, -targetex, -targetslp, -bmicat, -depressed)
# 3,792 obs of 15 variables
# Check NAs in dataset
colSums(is.na(ForImputing))
# There are numerous missing values - 1 educ, 1 marital, 305 income, 206 snoring, 190 apnea, 
# 50 bmi, 159 waist, 2 smoke, 446 alcohol, 301 phq9
# Fill in missing values with MICE

## MICE
# Read in this dataset to avoid running MICE each time
imputed <- read_csv("imputed.csv")
summary(imputed)
# Based on: https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/
# Ensure variables are in correct format:
str(ForImputing)
# Change factor variables accordingly
ForImputing <- ForImputing %>%
  mutate(
    raceeth = as.factor(raceeth),
    educ = as.factor(educ),
    marital = as.factor(marital),
    household = as.factor(household), # factored because categories are bound 0 - 6
    income = as.factor(income),
    snoring = as.factor(snoring),
    apnea = as.factor(apnea),
    smoke = as.factor(smoke),
    alcohol = as.factor(alcohol),
    phq9 = as.factor(phq9) # treat as factor because imputation gives decimal values
  )
str(ForImputing)
# Call in the MICE
library(mice)
init = mice(ForImputing, maxit=0)
meth = init$method
predM = init$predictorMatrix
# Specify methods for imputing binomial, ordinal, and continuous variables
meth[c("educ")]="polyreg"
meth[c("marital")]="logreg"
meth[c("income")]="polyreg"
meth[c("snoring")]="polyreg"
meth[c("apnea")]="logreg"
meth[c("bmi")]="norm.predict"
meth[c("waist")]="norm.predict"
meth[c("smoke")]="logreg"
meth[c("alcohol")]="polyreg"
meth[c("phq9")]="polyreg"
# When MICE was initially run, 'waist' produces values far above values in our dataset
# Decided to bind waist results within the values in dataset
# First try at MICE produced extremely high values for some imputations
summary(ForImputing$waist)
post <- make.post(ForImputing)
# post["waist"] <- "ifdo(c(waist < 62, waist > 170), c(62, 170))"
post["waist"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(62, 170))"
# Run multiple imputations
imputed <- mice(ForImputing, method = meth, predictorMatrix=predM, m=5, post = post)
# Create imputed dataset
imputed <- complete(imputed)
# Check for missing values
sapply(imputed, function(x) sum(is.na(x)))
# No missing values
# Compare imputed and observed data
summary(ForImputing)
summary(imputed)
# Change phq9 back to numeric
ForImputing <- ForImputing %>% mutate(phq9 = as.integer(phq9) - 1)
imputed <- imputed %>% mutate(phq9 = as.integer(phq9) - 1)
# Compare imputed and observed data
summary(ForImputing)
summary(imputed)
# Write to csv to avoid running mice each time
write_csv(imputed, "imputed.csv")
# Check that imputations done reasonably well
# Income
num.income.1 <- as.numeric(ForImputing$income)
num.income.2 <- as.numeric(imputed$income)
hist(num.income.1, breaks = 0:3)
hist(num.income.2, breaks = 0:3)
summary(num.income.1)
summary(num.income.2)
# Mean is only slightly lower (1.945 -> 1.941) and distribution of variables is similar
# Snoring
num.snoring.1 <- as.numeric(ForImputing$snoring)
num.snoring.2 <- as.numeric(imputed$snoring)
hist(num.snoring.1, breaks = 0:4)
hist(num.snoring.2, breaks = 0:4)
summary(num.snoring.1)
summary(num.snoring.2)
# Mean is only slightly lower (2.679 -> 2.674) and distribution of variables is similar
# Apnea
num.apnea.1 <- as.numeric(ForImputing$apnea)
num.apnea.2 <- as.numeric(imputed$apnea)
hist(num.apnea.1, breaks = 0:2)
hist(num.apnea.2, breaks = 0:2)
summary(num.apnea.1)
summary(num.apnea.2)
# Mean is only slightly bigger (1.303 -> 1.305) and distribution of variables is similar
# BMI
hist(ForImputing$bmi)
hist(imputed$bmi)
hist(ForImputing$bmi[ForImputing$bmi > 50])
hist(imputed$bmi[imputed$bmi > 50])
# No big changes in values, especially extreme values
summary(ForImputing$bmi)
summary(imputed$bmi)
# Range is maintained, median is same, mean is slightly lower (29.34 -> 29.33)
# Waist
hist(ForImputing$waist)
hist(imputed$waist)
# Truncated extreme values for waist, distribution still similar
hist(ForImputing$waist[ForImputing$waist > 150])
hist(imputed$waist[imputed$waist > 150])
# There are a few more extreme values, reasonable because of truncated imputation
summary(ForImputing$waist)
summary(imputed$waist)
# Very similar results -> max: 169.6 -> 170 (by design), same median, mean: 101.1 -> 101.25
# Alcohol
num.alcohol.1 <- as.numeric(ForImputing$alcohol)
num.alcohol.2 <- as.numeric(imputed$alcohol)
hist(num.alcohol.1, breaks = 0:3)
hist(num.alcohol.2, breaks = 0:3)
summary(num.alcohol.1)
summary(num.alcohol.2)
# Mean is only slightly lower (2.182 -> 2.181) and distribution of variables is similar
# PHQ-9
hist(ForImputing$phq9)
hist(imputed$phq9)
hist(ForImputing$phq9[ForImputing$phq9 > 10])
hist(imputed$phq9[imputed$phq9 > 10])
# No big changes in values
table(ForImputing$phq9)
table(imputed$phq9)
summary(ForImputing$phq9)
summary(imputed$phq9)
# Range is maintained, median is same, mean is slightly higher (2.875 -> 2.994)

## Convert imputed back to full dataset
# Exercise - use binary variable indicating whether person did 150 min or more exercise/wk
imputed <- imputed %>% 
  mutate(targetex = ifelse(exminwk < 150, 0, 1), .after = exminwk)
summary(imputed) # 3,792 obs of 16 variables
table(imputed$targetex, useNA = "always") # 2,227 are 0, 1,565 are 1 (mean = 0.4127)
# Mean is exactly the same as before imputation
hist(imputed$targetex, breaks = 2)
# Sleep - use binary variable indicating whether person slept 6 hours of less/night
imputed <- imputed %>% 
  mutate(targetslp = ifelse(slphrs > 6, 1, 0), .after = slphrs)
summary(imputed) # 3,792 obs of 17 variables
table(imputed$targetslp) # 830 are 0 and 2,962 are 1 (mean = 0.7811)
# Mean is exactly the same as before imputation
hist(imputed$targetslp, breaks = 2)
# Depression - use binary variable indicating if phq9 is 10 or more
imputed <- imputed %>% mutate(depressed = ifelse(phq9 > 9, 1, ifelse(phq9 < 10, 0, NA)))
summary(imputed) # 3,792 obs of 18 variables
hist(imputed$depressed, breaks = 2)
table(imputed$depressed) # 3,494 not depressed, and 298 depressed (mean = 0.07859)
# Mean is higher (0.07161 -> 0.07859) and distribution of variables is similar
# Add back SEQN to save as csv file
imputed.full <- cbind(ObsDataPrimary$SEQN, imputed) # 3,792 obs of 19 variables
names(imputed.full)[names(imputed.full) == 'ObsDataPrimary$SEQN'] <- 'SEQN'
summary(imputed.full)
# Write to csv to avoid running mice each time
write_csv(imputed.full, "imputed.full.csv")
# Run a logistic regression with fully imputed dataset
glm.imputed.full = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                      bmi + waist + factor(depressed),
                    family = binomial(link = "logit"), data = imputed.full)
summary(glm.imputed.full)
exp(cbind(OR = coef(glm.imputed.full), confint(glm.imputed.full)))
# targetex (OR = 1.3560367 (1.1401719-1.6151190), p = 0.000605)
# Slightly smaller effect size vs unimputed set (1.389 -> 1.356), same p value

#####
## IPTW
# Reading in dataset
set.seed(252)
ObsDataPrimary <- read.csv("imputed.full.csv") # 3,792 obs of 19 variables
# Use only final variable set for analysis
ObsData <- ObsDataPrimary %>% select(-SEQN, -exminwk, -slphrs, -household, -income, -snoring,
                                     -apnea, -smoke, -alcohol, -phq9)
# 3,792 obs of 9 variables
# Check NAs in dataset
colSums(is.na(ObsData))
# There are no missing values
summary(ObsData)

# Estimate the exposure mechanism P(A|W)
prob.AW.reg = glm(targetex ~ age + factor(raceeth) + factor(educ) + factor(marital) + 
                      bmi + waist + factor(depressed),
                    family = binomial(link = "logit"), data = ObsData)
summary(prob.AW.reg)
# Predicted probability of meeting target exercise guidelines given covariates
prob.1W <- predict(prob.AW.reg, type= "response")
# Predicted probability of not meeting target exercise guidelines given covariates
prob.0W <- 1 - prob.1W
# Distribution of predicted probabilities
summary(prob.1W)
summary(prob.0W)
# Create weights for IPTW
wt1 <- as.numeric(ObsData$targetex==1)/prob.1W
wt0 <- as.numeric(ObsData$targetex==0)/prob.0W
summary(wt1)
# The largest weight is 18.423, which does not suggest a near violation of positivity
hist(wt1)
# The vast majority of weights are four and under
hist(wt1[wt1 > 5])
hist(wt1[wt1>8])
table(wt1[wt1>8])
# There is only one weight ~ 18.4, two between 10-12,  eight between 8-10
# Point estimate
iptw <- mean(wt1*ObsData$targetslp) - mean(wt0*ObsData$targetslp)



#####
## Super Learning
ObsData <- ObsData %>% mutate(A = targetex) %>% mutate(Y = targetslp) %>% 
  select(-targetex, -targetslp)

ObsData <- na.omit(ObsData)

SL.library <- c("SL.mean", "SL.glm", "SL.step.interaction", "SL.earth") 
                # 'SL.rpart', "SL.randomForest")

#------------------------------------------
# 2. call ltmle with Super Learner (same libraries)
#------------------------------------------
ltmle.SL <- ltmle(data=ObsData, Anodes='A', Ynodes='Y', abar=list(1,0),
                  SL.library=SL.library, estimate.time = F)
summary(ltmle.SL)

X <- subset(ObsData, select = -Y)

SL.out <- SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family='binomial', cvControl=list(V=5))
SL.out
CV.SL.out<- CV.SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family='binomial', 
                            cvControl=list(V=5), innerCvControl=list(list(V=5)))
summary(CV.SL.out)
