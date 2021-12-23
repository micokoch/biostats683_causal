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
glm.imputed.full = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + 
                         factor(marital) + bmi + waist + factor(depressed),
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
summary(wt0)
# The largest weight is 5.0815, which does not suggest a positivity violation
hist(wt0, breaks = 0:6)
# Almost all weights between 1 and 2

# Point estimate for risk difference
iptw.rd <- mean(wt1*ObsData$targetslp) - mean(wt0*ObsData$targetslp)
iptw.rd
# Point estimate risk difference = 0.054164
# Point estimate for odds ratio
iptw.or <- (mean(wt1*ObsData$targetslp)/(1-mean(wt1*ObsData$targetslp))) /
  (mean(wt0*ObsData$targetslp)/(1-(mean(wt0*ObsData$targetslp))))
iptw.or
# Point estimate odds ratio = 1.376661

# Truncate weights at 8
sum(wt1 > 8)
wt1.trunc <- wt1
wt1.trunc[ wt1.trunc > 8] <- 8
sum(wt0 > 8)
wt0.trunc <- wt0
wt0.trunc[wt0.trunc > 8] <- 8
# IPTW estimand with truncated weights
# Risk difference
iptw.tr.rd <- mean(wt1.trunc*ObsData$targetslp) - mean(wt0.trunc*ObsData$targetslp)
iptw.tr.rd
# Point estimate for truncated risk difference = 0.04982629
# Odds ratio
iptw.tr.or <- (mean(wt1.trunc*ObsData$targetslp)/(1-mean(wt1.trunc*ObsData$targetslp))) /
              (mean(wt0.trunc*ObsData$targetslp)/(1-(mean(wt0.trunc*ObsData$targetslp))))
iptw.tr.or
# Point estimate for truncated odds ratio = 1.338776

# Stabilized IPTW estimator - Modified Horvitz-Thompson estimator
# Risk difference
iptw.st.rd <- sum(wt1*ObsData$targetslp) / sum(wt1) - sum(wt0*ObsData$targetslp) / sum(wt0)
iptw.st.rd
# RD = 0.04489871
# Odds ratio
iptw.st.or <- ((sum(wt1.trunc*ObsData$targetslp)/sum(wt1))/
                 ((sum(1-wt1.trunc*ObsData$targetslp))/sum(wt1))) /
                 ((sum(wt0.trunc*ObsData$targetslp)/sum(wt0))/
                    ((sum(1-wt0.trunc*ObsData$targetslp))/sum(wt0)))
iptw.st.or
# OR = 1.338776

# Unadjusted Estimator
unadj.rd <- mean(ObsData[ObsData$targetex==1, 'targetslp']) - 
  mean(ObsData[ObsData$targetex==0, 'targetslp'])
unadj.rd
# RD = 0.06152721
unadj.or <- (mean(ObsData[ObsData$targetex==1, 'targetslp'])/
               (1-mean(ObsData[ObsData$targetex==1, 'targetslp']))) /
              (mean(ObsData[ObsData$targetex==0, 'targetslp'])/
                 (1-(mean(ObsData[ObsData$targetex==0, 'targetslp']))))
unadj.or
# OR = 1.445504

# G Computation
outcome.reg = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                         bmi + waist + factor(depressed),
                       family = binomial(link = "logit"), data = ObsData)
exp <- unexp <- ObsData
exp$targetex <- 1
unexp$targetex <- 0
# Risk difference
SS.rd <- mean(predict(outcome.reg, newdata=exp, type='response')) - 
  mean(predict(outcome.reg, newdata=unexp, type='response'))
SS.rd
# SS = 0.04951699
# Odds ratio
SS.or <- ((mean(predict(outcome.reg, newdata=exp, type='response'))) / 
  (1-mean(predict(outcome.reg, newdata=exp, type='response')))) /
  ((mean(predict(outcome.reg, newdata=unexp, type='response'))) / 
     (1-mean(predict(outcome.reg, newdata=unexp, type='response'))))
SS.or
# SS = 1.343719

# Compare different results
# Risk difference
round(data.frame(iptw.rd, iptw.tr.rd, iptw.st.rd, unadj.rd, SS.rd)*100, 2)
# Odds ratio
round(data.frame(iptw.or, iptw.tr.or, iptw.st.or, unadj.or, SS.or), 2)

#####
## Super Learning
library(SuperLearner)
# Read in data to be safe
set.seed(252)
ObsDataPrimary <- read.csv("imputed.full.csv") # 3,792 obs of 19 variables
# Use only final variable set for analysis
ObsData <- ObsDataPrimary %>% select(-SEQN, -exminwk, -slphrs, -household, -income, -snoring,
                                     -apnea, -smoke, -alcohol, -phq9)
# Rename exposure A and outcome Y for simplicity
ObsData <- ObsData %>% rename(Y = targetslp) %>% 
  rename(A = targetex)
# 3,792 obs of 9 variables
# Check NAs in dataset
colSums(is.na(ObsData))
# There are no missing values
summary(ObsData)
# Specify libararies
SL.library <- c("SL.mean", "SL.glm", "SL.step.interaction", "SL.earth", "SL.glmnet", "SL.ranger")

# Hardcoding TMLE in Super Learner didn't work well
# # Create a function to handcode TMLE with Super Learner
# run.tmle <- function(ObsData, SL.library){
#   ## Simple substitution estimator
#   X <-subset(ObsData, select = c(A, age, raceeth, educ, marital, bmi, waist, depressed))
#   X1 <- X0 <- X
#   X1$A <- 1
#   X0$A <- 0
#   SL.outcome <- SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family="binomial")
#   expY.givenAW <- predict(SL.outcome, newdata=ObsData)$pred
#   expY.given1W <- predict(SL.outcome, newdata=X1)$pred
#   expY.given0W <- predict(SL.outcome, newdata=X0)$pred
#   # Simple substitution estimator risk difference
#   PsiHat.SS.rd <- mean(expY.given1W - expY.given0W)
#   # Simple substitution estimator odds ratio
#   PsiHat.SS.or <- (mean(expY.given1W)/(1-mean(expY.given1W))) /
#     (mean(expY.given0W)/(1-(mean(expY.given0W))))
#   ## IPTW
#   SL.exposure<- SuperLearner(Y=ObsData$A, X=subset(ObsData, select= -c(A, Y)), 
#                              SL.library=SL.library, family="binomial")
#   probA1.givenW <- SL.exposure$SL.predict
#   probA0.givenW <- 1- probA1.givenW
#   # Clever covariate for risk difference
#   H.AW <- as.numeric(ObsData$A==1)/probA1.givenW - as.numeric(ObsData$A==0)/probA0.givenW
#   H.1W <- 1/probA1.givenW
#   H.0W <- -1/probA0.givenW
#   # Clever covariate for odds ratio
#   H.AW.or <- ((as.numeric(ObsData$A==1)/probA1.givenW)/
# (1-(as.numeric(ObsData$A==1)/probA1.givenW))) / 
#     ((as.numeric(ObsData$A==0)/probA0.givenW)/
# (1-(as.numeric(ObsData$A==0)/probA0.givenW)))
#   # IPTW estimator risk difference
#   PsiHat.IPTW.rd <- mean(H.AW*ObsData$Y)
#   # IPTW estimator odds ratio
#   PsiHat.IPTW.or <- mean(H.AW.or*ObsData$Y)
#   ## Targeting and TMLE
#   logitUpdate <- glm(ObsData$Y ~ -1 +offset(qlogis(expY.givenAW)) + H.AW, family='binomial')
#   epsilon <- logitUpdate$coef
#   expY.givenAW.star <- plogis(qlogis(expY.givenAW)+ epsilon*H.AW)
#   expY.given1W.star <- plogis(qlogis(expY.given1W)+ epsilon*H.1W)
#   expY.given0W.star <- plogis(qlogis(expY.given0W)+ epsilon*H.0W)
#   # TMLE risk difference
#   PsiHat.TMLE.rd <- mean(expY.given1W.star - expY.given0W.star)
#   # TMLE odds ratio
#   PsiHat.TMLE.or <- (mean(expY.given1W.star)/(mean(1-expY.given1W.star))) / 
#     (mean(expY.given0W.star)/(mean(1-expY.given0W.star)))
#   # List the point estimates
#   estimates.rd <- data.frame(cbind(PsiHat.SS.rd=PsiHat.SS.rd, PsiHat.IPTW.rd, PsiHat.TMLE.rd))
#   estimates.or <- data.frame(cbind(PsiHat.SS.or=PsiHat.SS.or, PsiHat.IPTW.or, PsiHat.TMLE.or))
#   predictions <- data.frame(cbind(expY.givenAW.star, expY.given1W.star, expY.given0W.star))
#   colnames(predictions)<- c('givenAW', 'given1W', 'given0W')
#   list(estimates=estimates.rd, predictions=predictions, H.AW=H.AW)
#   list(estimates=estimates.or, predictions=predictions, H.AW=H.AW.or)
# }
# 
# out <- run.tmle(ObsData=ObsData, SL.library=SL.library)
# est.rd <- out$estimates.rd
# est.rd*100
# est.or <- out$estimates.or
# est.or


############
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

SL.out <- SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, 
                       family='binomial', cvControl=list(V=5))
SL.out
CV.SL.out<- CV.SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family='binomial', 
                            cvControl=list(V=5), innerCvControl=list(list(V=5)))
summary(CV.SL.out)
