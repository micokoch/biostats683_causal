## Super Learning
# Libraries
library(tidyverse)
library(SuperLearner)
library(ltmle)

# setwd("final_project")
getwd()
# Set seed
set.seed(252)
## Reading in dataset
ObsDataPrimary <- read.csv("slpexcov1517.csv") # 3,792 obs of 20 variables
# Use only final variable set for analysis
ObsData <- ObsDataPrimary %>% select(-SEQN, -exminwk, -slphrs, -household, -income, -snoring,
                              -apnea, -bmicat, -smoke, -alcohol, -phq9)
# 3,792 obs of 9 variables
# Check NAs in dataset
colSums(is.na(ObsData))
# There are numerous missing values - 1 educ, 1 marital, 50 bmi, 159 waist, 301 depressed
# Drop missing values
ObsData <- na.exclude(ObsData) # ObsData now has 3,406 obs of 9 variables (dropped 386 obs)
colSums(is.na(ObsData)) # There are no missing values
# Rename exposure "A" and outcome "Y"
summary(ObsData)
ObsData <- ObsData %>% 
  rename(A = targetex) %>% 
  rename(Y = targetslp) %>% 
  relocate(Y)
names(ObsData)

#####
## IPTW
# Estimate the exposure mechanism P(A|W)
prob.AW.reg = glm(A ~ age + factor(raceeth) + factor(educ) + factor(marital) + 
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
wt1 <- as.numeric(ObsData$A==1)/prob.1W
wt0 <- as.numeric(ObsData$A==0)/prob.0W
summary(wt1)
# The largest weight is 19.795, which does not suggest a near violation of positivity
hist(wt1)
# The vast majority of weights are four and under
hist(wt1[wt1 > 5])
hist(wt1[wt1>8])
table(wt1[wt1>8])
# Biggest weight ~ 19.8, two between 10-14,  nine between 8-10
summary(wt0)
# The largest weight is 5.409, which does not suggest a positivity violation
hist(wt0, breaks = 0:6)
# Almost all weights between 1 and 2
hist(wt0[wt0 > 5])
# There are five weights between 5-5.5

# Point estimate for risk difference
iptw.rd <- mean(wt1*ObsData$Y) - mean(wt0*ObsData$Y)
iptw.rd
# Point estimate risk difference = 0.05629606
# Point estimate for odds ratio
iptw.or <- (mean(wt1*ObsData$Y)/(1-mean(wt1*ObsData$Y))) /
  (mean(wt0*ObsData$Y)/(1-(mean(wt0*ObsData$Y))))
iptw.or
# Point estimate odds ratio = 1.390607

# Truncate weights at 8
sum(wt1 > 8) # 12 values over 8
wt1.trunc <- wt1
wt1.trunc[ wt1.trunc > 8] <- 8
sum(wt0 > 8)
wt0.trunc <- wt0
wt0.trunc[wt0.trunc > 8] <- 8
# IPTW estimand with truncated weights
# Risk difference
iptw.tr.rd <- mean(wt1.trunc*ObsData$Y) - mean(wt0.trunc*ObsData$Y)
iptw.tr.rd
# Point estimate for truncated risk difference = 0.05075302
# Odds ratio
iptw.tr.or <- (mean(wt1.trunc*ObsData$Y)/(1-mean(wt1.trunc*ObsData$Y))) /
  (mean(wt0.trunc*ObsData$Y)/(1-(mean(wt0.trunc*ObsData$Y))))
iptw.tr.or
# Point estimate for truncated odds ratio = 1.342248

# Stabilized IPTW estimator - Modified Horvitz-Thompson estimator
# Risk difference
iptw.st.rd <- sum(wt1*ObsData$Y) / sum(wt1) - sum(wt0*ObsData$Y) / sum(wt0)
iptw.st.rd
# RD = 0.04822426
# Odds ratio
iptw.st.or <- ((sum(wt1.trunc*ObsData$Y)/sum(wt1))/
                 ((sum(1-wt1.trunc*ObsData$Y))/sum(wt1))) /
  ((sum(wt0.trunc*ObsData$Y)/sum(wt0))/
     ((sum(1-wt0.trunc*ObsData$Y))/sum(wt0)))
iptw.st.or
# OR = 1.342248

# Unadjusted Estimator
unadj.rd <- mean(ObsData[ObsData$A==1, 'Y']) - 
  mean(ObsData[ObsData$A==0, 'Y'])
unadj.rd
# RD = 0.06442889
unadj.or <- (mean(ObsData[ObsData$A==1, 'Y'])/
               (1-mean(ObsData[ObsData$A==1, 'Y']))) /
  (mean(ObsData[ObsData$A==0, 'Y'])/
     (1-(mean(ObsData[ObsData$A==0, 'Y']))))
unadj.or
# OR = 1.465781

# G Computation
outcome.reg = glm(Y ~ A + age + factor(raceeth) + factor(educ) + factor(marital) + 
                    bmi + waist + factor(depressed),
                  family = binomial(link = "logit"), data = ObsData)
exp <- unexp <- ObsData
exp$A <- 1
unexp$A <- 0
# Risk difference
SS.rd <- mean(predict(outcome.reg, newdata=exp, type='response')) - 
  mean(predict(outcome.reg, newdata=unexp, type='response'))
SS.rd
# SS = 0.05367993
# Odds ratio
SS.or <- ((mean(predict(outcome.reg, newdata=exp, type='response'))) / 
            (1-mean(predict(outcome.reg, newdata=exp, type='response')))) /
  ((mean(predict(outcome.reg, newdata=unexp, type='response'))) / 
     (1-mean(predict(outcome.reg, newdata=unexp, type='response'))))
SS.or
# SS = 1.373851

# Compare different results
# Risk difference
rd <- round(data.frame(iptw.rd, iptw.tr.rd, iptw.st.rd, unadj.rd, SS.rd)*100, 2)
rd
# Odds ratio
or <- round(data.frame(iptw.or, iptw.tr.or, iptw.st.or, unadj.or, SS.or), 2)
or
# Beautify tables
library(DT)
datatable(or)
library(gridExtra)
pdf("or_results.pdf", height=11, width=8.5)
grid.table(or)
dev.off()
library(gapminder)
library(gt)
or.table <- or[, c(4, 5, 1, 2, 3)]
or.table <- or.table %>% 
  rename(Unadjusted = unadj.or, GComputation = SS.or, IPTW = iptw.or, Truncated = iptw.tr.or, 
         Stabilized = iptw.st.or)
or.table %>% gt()

#####
## Super Learning
summary(ObsData)
# 3,406 obs of 9 variables, no NAs
# Specify libraries
SL.library <- c("SL.mean", "SL.glm", "SL.step.interaction", "SL.earth", "SL.glmnet", "SL.ranger")
# Create a function to handcode TMLE with Super Learner
run.tmle <- function(ObsData, SL.library){
  ## Simple substitution estimator
  # dataframe X with baseline covariates and exposure
  X <- subset(ObsData, select = c(A, age, raceeth, educ, marital, bmi, waist, depressed))
  # set the exposure=1 in X1 and the exposure=0 in X0
  X1 <- X0 <- X
  X1$A <- 1
  X0$A <- 0
  # Estimate E_0(Y|A,W) with Super Learner
  SL.outcome <- SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family="binomial")
  # get the expected outcome, given the observed exposure and covariates
  expY.givenAW <- predict(SL.outcome, newdata=ObsData)$pred
  # expected outcome, given A=1 and covariates
  expY.given1W <- predict(SL.outcome, newdata=X1)$pred
  # expected outcome, given A=0 and covariates
  expY.given0W <- predict(SL.outcome, newdata=X0)$pred
  # Simple substitution estimator risk difference
  PsiHat.SS.rd <- mean(expY.given1W - expY.given0W)
  # Simple substitution estimator odds ratio
  PsiHat.SS.or <- mean((expY.given1W/(1-expY.given1W))/(expY.given0W/(1-expY.given0W)))
  
  ## IPTW
  # Super Learner for the exposure mechanism P_0(A=1|W)
  SL.exposure <- SuperLearner(Y=ObsData$A, X=subset(ObsData, select= -c(A, Y)), 
                             SL.library=SL.library, family="binomial")
  # generate the predicted prob of being exposed, given baseline cov
  probA1.givenW <- SL.exposure$SL.predict
  # generate the predicted prob of not being exposed, given baseline cov
  probA0.givenW <- 1- probA1.givenW
  # Clever covariate for risk difference
  H.AW <- as.numeric(ObsData$A==1)/probA1.givenW - as.numeric(ObsData$A==0)/probA0.givenW
  # also want to evaluate the clever covariate at A=1 and A=0 for all participants
  H.1W <- 1/probA1.givenW
  H.0W <- -1/probA0.givenW
  # IPTW estimator risk difference
  PsiHat.IPTW.rd <- mean(H.AW*ObsData$Y)
  # IPTW estimator odds ratio
  PsiHat.IPTW.or <- (mean(H.1W*ObsData$Y)/(1-mean(H.1W*ObsData$Y))) /
    (mean(H.0W*ObsData$Y)/(1-(mean(H.0W*ObsData$Y))))
  
  ## Targeting and TMLE
  # Update the initial estimator of E_0(Y|A,W)
  # run logistic regression of Y on H.AW using the logit of the esimates as offset
  logitUpdate <- glm(ObsData$Y ~ -1 + offset(qlogis(expY.givenAW)) + H.AW, family='binomial')
  epsilon <- logitUpdate$coef
  # obtain the targeted estimates
  expY.givenAW.star <- plogis(qlogis(expY.givenAW) + epsilon*H.AW)
  expY.given1W.star <- plogis(qlogis(expY.given1W) + epsilon*H.1W)
  expY.given0W.star <- plogis(qlogis(expY.given0W) + epsilon*H.0W)
  # TMLE point estimate - risk difference
  PsiHat.TMLE.rd <- mean(expY.given1W.star - expY.given0W.star)
  # TMLE point estimate - odds ratio
  PsiHat.TMLE.or <- (mean(expY.given1W.star)/(1-mean(expY.given1W.star))) /
    (mean(expY.given0W.star)/(1-(mean(expY.given0W.star))))
  
  ## Return a list with the point estimates, targeted estimates of E_0(Y|A,W),
  # and the vector of clever covariates
  estimates <- data.frame(cbind(PsiHat.SS.rd=PsiHat.SS.rd, PsiHat.IPTW.rd, PsiHat.TMLE.rd, 
                                PsiHat.SS.or, PsiHat.IPTW.or, PsiHat.TMLE.or))
  predictions <- data.frame(cbind(expY.givenAW.star, expY.given1W.star, expY.given0W.star))
  colnames(predictions)<- c('givenAW', 'given1W', 'given0W')
  list(estimates.rd=estimates, predictions=predictions, H.AW=H.AW)
}

out_run_tmle <- run.tmle(ObsData=ObsData, SL.library=SL.library)
save(out_run_tmle, file = "out_run_tmle.RData")
est_run_tmle <- out_run_tmle$estimates
est_run_tmle
save(est_run_tmle, file = "est_run_tmle.RData")
est_run_tmle_gt <- est_run_tmle %>% gt()
save(est_run_tmle_gt, file = "est_run_tmle_gt.RData")
pander(est_run_tmle)

## Estimate the variance of TMLE
# clever covariate
H.AW <- out_run_tmle$H.AW
# targeted predictions
expY.AW.star <- out_run_tmle$predictions[,'givenAW']
expY.1W.star <- out_run_tmle$predictions[,'given1W']
expY.0W.star <- out_run_tmle$predictions[,'given0W']
# point estimate
PsiHat.TMLE <- est_run_tmle$PsiHat.TMLE.rd
# plug-in
IC <- H.AW*(ObsData$Y - expY.AW.star) + expY.1W.star - expY.0W.star - PsiHat.TMLE
summary(IC)
# estimate sigma^2 with the variance of the IC divided by n
n <- nrow(ObsData)
varHat.IC <- var(IC)/n
varHat.IC
# standard error estimate
se <- sqrt(varHat.IC)
se
# obtain 95% two-sided confidence intervals:
alpha <- 0.05
c(PsiHat.TMLE + qnorm(alpha/2, lower.tail=T)*se,
  PsiHat.TMLE + qnorm(alpha/2, lower.tail=F)*se)
# calculate the pvalue
pvalue <- 2* pnorm(abs(PsiHat.TMLE/se), lower.tail=F )
pvalue

## NP-Boot for B=100 bootstrapped samples
# number of bootstrap samples
B = 100
# data frame for estimates based on the boot strap sample
estimates <- data.frame(matrix(NA, nrow=B, ncol=6))
# for loop from b=1 to total number of bootstrap samples
for(b in 1:B){
  # sample the indices 1 to n with replacement
  bootIndices <- sample(1:n, replace=T)
  bootData <- ObsData[bootIndices,]
  # calling the above function
  estimates[b,] <- run.tmle(ObsData=bootData, SL.library=SL.library)$estimates
  # keep track of the iteractions completed
  print(b)
}
colnames(estimates)<-c("SimpSubs", "IPTW", "TMLE")
save(estimates, file='bootstrap_run_tmle.Rdata')

# Explore the bootstrapped point estimates
summary(estimates)
#saving the histograms as a pdf
pdf(file="bootstrap_run_tmle.pdf")
par(mfrow=c(3,1))
hist(estimates[,1], main="Histogram of point estimates from the Simple Substitution estimator 
     over 100 bootstrapped samples", xlab="Point Estimates")
hist(estimates[,2], main="Histogram of point estimates from IPTW estimator 
     over B bootstrapped samples", xlab="Point Estimates")
hist(estimates[,3], main="Histogram of point estimates from TMLE 
     over B bootstrapped samples", xlab="Point Estimates")
dev.off()

## 95% Confidence intervals using a normal distribution & via quantiles
create.CI <- function(pt, boot, alpha=0.05){
  Zquant <- qnorm(alpha/2, lower.tail=F)
  CI.normal <- c(pt - Zquant*sd(boot), pt + Zquant*sd(boot) )
  CI.quant <- quantile(boot, prob=c(0.025,0.975) )
  out <- data.frame(rbind(CI.normal, CI.quant))*100
  colnames(out)<- c('CI.lo', 'CI.hi')
  out
}
# The point estimate 'pt' is from the original dataset
# Simple Subs
est_run_tmle$PsiHat.SS.rd
# [1] 0.04451638
create.CI(pt=est_run_tmle$PsiHat.SS.rd, boot=estimates[,"SimpSubs"])
#              CI.lo    CI.hi
# CI.normal 1.887290 7.015985
# CI.quant  1.314127 5.769350

# IPTW
est_run_tmle$PsiHat.IPTW.rd
# [1] 0.005275451
create.CI(pt=est_run_tmle$PsiHat.IPTW.rd, boot=estimates[,"IPTW"])
#              CI.lo    CI.hi
# CI.normal -2.441416  3.496507
# CI.quant  -11.560677 -6.281583

# TMLE
est_run_tmle$PsiHat.TMLE.rd
# [1] 0.05016527
create.CI(pt=est_run_tmle$PsiHat.TMLE.rd, boot=estimates[,"TMLE"])
#              CI.lo    CI.hi
# CI.normal 2.305681 7.727374
# CI.quant  2.694661 8.044353

#####
## Super Learning 2 - LTMLE
ObsData <- ObsData %>% 
  relocate(A) %>% 
  relocate(Y, .after = last_col())
# Check ObsData
colSums(is.na(ObsData))
# SL libraries
SL.library <- c("SL.mean", "SL.glm", "SL.step.interaction", "SL.earth", "SL.glmnet", "SL.ranger")
# Covariates
lnods <- c('age', 'raceeth', 'educ', 'marital', 'bmi', 'waist', 'depressed')
#LTMLE
ltmle.SL <- ltmle(data=ObsData, Anodes='A', Lnodes = lnods, Ynodes='Y', abar=list(1,0),
                  SL.library=SL.library, estimate.time = F)
save(ltmle.SL, file = "ltmle.SL.RData")
ltmle.SL.summary <- summary(ltmle.SL)
ltmle.SL.summary
save(ltmle.SL.summary, file = "ltmle.SL.summary.RData")
# Treatment Estimate:
# Parameter Estimate:  0.81594 
# Estimated Std Err:  0.010293 
# p-value:  <2e-16 
# 95% Conf Interval: (0.79576, 0.83611) 
# 
# Control Estimate:
#   Parameter Estimate:  0.75151 
# Estimated Std Err:  0.0096934 
# p-value:  <2e-16 
# 95% Conf Interval: (0.73251, 0.77051) 
# 
# Additive Treatment Effect:
#   Parameter Estimate:  0.064429 
# Estimated Std Err:  0.014139 
# p-value:  5.1918e-06 
# 95% Conf Interval: (0.036717, 0.09214) 
# 
# Relative Risk:
#   Parameter Estimate:  1.0857 
# Est Std Err log(RR):  0.018042 
# p-value:  5.1365e-06 
# 95% Conf Interval: (1.048, 1.1248) 
# 
# Odds Ratio:
#   Parameter Estimate:  1.4658 
# Est Std Err log(OR):  0.085974 
# p-value:  8.6786e-06 
# 95% Conf Interval: (1.2385, 1.7348)

ltmle.SL.rd <- ltmle.SL.summary$effect.measures$ATE$estimate
ltmle.SL.or <- ltmle.SL.summary$effect.measures$OR$estimate
ltmle.SL.ee <- c(rd = ltmle.SL.rd, or = ltmle.SL.or)
ltmle.SL.ee
save(ltmle.SL.ee, file = "ltmle.SL.ee.RData")

X <- subset(ObsData, select = -Y)

ltmle.SL.out <- SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family='binomial', cvControl=list(V=20))
ltmle.SL.out
save(ltmle.SL.out, file = "ltmle.SL.out.RData")
#                              Risk       Coef
# SL.mean_All             0.1726459 0.05405124
# SL.glm_All              0.1694628 0.32583359
# SL.step.interaction_All 0.1707377 0.00000000
# SL.earth_All            0.1692558 0.37327442
# SL.glmnet_All           0.1694632 0.00000000
# SL.ranger_All           0.1716945 0.24684075

# Worth noting that when V=5, glm got the largest coef
#                              Risk       Coef
# SL.mean_All             0.1726186 0.02435327
# SL.glm_All              0.1693742 0.41852338
# SL.step.interaction_All 0.1702425 0.00000000
# SL.earth_All            0.1699335 0.30108319
# SL.glmnet_All           0.1693715 0.00000000
# SL.ranger_All           0.1718333 0.25604016

ltmle.CV.SL.out<- CV.SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family='binomial', 
                            cvControl=list(V=5), innerCvControl=list(list(V=20)))
save(ltmle.CV.SL.out, file = "ltmle.CV.SL.out.RData")
ltmle.CV.SL.summary <- summary(ltmle.CV.SL.out)
ltmle.CV.SL.summary
save(ltmle.CV.SL.summary, file = "ltmle.CV.SL.summary.RData")
# Risk is based on: Mean Squared Error
# 
# All risk estimates are based on V =  5 
# 
# Algorithm     Ave        se     Min     Max
# Super Learner 0.16876 0.0038836 0.15115 0.18829
# Discrete SL 0.17019 0.0039290 0.15187 0.18975
# SL.mean_All 0.17288 0.0039704 0.15351 0.19464
# SL.glm_All 0.17037 0.0039274 0.15205 0.18972
# SL.step.interaction_All 0.17188 0.0039643 0.15145 0.19203
# SL.earth_All 0.16991 0.0039615 0.15442 0.18975
# SL.glmnet_All 0.17036 0.0039254 0.15187 0.18990
# SL.ranger_All 0.17147 0.0039220 0.15570 0.19166




