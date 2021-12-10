## Super Learning
# Libraries
library(tidyverse)
library(SuperLearner)
library(ltmle)

setwd("final_project")
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
ObsData <- na.exclude(ObsData) # ObsData now has 3,406 obs of 9 variables
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
# Rename exposure A and outcome Y for simplicity
ObsData <- ObsData %>% rename(Y = targetslp) %>% 
  rename(A = targetex)
# 3,406 obs of 9 variables
# Check NAs in dataset
colSums(is.na(ObsData))
# There are no missing values
summary(ObsData)
# Specify libararies
SL.library <- c("SL.mean", "SL.glm", "SL.step.interaction", "SL.earth", "SL.glmnet", "SL.ranger")
# Create a function to handcode TMLE with Super Learner
run.tmle <- function(ObsData, SL.library){
  ## Simple substitution estimator
  X <-subset(ObsData, select = c(A, age, raceeth, educ, marital, bmi, waist, depressed))
  X1 <- X0 <- X
  X1$A <- 1
  X0$A <- 0
  SL.outcome <- SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family="binomial")
  expY.givenAW <- predict(SL.outcome, newdata=ObsData)$pred
  expY.given1W <- predict(SL.outcome, newdata=X1)$pred
  expY.given0W <- predict(SL.outcome, newdata=X0)$pred
  # Simple substitution estimator risk difference
  PsiHat.SS <- mean(expY.given1W - expY.given0W)
  ## IPTW
  SL.exposure <- SuperLearner(Y=ObsData$A, X=subset(ObsData, select= -c(A, Y)), 
                             SL.library=SL.library, family="binomial")
  probA1.givenW <- SL.exposure$SL.predict
  probA0.givenW <- 1- probA1.givenW
  # Clever covariate for risk difference
  H.AW <- as.numeric(ObsData$A==1)/probA1.givenW - as.numeric(ObsData$A==0)/probA0.givenW
  H.1W <- 1/probA1.givenW
  H.0W <- -1/probA0.givenW
  # IPTW estimator risk difference
  PsiHat.IPTW <- mean(H.AW*ObsData$Y)
  ## Targeting and TMLE
  logitUpdate <- glm(ObsData$Y ~ -1 +offset(qlogis(expY.givenAW)) + H.AW, family='binomial')
  epsilon <- logitUpdate$coef
  expY.givenAW.star <- plogis(qlogis(expY.givenAW)+ epsilon*H.AW)
  expY.given1W.star <- plogis(qlogis(expY.given1W)+ epsilon*H.1W)
  expY.given0W.star <- plogis(qlogis(expY.given0W)+ epsilon*H.0W)
  # TMLE risk difference
  PsiHat.TMLE <- mean(expY.given1W.star - expY.given0W.star)
  # List the point estimates
  estimates <- data.frame(cbind(PsiHat.SS=PsiHat.SS, PsiHat.IPTW, PsiHat.TMLE))
  predictions <- data.frame(cbind(expY.givenAW.star, expY.given1W.star, expY.given0W.star))
  colnames(predictions)<- c('givenAW', 'given1W', 'given0W')
  list(estimates=estimates, predictions=predictions, H.AW=H.AW)
}

out <- run.tmle(ObsData=ObsData, SL.library=SL.library)
est <- out$estimates
est*100

#####
## Super Learning 2
colSums(is.na(ObsData))

SL.library <- c("SL.mean", "SL.glm", "SL.step.interaction", "SL.earth", "SL.glmnet", "SL.ranger")

ltmle.SL <- ltmle(data=ObsData, Anodes='A', Ynodes='Y', abar=list(1,0),
                  SL.library=SL.library, estimate.time = F)
summary(ltmle.SL)

X <- subset(ObsData, select = -Y)

SL.out <- SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family='binomial', cvControl=list(V=5))
SL.out
CV.SL.out<- CV.SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, family='binomial', 
                            cvControl=list(V=5), innerCvControl=list(list(V=5)))
summary(CV.SL.out)
