##### LTMLE

# Libraries
library(tidyverse)
library(SuperLearner)
library(ltmle)
library(gt)
library(pander)

# Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

# Specify SuperLearner libraries
SL.library <- c("SL.mean", "SL.glm", "SL.step.interaction")#, "SL.earth", "SL.glmnet", "SL.ranger")

## Functions
# Formatting the dataset appropriately
format.slpex.data <- function(x){
  x <- x %>% 
    mutate(
      .imp = as.integer(.imp),
      .id = as.integer(.id),
      vigex = as.factor(vigex),
      daysvigex = as.integer(daysvigex),
      minvigex = as.integer(minvigex),
      vigexminwk = as.integer(vigexminwk),
      modex = as.factor(modex),
      daysmodex = as.integer(daysmodex),
      minmodex = as.integer(minmodex),
      modexminwk = as.integer(modexminwk),
      exminwk = as.integer(exminwk),
      targetex = as.integer(targetex),
      slphrs = as.numeric(slphrs),
      targetslp = as.integer(targetslp),
      snoring = as.factor(snoring),
      apnea = as.factor(apnea),
      gender = as.factor(gender),
      age = as.integer(age),
      raceeth = as.factor(raceeth),
      usborn = as.factor(usborn),
      educ = as.factor(educ),
      marital = as.factor(marital),
      pregnancy = as.factor(pregnancy),
      household = as.factor(household),
      income = as.factor(income),
      bmi = as.numeric(bmi),
      waist = as.numeric(waist),
      smoke = as.factor(smoke),
      alcohol = as.factor(alcohol),
      phq01 = as.integer(phq01),
      phq02 = as.integer(phq02),
      phq03 = as.integer(phq03),
      phq04 = as.integer(phq04),
      phq05 = as.integer(phq05),
      phq06 = as.integer(phq06),
      phq07 = as.integer(phq07),
      phq08 = as.integer(phq08),
      phq09 = as.integer(phq09),
      phqsum = as.integer(phqsum),
      depressed = as.factor(depressed),
      inAnalysis = as.factor(inAnalysis),
      SEQN = as.integer(SEQN),
      WTINT2YR = as.numeric(WTINT2YR),
      WTMEC2YR = as.numeric(WTMEC2YR),
      SDMVPSU = as.integer(SDMVPSU),
      SDMVSTRA = as.integer(SDMVSTRA)
    )
  return(x)
}

# Function to make exposure and outcome as factors
factor.exp.out <- function(x){
  x <- x %>% mutate(A = as.numeric(targetex), Y = as.numeric(targetslp))
  return(x)
}

## Super Learning
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
  expY.givenAW <- predict(SL.outcome, newdata=X)$pred
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
  colnames(predictions) <- c('givenAW', 'given1W', 'given0W')
  list(estimates.rd=estimates, predictions=predictions, H.AW=H.AW)
}


##### Read in and format datasets
## Read in dataset
unimputed.00.comp <- read_csv("unimputed.00.comp.csv")
unimputed.incomp <- unimputed.00.comp %>% 
  format.slpex.data() %>% 
  factor.exp.out() %>% 
  select(age, raceeth, educ, marital, bmi, waist, depressed, A, Y) %>% 
  na.omit
unimputedwts <- unimputed.00.comp %>% 
  format.slpex.data() %>% 
  factor.exp.out() %>% 
  select(Y, A, age, raceeth, educ, marital, bmi, waist, depressed, WTMEC2YR) %>% 
  na.omit %>% 
  select(WTMEC2YR)
unimputedwts <- unimputedwts$WTMEC2YR
imputed.07.comp <- read_csv("imputed.07.comp.csv") 
imputed7incomp <- imputed.07.comp %>% 
  format.slpex.data() %>% 
  factor.exp.out() %>% 
  select(age, raceeth, educ, marital, bmi, waist, depressed, A, Y)
imputed7wts <- imputed.07.comp %>% 
  format.slpex.data() %>% 
  factor.exp.out() %>% 
  select(Y, A, age, raceeth, educ, marital, bmi, waist, depressed, WTMEC2YR) %>% 
  na.omit %>% 
  select(WTMEC2YR)
imputed7wts <- imputed7wts$WTMEC2YR
long.imputed.comp <- read_csv("long.imputed.comp.csv") %>% 
  format.slpex.data() %>% 
  factor.exp.out()

### Run TMLE - VERY Long (load objects)
## Unimputed
# unimp_slpex_run_tmle <- run.tmle(unimputed.incomp, SL.library)
# save(unimp_slpex_run_tmle, file = "unimp_slpex_run_tmle.RData")
load("unimp_slpex_run_tmle.RData")
unimp_est_slpex_run_tmle <- unimp_slpex_run_tmle$estimates
unimp_est_slpex_run_tmle
# save(unimp_est_slpex_run_tmle, file = "unimp_est_slpex_run_tmle.RData")
unimp_est_slpex_run_tmle_gt <- unimp_est_slpex_run_tmle %>% gt()
# save(unimp_est_slpex_run_tmle_gt, file = "unimp_est_run_tmle_gt.RData")
pander(unimp_est_slpex_run_tmle)

## Imputed #7
# imp7_slpex_run_tmle <- run.tmle(imputed7incomp, SL.library)
# save(imp7_slpex_run_tmle, file = "imp7_slpex_run_tmle.RData")
load("imp7_slpex_run_tmle.RData")
imp7_est_slpex_run_tmle <- imp7_slpex_run_tmle$estimates
imp7_est_slpex_run_tmle
# save(imp7_est_slpex_run_tmle, file = "imp7_est_slpex_run_tmle.RData")
imp7_est_slpex_run_tmle_gt <- imp7_est_slpex_run_tmle %>% gt()
# save(imp7_est_slpex_run_tmle_gt, file = "imp7_est_run_tmle_gt.RData")
pander(imp7_est_slpex_run_tmle)


#############
### ltmle
ltmle.unimp_incomp.SL <- ltmle(data=unimputed.incomp, Anodes='A', Ynodes='Y', abar=list(1, 0), 
                  SL.library=SL.library, estimate.time = T, observation.weights = unimputedwts)
save(ltmle.unimp_incomp.SL, file = "ltmle.unimp_incomp.SL.RData")
summary(ltmle.unimp_incomp.SL)

# Lnodes <- c("CD4_1", "CD4_2")
# Ynodes <- grep("^Y", names(sampleDataForLtmleMSM$data))
# msm.weights <- matrix(1:12, nrow=4, ncol=3) #just an example (can also use a 200x3x4 array),
# #or NULL (for no weights), or "empirical" (the default)
# result2 <- ltmleMSM(sampleDataForLtmleMSM$data, Anodes=Anodes, Lnodes=Lnodes, Ynodes=Ynodes,
#                     survivalOutcome=TRUE,
#                     regimes=sampleDataForLtmleMSM$regimes,
#                     summary.measures=sampleDataForLtmleMSM$summary.measures, final.Ynodes=Ynodes,
#                     working.msm="Y ~ male + time + I(pmax(time - switch.time, 0))",
#                     msm.weights=msm.weights, estimate.time=FALSE)
# print(summary(result2))



