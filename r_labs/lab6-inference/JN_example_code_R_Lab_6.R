#--------------------------------------------------
# 1.  Import the data set and assign it to object ObsData; explore
#--------------------------------------------------
library(SuperLearner)
library(ltmle)
library(foreach)
library(doParallel)
ObsData<- read.csv("RLab6.Inference.csv")
n <- nrow(ObsData)
n
set.seed(1)


#--------------------------------------------------
# 2. load the SuperLearner library and specify the package
#--------------------------------------------------
# specify the library
SL.library <- c("SL.glm", "SL.step", "SL.glm.interaction")

#--------------------------------------------------
#3-9: Create a function to handcode TMLE with Super Learner
#--------------------------------------------------
run.tmle <- function(ObsData, SL.library){
  
  #------------------------------------------
  # Simple substitution estimator 
  #------------------------------------------

  # dataframe X with baseline covariates and exposure
  X <- subset(ObsData, select=c(A, W1, W2, W3, W4))
  
  # set the exposure=1 in X1 and the exposure=0 in X0
  X1 <- X0 <- X
  X1$A <- 1 	# exposed ('good guy')
  X0$A <- 0	# unexposed (not a 'good guy')
  
  # Estimate E_0(Y|A,W) with Super Learner
  SL.outcome <- SuperLearner(Y=ObsData$Y, X=X, SL.library=SL.library, 
                             family="binomial")
  
  # get the expected outcome, given the observed exposure and covariates
  expY.givenAW <- predict(SL.outcome, newdata=ObsData)$pred
  # expected outcome, given A=1 and covariates 
  expY.given1W <- predict(SL.outcome, newdata=X1)$pred
  # expected outcome, given A=0 and covariates
  expY.given0W <- predict(SL.outcome, newdata=X0)$pred
  
  # simple substitution estimator would be 
  PsiHat.SS <- mean(expY.given1W - expY.given0W)
  
  #------------------------------------------
  # Inverse probability of txt weighting
  #------------------------------------------
  
  #  Super Learner for the exposure mechanism  P_0(A=1|W)
  SL.exposure <- SuperLearner(Y=ObsData$A, 
                              X=subset(ObsData, select= -c(A,Y)),
                              SL.library=SL.library, family="binomial")
  
  # generate the predicted prob of being exposed, given baseline cov
  probA1.givenW <- SL.exposure$SL.predict
  # generate the predicted prob of not being exposed, given baseline cov
  probA0.givenW <- 1- probA1.givenW
  
  # clever covariate
  H.AW <- as.numeric(ObsData$A==1)/probA1.givenW - as.numeric(ObsData$A==0)/probA0.givenW
  
  # also want to evaluate the clever covariate at A=1 and A=0 for all participants
  H.1W <- 1/probA1.givenW
  H.0W <- -1/probA0.givenW
  
  # IPTW estimate
  PsiHat.IPTW <- mean(H.AW*ObsData$Y)
  
  #------------------------------------------
  # Targeting & TMLE
  #------------------------------------------
  
  # Update the initial estimator of E_0(Y|A,W)
  # run logistic regression of Y on H.AW using the logit of the esimates as offset
  logitUpdate<- glm( ObsData$Y ~ -1 +offset(qlogis(expY.givenAW)) + 
                       H.AW, family='binomial')
  epsilon <- logitUpdate$coef
  
  # obtain the targeted estimates
  expY.givenAW.star<- plogis( qlogis(expY.givenAW)+ epsilon*H.AW )  
  expY.given1W.star<- plogis( qlogis(expY.given1W)+ epsilon*H.1W )	
  expY.given0W.star<- plogis( qlogis(expY.given0W)+ epsilon*H.0W )
  
  # TMLE point estimate
  PsiHat.TMLE<- mean(expY.given1W.star - expY.given0W.star)
  
  #------------------------------------------
  # Return a list withthe point estimates, targeted estimates of E_0(Y|A,W), 
  # and the vector of clever covariates
  #------------------------------------------
  
  estimates <- data.frame(cbind(PsiHat.SS=PsiHat.SS, PsiHat.IPTW, PsiHat.TMLE))
  predictions <- data.frame(cbind(expY.givenAW.star, expY.given1W.star, expY.given0W.star))
  colnames(predictions) <- c('givenAW', 'given1W', 'given0W')
  list(estimates=estimates, predictions=predictions, H.AW=H.AW)
}


out <- run.tmle(ObsData = ObsData, SL.library = SL.library)
est <- out$estimates
est


# Influence-curve based estimate of the confidence interval

# clever covariate
H.AW <- out$H.AW
# targeted predictions
expY.AW.star <- out$predictions[,'givenAW']
expY.1W.star <- out$predictions[,'given1W']
expY.0W.star <- out$predictions[,'given0W']
#  point estimate
PsiHat.TMLE <- est$PsiHat.TMLE

# plug-in
IC <- H.AW*(ObsData$Y - expY.AW.star) + expY.1W.star - expY.0W.star - PsiHat.TMLE
summary(IC)
hist(IC)

# estimate sigma^2 with the variance of the IC divided by n
varHat.IC <- var(IC)/n
varHat.IC
# standard error estimate
se <- sqrt(varHat.IC)
se

# obtain 95% two-sided confidence intervals:
alpha <- 0.05
c(PsiHat.TMLE+qnorm(alpha/2, lower.tail=T)*se,
  PsiHat.TMLE+qnorm(alpha/2, lower.tail=F)*se)

# calculate the pvalue
2* pnorm( abs(PsiHat.TMLE /se), lower.tail=F )


#-----------
# using the ltmle package - simmilar but not exactly the same
#------------
set.seed(1)

out <- ltmle(data=ObsData, Anodes='A', Ynodes='Y', abar=list(1,0),
             SL.library = SL.library, estimate.time=F)
summary(out)
summary(out)$effect.measures$ATE
est
c(PsiHat.TMLE+qnorm(alpha/2, lower.tail=T)*se,
  PsiHat.TMLE+qnorm(alpha/2, lower.tail=F)*se)


#########################################################
####### Let's evaluate the IC-based inference performance
#########################################################

# set the true value to 0
Psi.P0 <- 0
# set the number of observations
n <- 2500
# set the number of iterations
R <- 500
# create the empty vectors
pt.est <- ci.cov <- reject <- rep(NA, R)

source('RLab6_datagen.R')
#  the for loop
# for(r in 1:R){
#   # draw a new sample
#   NewData <- generateData(n=n, effect=F, get.psi.star=F)
#   
#   # run the tmle package
#   out <- ltmle(data=NewData, Anodes='A', Ynodes='Y', abar=list(1,0),
#                SL.library=SL.library, estimate.time=F)
#   out <- summary(out)$effect.measures$ATE
#   pt.est[r]<- out$estimate
#   ci.cov[r]<- out$CI[1]<= Psi.P0 & Psi.P0 <= out$CI[2]
#   reject[r]<- out$pvalue< 0.05
#   
#   # keep track of the iterations completed
#   print(r)
# }


#save(pt.est, ci.cov, reject, file='RLab6_answers.Rdata')

load('RLab6_answers.Rdata')

# create pdf of the histogram of point estimates
dev.off()
hist(pt.est)

# Confidence interval coverage
mean(ci.cov)

# Type I error rate -
mean(reject)

########################################## Compare to bootstrap
################# First: The SLow Way... no parallel processing
# NP-Boot for B=500 bootstrapped samples
###################
# number of bootstrap samples
# B = 500
# data frame for estimates based on the boot strap sample
# estimates <- data.frame(matrix(NA, nrow=B, ncol=3))
# for loop from b=1 to total number of bootstrap samples
# for(b in 1:B){
# 
#   # sample the indices 1 to n with replacement
#   bootIndices<- sample(1:n, replace=T)
#   bootData<- ObsData[bootIndices,]
# 
#   # calling the above function
#   estimates[b,] <- run.tmle(ObsData=bootData, SL.library=SL.library)$estimates
# 
#   # keep track of the iteractions completed
#   print(b)
# }
# colnames(estimates)<-c("SimpSubs", "IPTW", "TMLE")
# save(estimates, file='RLab6_boot.Rdata')
# load('RLab6_boot.Rdata')


# Let's do this using PARALLEL PROCESSING!!!!!!!!!!!!!!!!!!!!
library(foreach)
library(doParallel)
B = 1000
registerDoParallel(cores = detectCores())
estimates <-
  foreach(b = 1:B, .combine = rbind) %dopar% {
  # sample the indices 1 to n with replacement
  bootIndices<- sample(1:n, replace=T)
  bootData <- ObsData[bootIndices,]
  # calling the above function
  output <- run.tmle(ObsData=bootData, SL.library=SL.library)$estimates
  return(output)
}

colnames(estimates)<-c("SimpSubs", "IPTW", "TMLE")
#save(estimates, file='RLab6_boot_par_JN.Rdata')
load('RLab6_boot_par_JN.Rdata')



#---------------------------------
# Explore the bootstrapped point estimates
#---------------------------------
summary(estimates)

dev.off()
par(mfrow=c(3,1))
hist(estimates[,1], main="Histogram of point estimates from the Simple Substitution estimator
over B bootstrapped samples", xlab="Point Estimates")
hist(estimates[,2], main="Histogram of point estimates from IPTW estimator
over B bootstrapped samples",  xlab="Point Estimates")
hist(estimates[,3], main="Histogram of point estimates from TMLE
over B bootstrapped samples",  xlab="Point Estimates")



#---------------------------------
# 95% Confidence intervals assuming a normal dist & via quantiles
#---------------------------------
create.CI <- function(pt, boot, alpha=0.05){
  Zquant <- qnorm(alpha/2, lower.tail=F)
  CI.normal <- c(pt - Zquant*sd(boot), pt + Zquant*sd(boot) )
  CI.quant  <- quantile(boot, prob=c(0.025,0.975) )
  out <- data.frame(rbind(CI.normal, CI.quant))
  colnames(out) <- c('CI.lo', 'CI.hi')
  out
}

# IMPORTANT - POINT OF CONFUSION FOR PAST STUDENTS
# The point estimate 'pt' is from the original dataset

# Simple Subs - note the bias because of misspecified regression? Will it converge fast enough?
est$PsiHat.SS
create.CI(pt=est$PsiHat.SS, boot=estimates[,"SimpSubs"])

# IPTW
est$PsiHat.IPTW
create.CI(pt=est$PsiHat.IPTW, boot=estimates[,"IPTW"])

# TMLE
est$PsiHat.TMLE
create.CI(pt=est$PsiHat.TMLE, boot=estimates[,"TMLE"])

# Compare to IC estimate
c(PsiHat.TMLE+qnorm(alpha/2, lower.tail=T)*se,
  PsiHat.TMLE+qnorm(alpha/2, lower.tail=F)*se)


# While CI looks smaller in SS estimator,
# bias will make the MSE much larger...
# TMLE best.

# If estimators are biased (often the case), theory behind boostratp doesn't hold.
# Just use TMLE!



