library(tidyverse)
library(SuperLearner)
library(ltmle)
library(MASS)

set.seed(252)

ObsData <- read.csv("RLab5.TMLE.csv")
names(ObsData)
head(ObsData)
summary(ObsData)
nrow(ObsData)

SL.library<- c("SL.mean", "SL.glm", "SL.step.interaction")
X <- subset(ObsData, select = -Y)
X1 <- X0 <- X
X1$A <- 1         # under exposure
X0$A <- 0

# Outcome regression
SL.outcome <- SuperLearner(Y = ObsData$Y, X = X,
                           SL.library=SL.library, family="binomial")
SL.outcome

expY.givenAW <- predict(SL.outcome, newdata=X)$pred
expY.given1W <- predict(SL.outcome, newdata=X1)$pred
expY.given0W <- predict(SL.outcome, newdata=X0)$pred
head(expY.given1W)
head(expY.given0W)

# Propensity score
SL.exposure <- SuperLearner(Y=ObsData$A,
                            X = X %>% dplyr::select(-A),
                            SL.library = SL.library, family="binomial")

probA1.givenW <- SL.exposure$SL.predict
probA0.givenW <- 1 - SL.exposure$SL.predict
summary(data.frame(probA1.givenW, probA0.givenW))

H.AW <- as.numeric(ObsData$A==1)/probA1.givenW - as.numeric(ObsData$A==0)/probA0.givenW

H.1W <-  1/probA1.givenW
H.0W <- -1/probA0.givenW
tail(data.frame(ObsData$A, H.AW, H.1W, H.0W))

logitUpdate<- glm(ObsData$Y ~ -1 + offset(qlogis(expY.givenAW)) + H.AW,
                  family='binomial')
summary(logitUpdate)
epsilon <- logitUpdate$coef
epsilon

expY.givenAW.star <- plogis(qlogis(expY.givenAW) + epsilon*H.AW)
expY.given1W.star <- plogis(qlogis(expY.given1W) + epsilon*H.1W)
expY.given0W.star <- plogis(qlogis(expY.given0W) + epsilon*H.0W)

PsiHat.TMLE <- mean(expY.given1W.star - expY.given0W.star)
PsiHat.TMLE
#################### can evaluate under known DGP

# Getting inference (From Lecture 11)
IC_hat <- H.AW * (ObsData$Y - expY.givenAW.star) + expY.given1W.star - expY.given0W.star - PsiHat.TMLE
head(sort(IC_hat))
hist(IC_hat)
SE <- sqrt(var(IC_hat) / nrow(ObsData))
SE
PsiHat.TMLE + 1.96*(SE)
PsiHat.TMLE - 1.96*(SE)

######################################################################################
ObsData<- read.csv("RLab5.TMLE.csv")
set.seed(252)
# we can learn a lot more about the function by reading the help file
?ltmle

ltmle.SL <- ltmle(data = ObsData,
                  Anodes = 'A', Ynodes = 'Y', abar = list(1,0),
                  SL.library = SL.library,
                  SL.cvControl = list(V = 5))
summary(ltmle.SL)
PsiHat.TMLE
SE
##################### Note: No easy way to get the IC estimates out of "standard"
#####################       LTMLE. Need to do arm-specific estimation to get that.
#####################       Example waaay at the bottom of this R script.


########## You can play with code like this to explore double robustness property
ltmle.parametric <- ltmle(data = ObsData, Anodes='A', Ynodes='Y', abar=list(1,0),
                          Qform = c(Y="Q.kplus1 ~ A + W4"),
                          gform = "A ~ W2 + W1")
summary(ltmle.parametric)


# adding a dummy variable to observed data
ObsData_unadj <- data.frame(U = 1, ObsData)
ltmle.unadj <- ltmle(data = ObsData_unadj, Anodes='A', Ynodes='Y', abar=list(1,0),
                     Qform = c(Y="Q.kplus1 ~ A"), gform = "A ~ U")
summary(ltmle.unadj)

########################### DANGER! You are hoping it will just use U, but instead uses all variables.
ltmle_BAD <- ltmle(data = ObsData_unadj,
                   Anodes='A', Ynodes='Y', abar=list(1,0))
summary(ltmle_BAD)

# BIG IDEA: ONLY INCLUDE W, A, Y in data frame for LTMLE




##################################################################
#  Binary outcomes: get RR, OR, and RD. yay!
ObsData<- read.csv("RLab5.TMLE.csv")
n <- nrow(ObsData)
Ybin <- rbinom(n = n, size = 1, prob = ObsData$Y)

ObsData.bin <- ObsData %>% mutate(Y = Ybin)

ltmle.SL.bin <- ltmle(data = ObsData.bin,
                 Anodes='A', Ynodes='Y', abar=list(1,0),
                 SL.library = SL.library,
                 SL.cvControl = list(V = 5))
summary(ltmle.SL.bin)







############################## WHAT ABOUT RATIOS FOR NON-BINARY OUTCOMES??????
# Example: incidence rate ratios...
# Or, in general, any treatment-specific means.
# Need the ICs and EstarY values for each arm. See below.

# We already have Super Learner-based estimates of the 
# conditional mean outcome and the propensity score

# calculate 2-dimensional clever covariate
H.1W <- as.numeric(ObsData$A==1)/probA1.givenW 
H.0W <- as.numeric(ObsData$A==0)/probA0.givenW

# target 
logitUpdate <- glm(ObsData$Y~ -1 + offset(qlogis(expY.givenAW)) + 
                    H.0W + H.1W, family="binomial")
eps <- logitUpdate$coef
eps

# obtain the targeted estimates
expY.givenAW.star <- plogis(qlogis(expY.givenAW) + 
                              eps['H.0W']*H.0W + eps['H.1W']*H.1W)	
expY.given1W.star <- plogis(qlogis(expY.given1W) + 
                               eps['H.1W']/probA1.givenW )
expY.given0W.star <- plogis(qlogis(expY.given0W) + 
                               eps['H.0W']/probA0.givenW )
TMLE2 <- data.frame(cbind(
  psi1 = mean(expY.given1W.star),
  psi0 = mean(expY.given0W.star),
  diff = mean(expY.given1W.star) - mean(expY.given0W.star),
  ratio = mean(expY.given1W.star) / mean(expY.given0W.star)
))

TMLE2
# comparison to before
summary(ltmle.SL)
PsiHat.TMLE
########## However, this did not give us infeence.


######################### This is probably easier, using LTMLE...
##################################################################
# Treatment-specific means
txt <- ltmle(data = ObsData,
              Anodes='A', Ynodes='Y', abar = 1,
              SL.library=SL.library,
              SL.cvControl = list(V = 5))
con <- ltmle(data = ObsData,
              Anodes='A', Ynodes='Y', abar = 0,
              SL.library=SL.library,
              SL.cvControl = list(V = 5))
summary(txt)
summary(con)
# Why would we do this? To get influence curve estimates for each arm

psi.1 <- txt$estimates["tmle"]
psi.0 <- con$estimates["tmle"]
IC.1 <- txt$IC$tmle
IC.0 <- con$IC$tmle

get.CI <- function(psi.hat, IC, do.relative = T, start){
  J <- length(IC)
  var.IC <- var(IC) / J
  # cutoff based on t-dist for testing and CI	
  cutoff <- qt(0.05 / 2, df = (J - 2), lower.tail = F)
  # standard error (square root of the variance)
  se <- sqrt(var.IC)
  # test statistic (if goal=aRR then on the transformed scale)
  tstat <- psi.hat / se
  pval <- 2*pt(abs(tstat), df = (J - 2), lower.tail = F)
  # 95% confidence interval
  CI.lo <- (psi.hat - cutoff*se)
  CI.hi <- (psi.hat + cutoff*se)
  psi.hat<- exp(psi.hat)
  CI.lo <- exp(CI.lo)
  CI.hi <- exp(CI.hi)
  out <- data.frame(pt = psi.hat, CI.lo, CI.hi, se, pval)
  colnames(out) <- paste(start, colnames(out), sep=".")
  out
}

# going after RR, then get IC estimate on log scale
#	i.e. Delta method for log(aRR) = log(R1) - log(R0)
IC.ratio <- 1/psi.1*IC.1 - 1/psi.0*IC.0
txt <- get.CI(psi.hat = psi.1, IC = IC.1, start = "txt")
con <- get.CI(psi.hat = psi.0, IC = IC.0, start = "con")
RR <- get.CI(psi.hat = log(psi.1 / psi.0), IC = IC.ratio, do.relative = T, start = "RR")
# Does it match? Yes. 
RR
TMLE2
summary(ltmle.SL)

# Note asymmetry
RR$RR.CI.hi - RR$RR.pt
RR$RR.CI.lo - RR$RR.pt



########################### Example with clustered data
ncl <- 10
n <- 1000
X <- round(rnorm(n = n, sd = 4))
C <- rep(round(rnorm(ncl, sd = 4)), each = n / ncl)
id <- rep(1:ncl, each = n / ncl)
A <- rep(0:1, each = n/2)
Y <- round(A + X + C + rnorm(n))
ind <- data.frame(X, C, id, A, Y)

################## no abar
Imod <- ltmle(data = ind %>% dplyr::select(X, C, A, Y),
              Anodes = "A", Ynodes = "Y", abar = 1,
              Qform = c(Y = "Q.kplus1 ~ X + C + A"),
              gform = "A ~ 1", #id = ind$id, ### No accounting for clustering in this one!!!
              variance.method = "tmle")
Cmod <- ltmle(data = ind %>% dplyr::select(X, C, A, Y),
              Anodes = "A", Ynodes = "Y", abar = 1,
              Qform = c(Y = "Q.kplus1 ~ X + C + A"),
              gform = "A ~ 1",
              id = ind$id, #########################################
              variance.method = "tmle")
summary(Imod)
summary(Cmod)



















