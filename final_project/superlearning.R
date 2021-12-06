## Super Learning
# Libraries
library(tidyverse)
library(SuperLearner)
library(ltmle)

setwd("final_project")
getwd()

## Reading in dataset
set.seed(252)
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
summary(ObsData)


## IPTW
# Estimate the exposure mechanism
prob.AW.reg = glm(targetex ~ age + factor(raceeth) + factor(educ) + factor(marital) + 
                    bmi + waist + factor(depressed),
                  family = binomial(link = "logit"), data = imputed)
summary(prob.AW.reg)
# Probability of meeting target exercise guidelines given exposure
prob.1W <- predict(prob.AW.reg, type= "response")
# Probability of not meeting target exercise guidelines given exposure
prob.0W <- 1 - prob.1W
# Distribution of predicted probabilities
summary(prob.1W)
summary(prob.0W)
# Create weights for IPTW
wt1 <- as.numeric(imputed$targetex==1)/prob.1W
wt0 <- as.numeric(imputed$targetex==0)/prob.0W
summary(wt1)
hist(wt1)
hist(wt1[wt1 > 5])
hist(wt1[wt1>8])
hist(wt1[wt1>15])
# There is only one weight ~ 17, two between 10-12,  5 between 8-10
# Point estimate
iptw <- mean(wt1*imputed$targetslp) - mean(wt0*imputed$targetslp)



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
