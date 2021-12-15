## Exercise and sleep and covariates
# Libraries
# library(tidyverse)
# library(nhanesA)
library(riskCommunicator)
library(pander)
library(gt)
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
# str(slpexcov1517)
# str(slpexcov1517.factor)

# Binary table
binslpexcov1517 <- dplyr::select(slpexcov1517, SEQN, targetex, targetslp)
summary(binslpexcov1517)

#Contingency tables
binslpexcov1517 %>% select(-SEQN) %>% table()
#          targetslp
# targetex    0    1
#        0  701 1526
#        1  429 1136

slpexcov1517 %>% 
  dplyr::select(targetex, targetslp) %>% 
  group_by(targetex, targetslp) %>% 
  summarise(n = n())
# # A tibble: 4 × 3
# # Groups:   targetex [2]
#   targetex targetslp     n
#       <dbl>     <dbl> <int>
# 1        0         0   701
# 2        0         1  1526
# 3        1         0   429
# 4        1         1  1136

# Fit a logistic model to the data without confounders and look at results
glm.slpex = glm(targetslp ~ targetex, family = binomial(link = "logit"), data = slpexcov1517)
summary(glm.slpex)
# Coefficients:
#              Estimate  Std. Error z value Pr(>|z|)    
# (Intercept)   0.77790    0.04563  17.049  < 2e-16 ***
#  targetex     0.19591    0.07275   2.693  0.00708 ** 
table(slpexcov1517$targetex)
# 0    1 
# 2227 1565 
# Odds ratio - conditional effect
exp(cbind(OR = coef(glm.slpex), confint(glm.slpex)))
#                   OR    2.5 %   97.5 %
# (Intercept) 2.176890 1.991621 2.381749
# targetex    1.216423 1.055130 1.403409

# ATE (risk difference - marginal effect)
# Probability for each individual based on the model fit under targetex = 1
prob.1 <- slpexcov1517 %>% mutate(targetex=1)
table(prob.1$targetex)
#    1 
# 3792
p1.bin <- mean(predict(glm.slpex, prob.1, type = "response"))
# Probability for each individual based on the model fit under targetex = 0
prob.0 <- slpexcov1517 %>% mutate(targetex=0)
table(prob.0$targetex)
#    0
# 3792
p0.bin <- mean(predict(glm.slpex, prob.0, type = "response"))
# Marginal probabilities
c(p1.bin, p0.bin)
# 0.7258786 0.6852268
risk_diff.bin <- p1.bin - p0.bin
risk_diff.bin
# 0.04065183
# Compare with odds ratio
odds_ratio.bin <- exp(coef(glm.slpex)["targetex"])
odds_ratio.bin
# 1.216423 
# Check it matches values obtained calculating OR with probabilities
(p1.bin/(1-p1.bin))/(p0.bin/(1-p0.bin))
# 1.216423 
ee.bin <- c(rd = risk_diff.bin, or = odds_ratio.bin)
ee.bin
# rd          or.targetex
# 0.04065183  1.21642272 
# Try Risk Communicator results
slpex.results.bin <- gComp(data = slpexcov1517.factor, Y = "targetslp", X = "targetex", 
                       outcome.type = "binary", R = 200)
slpex.results.bin
# Parameter estimates: 
# targetex1_v._targetex0 Estimate (95% CI)
# Risk Difference                                 0.041 (0.012, 0.070) - results match
# Risk Ratio                                      1.059 (1.017, 1.102)
# Odds Ratio                                      1.216 (1.055, 1.410) - results match
# Number needed to treat/harm                                   24.599

# We get the same results for odds ratio, and risk difference
# plot(slpex.results.bin)

# Fit data with saturated model and see how effects change
glm.slpexcov = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                     factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                     waist + factor(smoke) + factor(alcohol) + factor(depressed),
                   family = binomial(link = "logit"), data = slpexcov1517)
summary(glm.slpexcov)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.654302   0.409249   4.042 5.29e-05 ***
# targetex            0.173715   0.093090   1.866  0.06203 .  
# age                -0.011480   0.003996  -2.873  0.00407 ** 
# factor(raceeth)2   -0.171067   0.123084  -1.390  0.16458    
# factor(raceeth)3   -0.690181   0.120920  -5.708 1.14e-08 ***
# factor(raceeth)4   -0.159483   0.133615  -1.194  0.23263    
# factor(educ)2      -0.211356   0.133047  -1.589  0.11215    
# factor(educ)3      -0.165304   0.133309  -1.240  0.21497    
# factor(educ)4       0.231800   0.154898   1.496  0.13453    
# factor(marital)2   -0.111130   0.110933  -1.002  0.31645    
# factor(household)2 -0.055578   0.173730  -0.320  0.74903    
# factor(household)3  0.091817   0.184803   0.497  0.61931    
# factor(household)4  0.025191   0.191158   0.132  0.89516    
# factor(household)5 -0.096408   0.201211  -0.479  0.63184    
# factor(household)6 -0.139762   0.205367  -0.681  0.49616    
# factor(income)2     0.032239   0.103884   0.310  0.75631    
# factor(income)3    -0.311603   0.132311  -2.355  0.01852 *  
# factor(snoring)2    0.038468   0.131791   0.292  0.77037    
# factor(snoring)3    0.029436   0.140357   0.210  0.83389    
# factor(snoring)4   -0.160765   0.132261  -1.216  0.22417    
# factor(apnea)1     -0.015970   0.097213  -0.164  0.86951    
# bmi                -0.056780   0.021741  -2.612  0.00901 ** 
# waist               0.018215   0.008663   2.103  0.03550 *  
# factor(smoke)1      0.021548   0.092093   0.234  0.81500    
# factor(alcohol)1    0.002327   0.118221   0.020  0.98430    
# factor(alcohol)2   -0.135476   0.120450  -1.125  0.26069    
# factor(depressed)1 -0.411326   0.169161  -2.432  0.01503 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

exp(cbind(OR = coef(glm.slpexcov), confint(glm.slpexcov)))
# .
#                           OR     2.5 %     97.5 %
# (Intercept)        5.2294268 2.3499217 11.6960837
# targetex           1.1897163 0.9916361  1.4284813
# age                0.9885859 0.9808595  0.9963491
# factor(raceeth)2   0.8427653 0.6621509  1.0729497
# factor(raceeth)3   0.5014855 0.3954433  0.6353502
# factor(raceeth)4   0.8525844 0.6565118  1.1087429
# factor(educ)2      0.8094855 0.6231473  1.0500074
# factor(educ)3      0.8476358 0.6521033  1.0999194
# factor(educ)4      1.2608674 0.9307376  1.7086220
# factor(marital)2   0.8948222 0.7191246  1.1110646
# factor(household)2 0.9459378 0.6714672  1.3274198
# factor(household)3 1.0961637 0.7618745  1.5729599
# factor(household)4 1.0255109 0.7039871  1.4900754
# factor(household)5 0.9080938 0.6114415  1.3462242
# factor(household)6 0.8695649 0.5808317  1.2998421
# factor(income)2    1.0327640 0.8417757  1.2650716
# factor(income)3    0.7322724 0.5647021  0.9487652
# factor(snoring)2   1.0392180 0.8022790  1.3452314
# factor(snoring)3   1.0298732 0.7821519  1.3563016
# factor(snoring)4   0.8514925 0.6564282  1.1026716
# factor(apnea)1     0.9841568 0.8138587  1.1914985
# bmi                0.9448016 0.9053236  0.9859131
# waist              1.0183817 1.0012699  1.0358699
# factor(smoke)1     1.0217823 0.8531053  1.2241143
# factor(alcohol)1   1.0023296 0.7941871  1.2626173
# factor(alcohol)2   0.8732997 0.6888405  1.1047418
# factor(depressed)1 0.6627711 0.4769405  0.9265821

# ATE (risk difference - marginal effect)
# Old method didn't work (predicted probabilities yielded many NAs) - didn't drop

# # Probability for each individual based on the model fit under targetex = 1
# p1.sat <- mean(predict(glm.slpexcov, prob.1, type = "response"), na.rm = T)
# # Probability for each individual based on the model fit under targetex = 0
# p0.sat <- mean(predict(glm.slpexcov, prob.0, type = "response"), na.rm = T)
# # Marginal probabilities
# c(p1.sat, p0.sat)
# # 0.8184718 0.7591212
# risk_diff.sat <- p1.sat - p0.sat
# risk_diff.sat
# # 0.05935064
# # Compare with odds ratio
# odds_ratio.sat <- exp(coef(glm.slpexcov)["targetex"])
# odds_ratio.sat
# # 1.451755
# # Check it matches values obtained calculating OR with probabilities
# (p1.sat/(1-p1.sat))/(p0.sat/(1-p0.sat))
# # 1.430695 - it doesn't match because I'm dropping NAs
# c(rd = risk_diff.sat, or = odds_ratio.sat)
# # rd          or.targetex
# # 0.05935064  1.45175476
# # rd result is not trustworthy

# Try Risk Communicator results
slpex.results.sat <- gComp(data = slpexcov1517.factor, Y = "targetslp", X = "targetex", 
                       Z = c("age", "raceeth", "educ", "marital", "household", 
                             "income", "snoring", "apnea", "bmi", "waist", "smoke", 
                             "alcohol", "depressed"), outcome.type = "binary", R = 200)
slpex.results.sat
# Parameter estimates: 
# targetex1_v._targetex0 Estimate (95% CI)
# Risk Difference                                 0.034 (-0.001, 0.070)
# Risk Ratio                                      1.048 (0.999, 1.100)
# Odds Ratio                                      1.190 (0.995, 1.435) - results match
# Number needed to treat/harm                                   29.209

# We get the same results for odds ratio, and we get a risk difference of 0.034
# plot(slpex.results.sat)
# Obtain risk difference value
risk_diff.sat <- as.numeric(slpex.results.sat$results.df[1,4])
risk_diff.sat
# 0.0342
# Obtain odds ratio from logistic regression
odds_ratio.sat <- exp(coef(glm.slpexcov)["targetex"])
odds_ratio.sat
# 1.189716
ee.sat <- c(rd = risk_diff.sat, or = odds_ratio.sat)
ee.sat
# rd          or.targetex
# 0.034200    1.189716 
# Smaller risk difference and odds ratio (vs. ee.bin)

# Removing covariates that have many NAs or seem unimportant, try parsimonious model
glm.slpexcov2 = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                      bmi + waist + factor(depressed),
                   family = binomial(link = "logit"), data = slpexcov1517)
summary(glm.slpexcov2)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.315873   0.313524   4.197 2.70e-05 ***
# targetex            0.182592   0.083184   2.195 0.028160 *  
# age                -0.012754   0.003320  -3.841 0.000122 ***
# factor(raceeth)2   -0.140134   0.108068  -1.297 0.194727    
# factor(raceeth)3   -0.566427   0.106896  -5.299 1.17e-07 ***
# factor(raceeth)4   -0.041243   0.119899  -0.344 0.730861    
# factor(educ)2      -0.100651   0.115589  -0.871 0.383879    
# factor(educ)3      -0.169604   0.113384  -1.496 0.134696    
# factor(educ)4       0.130828   0.126185   1.037 0.299833    
# factor(marital)2   -0.098238   0.083789  -1.172 0.241018    
# bmi                -0.078305   0.019256  -4.067 4.77e-05 ***
# waist               0.025921   0.007678   3.376 0.000735 ***
# factor(depressed)1 -0.355131   0.143630  -2.473 0.013416 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
exp(cbind(OR = coef(glm.slpexcov2), confint(glm.slpexcov2)))
#                           OR     2.5 %    97.5 %
# (Intercept)        3.7280055 2.0183603 6.9008349
# targetex           1.2003241 1.0200665 1.4134239
# age                0.9873273 0.9809112 0.9937639
# factor(raceeth)2   0.8692418 0.7033072 1.0744310
# factor(raceeth)3   0.5675499 0.4601119 0.6996738
# factor(raceeth)4   0.9595959 0.7591623 1.2149179
# factor(educ)2      0.9042482 0.7206463 1.1338853
# factor(educ)3      0.8439994 0.6753863 1.0535153
# factor(educ)4      1.1397718 0.8899796 1.4597504
# factor(marital)2   0.9064335 0.7687258 1.0676969
# bmi                0.9246825 0.8903482 0.9601873
# waist              1.0262595 1.0109692 1.0418691
# factor(depressed)1 0.7010817 0.5301309 0.9314875

# ATE (risk difference - marginal effect)
# Old method didn't work (predicted probabilities yielded many NAs)

# # Probability for each individual based on the model fit under targetex = 1
# p1.pars <- mean(predict(glm.slpexcov2, prob.1, type = "response"), na.rm = T)
# # Probability for each individual based on the model fit under targetex = 0
# p0.pars <- mean(predict(glm.slpexcov2, prob.0, type = "response"), na.rm = T)
# # Marginal probabilities
# c(p1.pars, p0.pars)
# # 0.8102022 0.7565222
# risk_diff.pars <- p1.pars - p0.pars
# risk_diff.pars
# # 0.05367993
# # Compare with odds ratio
# odds_ratio.pars <- exp(coef(glm.slpexcov2)["targetex"])
# odds_ratio.pars
# # 1.389087
# # Check it matches values obtained calculating OR with probabilities
# (p1.pars/(1-p1.pars))/(p0.pars/(1-p0.pars))
# # 1.373851 - it doesn't match because I'm dropping NAs
# c(rd = risk_diff.pars, or = odds_ratio.pars)
# # rd          or.targetex
# # 0.05367993  1.38908674
# # rd result is not trustworthy

# Try Risk Communicator results
slpex.results.pars <- gComp(data = slpexcov1517.factor, Y = "targetslp", X = "targetex", 
                       Z = c("age", "raceeth", "educ", "marital", "bmi", "waist", 
                             "depressed"), outcome.type = "binary", R = 200)
slpex.results.pars
# Parameter estimates: 
# targetex1_v._targetex0 Estimate (95% CI)
# Risk Difference                                 0.037 (0.010, 0.069)
# Risk Ratio                                      1.052 (1.013, 1.100)
# Odds Ratio                                      1.200 (1.050, 1.412)
# Number needed to treat/harm                                   27.204

# We get the same results for odds ratio, and we get a risk difference of 0.037
# plot(slpex.results.pars)
# Obtain risk difference value
risk_diff.pars <- as.numeric(slpex.results.pars$results.df[1,4])
risk_diff.pars
# 0.0368
# Obtain odds ratio from logistic regression
odds_ratio.pars <- exp(coef(glm.slpexcov2)["targetex"])
odds_ratio.pars
# 1.200324 
ee.pars <- c(rd = risk_diff.pars, or = odds_ratio.pars)
ee.pars
# rd          or.targetex
# 0.036800    1.200324 
# Smaller risk difference and odds ratio vs. ee.bin, but bigger than sat

# Compare results for binary, saturated, and parsimonious models
ee_all <- bind_rows(binary = ee.bin, saturated = ee.sat, parsimonious = ee.pars, .id = "id")
ee_all
save(ee_all, file = "ee_all.RData")
ee_all_gt <- ee_all %>% gt()
ee_all_gt
save(ee_all_gt, file = "ee_all_gt.RData")
pander(ee_all)

#####
slpexcov1517.naom <- slpexcov1517 %>% 
  select(-SEQN, -exminwk, -slphrs, -household, -income, -snoring, -apnea, -bmicat, 
         -smoke, -alcohol, -phq9) %>% 
  na.exclude()
  
# Parsimonious model running with na.omit
glm.slpexcov3 = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                      bmi + waist + factor(depressed),
                    family = binomial(link = "logit"), data = slpexcov1517.naom)
summary(glm.slpexcov3)
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.315873   0.313524   4.197 2.70e-05 ***
# targetex            0.182592   0.083184   2.195 0.028160 *  
# age                -0.012754   0.003320  -3.841 0.000122 ***
# factor(raceeth)2   -0.140134   0.108068  -1.297 0.194727    
# factor(raceeth)3   -0.566427   0.106896  -5.299 1.17e-07 ***
# factor(raceeth)4   -0.041243   0.119899  -0.344 0.730861    
# factor(educ)2      -0.100651   0.115589  -0.871 0.383879    
# factor(educ)3      -0.169604   0.113384  -1.496 0.134696    
# factor(educ)4       0.130828   0.126185   1.037 0.299833    
# factor(marital)2   -0.098238   0.083789  -1.172 0.241018    
# bmi                -0.078305   0.019256  -4.067 4.77e-05 ***
# waist               0.025921   0.007678   3.376 0.000735 ***
# factor(depressed)1 -0.355131   0.143630  -2.473 0.013416 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
exp(cbind(OR = coef(glm.slpexcov3), confint(glm.slpexcov3)))
#                           OR     2.5 %    97.5 %
# (Intercept)        3.7280055 2.0183603 6.9008349
# targetex           1.2003241 1.0200665 1.4134239
# age                0.9873273 0.9809112 0.9937639
# factor(raceeth)2   0.8692418 0.7033072 1.0744310
# factor(raceeth)3   0.5675499 0.4601119 0.6996738
# factor(raceeth)4   0.9595959 0.7591623 1.2149179
# factor(educ)2      0.9042482 0.7206463 1.1338853
# factor(educ)3      0.8439994 0.6753863 1.0535153
# factor(educ)4      1.1397718 0.8899796 1.4597504
# factor(marital)2   0.9064335 0.7687258 1.0676969
# bmi                0.9246825 0.8903482 0.9601873
# waist              1.0262595 1.0109692 1.0418691
# factor(depressed)1 0.7010817 0.5301309 0.9314875

# ATE (risk difference - marginal effect)
# Probability for each individual based on the model fit under targetex = 1
prob.1.naom <- slpexcov1517.naom %>% mutate(targetex=1)
table(prob.1.naom$targetex)
#    1 
# 3416
# Probability for each individual based on the model fit under targetex = 1
p1.parsna <- mean(predict(glm.slpexcov3, prob.1.naom, type = "response"))
# Probability for each individual based on the model fit under targetex = 0
prob.0.naom <- slpexcov1517.naom %>% mutate(targetex=0)
table(prob.0.naom$targetex)
#    0
# 3416
# Probability for each individual based on the model fit under targetex = 0
p0.parsna <- mean(predict(glm.slpexcov3, prob.0.naom, type = "response"))
# Marginal probabilities
c(p1.parsna, p0.parsna)
# 0.7206446 0.6835586
risk_diff.parsna <- p1.parsna - p0.parsna
risk_diff.parsna
# 0.03708604
# Compare with odds ratio
odds_ratio.parsna <- exp(coef(glm.slpexcov3)["targetex"])
odds_ratio.parsna
# 1.200324 
# Check it matches values obtained calculating OR with probabilities
(p1.parsna/(1-p1.parsna))/(p0.parsna/(1-p0.parsna))
# 1.194213 - it nearly matches effect estimate
c(rd = risk_diff.parsna, or = odds_ratio.parsna)
# rd          or.targetex
# 0.03708604  1.20032411 

# Try Risk Communicator results
# First, make new dataset dropping nas
slpexcov1517.factor.naom <- slpexcov1517.factor %>% 
  select(-SEQN, -exminwk, -slphrs, -household, -income, -snoring, -apnea, -bmicat, 
         -smoke, -alcohol, -phq9) %>% 
  na.exclude()

slpex.results.pars.naom <- gComp(data = slpexcov1517.factor.naom, Y = "targetslp", X = "targetex", 
                            Z = c("age", "raceeth", "educ", "marital", "bmi", "waist", 
                                  "depressed"), outcome.type = "binary", R = 200)
slpex.results.pars.naom
# Parameter estimates: 
#                             targetex1_v._targetex0 Estimate (95% CI)
# Risk Difference                                 0.037 (0.006, 0.067)
# Risk Ratio                                      1.052 (1.008, 1.098)
# Odds Ratio                                      1.200 (1.030, 1.404)
# Number needed to treat/harm                                   27.204

# We get almost same results for odds ratio, and we get a risk difference of 0.037
# plot(slpex.results.pars)
# Obtain risk difference value
risk_diff.pars.naom <- as.numeric(slpex.results.pars.naom$results.df[1,4])
risk_diff.pars.naom
# 0.0368
# Obtain odds ratio from logistic regression
odds_ratio.pars.naom <- exp(coef(glm.slpexcov3)["targetex"])
odds_ratio.pars.naom
# 1.200324 
ee.pars.naom <- c(rd = risk_diff.pars.naom, or = odds_ratio.pars.naom)
ee.pars.naom
# rd          or.targetex
# 0.036800    1.200324 
# Smaller risk difference and odds ratio (vs. ee.bin), almost identical to without dropping NAs

# # Use bmicat instead of bmi - full covariates
# glm.slpexcov4 = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
#                       factor(household) + factor(income) + factor(snoring) + factor(apnea) + 
#                       relevel(factor(bmicat), ref = 2) + waist + factor(smoke) + factor(alcohol) + 
#                       factor(depressed),
#                    family = binomial(link = "logit"), data = slpexcov1517)
# summary(glm.slpexcov4)
# exp(cbind(OR = coef(glm.slpexcov4), confint(glm.slpexcov4)))
# 
# # Use bmicat instead of bmi - reduced covariates
# glm.slpexcov5 = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
#                       relevel(factor(bmicat), ref = 2) + factor(depressed),
#                     family = binomial(link = "logit"), data = slpexcov1517)
# summary(glm.slpexcov5)
# exp(cbind(OR = coef(glm.slpexcov5), confint(glm.slpexcov5)))

#####

# Look at plots of fitted models (even though they're not very informative.)
ggplot(data = glm.slpex, mapping = aes(x = targetex, y = targetslp)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

ggplot(data = glm.slpexcov, mapping = aes(x = targetex, y = targetslp)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

ggplot(data = glm.slpexcov2, mapping = aes(x = targetex, y = targetslp)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue")

#####
# Checking for positivity violations
# library(summarytools)
# dfSummary(slpexcov, style = "grid", plain.ascii = TRUE)

# Race/ethnicity
racet <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, raceeth) %>% 
  group_by(targetex, targetslp, raceeth) %>% 
  summarise(n = n())
racet
# Smallest cell: ex: 1, slp: 0, raceeth: 4, count: 75

table(slpexcov1517[,c('raceeth', 'educ', 'targetex')])

# # Years in the US
# usys <- slpexcov %>% 
#   dplyr::select(targetex, targetslp, usyears) %>% 
#   group_by(targetex, targetslp, usyears) %>% 
#   summarise(n = n())
# usys
# # There are a couple of cells with very few values -> tarxetex==1, targetslp=0, usyears=(1|9)
# # Smallest cell: ex: 1, slp: 0, usyears: 1, count: 1
# # Small cell: ex: 1, slp: 0, usyears: 9, count: 2
# ## Shouldn't include this variable in our model

# Education
ed <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, educ) %>% 
  group_by(targetex, targetslp, educ) %>% 
  summarise(n = n())
ed
# One cell has NA for educ (ex: 0, slp: 1, educ: NA)
# Smallest cell: ex: 1, slp: 0, educ: 1, count: 63

# Marital
mrg <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, marital) %>% 
  group_by(targetex, targetslp, marital) %>% 
  summarise(n = n())
mrg
# One cell has NA for marital (ex: 1, slp: 0, marital: NA)
# Smallest cell: ex: 1, slp: 0, marital: 1, count: 161

# # Pregnancy
# slpexcov %>% 
#   dplyr::select(targetex, targetslp, pregnancy) %>% 
#   group_by(targetex, targetslp, pregnancy) %>% 
#   summarise(n = n())
# ## There are empty cells: ex: 1, slp: 0, pregnancy: 1
# # Smallest cell: ex: 0, slp: 0, pregnancy 1, count: 8
# ## Too many NAs, shouldn't include this variable in our model

# Household
home <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, household) %>% 
  group_by(targetex, targetslp, household) %>% 
  summarise(n = n())
home
# Smallest cells: ex: 1, slp: 0, household: 1, count: 47

table(slpexcov1517[,c('marital', 'household', 'targetex')])

# Income
plata <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, income) %>% 
  group_by(targetex, targetslp, income) %>% 
  summarise(n = n())
plata
# Various cells have NAs for income (00NA-54, 01NA-115, 10NA-40, 11NA-96)
# Smallest cell: ex: 1, slp: 0, income: 1, count: 101

### Old income info:
# There are a couple of cells with very few values -> tarxetex==1, targetslp=0, income=(1|2|13)
# Smallest cell: ex: 1, slp: 0, income: 13, count: 5
# Small cell: ex: 1, slp: 0, income: 1, count: 6
# Small cell: ex: 1, slp: 0, income: 2, count: 9

# Depression
depression <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, depressed) %>% 
  group_by(targetex, targetslp, depressed) %>% 
  summarise(n = n())
depression
# Various cells have NAs for depressed (00NA-44, 01NA-132, 10NA-33, 11NA-81)
# Smallest cells: ex: 1, slp: 0, depressed: 1, count: 21

table(slpexcov1517[,c('depressed', 'income', 'targetex')])

# BMI Categories
peso <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, bmicat) %>% 
  group_by(targetex, targetslp, bmicat) %>% 
  summarise(n = n())
peso
# Various cells have NAs for depressed (00NA-7, 01NA-29, 10NA-3, 11NA-11)
# Smallest cells: ex: 1, slp: 0, bmicat: 1, count: 6

table(slpexcov1517[,c('depressed', 'bmicat', 'targetex')])

#####
# slpexcov %>% count(pregnancy)
# slpexcov2 <- slpexcov %>% 
#   subset(pregnancy != 1 | is.na(pregnancy))
# summary(slpexcov2)
# 
# # New csv file with fewer (and fixed) covariates:
# slpexcov2 <- slpexcov2 %>% 
#   dplyr::select(-usyears, -pregnancy)
# summary(slpexcov2)
# head(slpexcov2)
# tail(slpexcov2)
# write_csv(slpexcov2, "slpexcov2.csv")
# 
# binslpex2 <- as_tibble(slpexcov2)
# binslpex2 %>% 
#   dplyr::select(targetex, targetslp) %>% 
#   group_by(targetex, targetslp) %>% 
#   summarise(n = n())

#####
## Once covariates are complete, replace simple imputations for exposure with IPTW
## Use NHANES weights for data
