## Exercise and sleep and covariates
# Libraries
# library(tidyverse)
# library(nhanesA)
library(riskCommunicator)
library(pander)
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
#        0  544 1683
#        1  286 1279

slpexcov1517 %>% 
  dplyr::select(targetex, targetslp) %>% 
  group_by(targetex, targetslp) %>% 
  summarise(n = n())
# # A tibble: 4 × 3
# # Groups:   targetex [2]
#   targetex targetslp     n
#       <dbl>     <dbl> <int>
# 1        0         0   544
# 2        0         1  1683
# 3        1         0   286
# 4        1         1  1279

# Fit a logistic model to the data without confounders and look at results
glm.slpex = glm(targetslp ~ targetex, family = binomial(link = "logit"), data = slpexcov1517)
summary(glm.slpex)
# Coefficients:
#              Estimate  Std. Error z value Pr(>|z|)    
# (Intercept)   1.12938    0.04932  22.899  < 2e-16 ***
#  targetex     0.36846    0.08192   4.498 6.87e-06 ***
table(slpexcov1517$targetex)
# 0    1 
# 2227 1565 
# Odds ratio - conditional effect
exp(cbind(OR = coef(glm.slpex), confint(glm.slpex)))
#                   OR    2.5 %   97.5 %
# (Intercept) 3.093750 2.810878 3.410527
# targetex    1.445504 1.232041 1.698740

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
# 0.8172524 0.7557252
risk_diff.bin <- p1.bin - p0.bin
risk_diff.bin
# 0.06152721
# Compare with odds ratio
odds_ratio.bin <- exp(coef(glm.slpex)["targetex"])
odds_ratio.bin
# 1.445504
# Check it matches values obtained calculating OR with probabilities
(p1.bin/(1-p1.bin))/(p0.bin/(1-p0.bin))
# 1.445504
ee.bin <- c(rd = risk_diff.bin, or = odds_ratio.bin)
ee.bin
# rd          or.targetex
# 0.06152721  1.44550399
# Try Risk Communicator results
slpex.results.bin <- gComp(data = slpexcov1517.factor, Y = "targetslp", X = "targetex", 
                       outcome.type = "binary", R = 200)
slpex.results.bin
# Parameter estimates: 
# targetex1_v._targetex0 Estimate (95% CI)
# Risk Difference                                 0.062 (0.034, 0.089) - results match
# Risk Ratio                                      1.081 (1.045, 1.121)
# Odds Ratio                                      1.446 (1.224, 1.706) - results match
# Number needed to treat/harm                                   16.253

# We get the same results for odds ratio, and risk difference
# plot(slpex.results.bin)

# Fit data with saturated model and see how effects change
glm.slpexcov = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                     factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                     waist + factor(smoke) + factor(alcohol) + factor(depressed),
                   family = binomial(link = "logit"), data = slpexcov1517)
summary(glm.slpexcov)
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.688443   0.453792   3.721 0.000199 ***
# targetex            0.372773   0.105710   3.526 0.000421 ***
# age                -0.007625   0.004471  -1.705 0.088118 .  
# factor(raceeth)2   -0.163198   0.139485  -1.170 0.241998    
# factor(raceeth)3   -0.778182   0.133716  -5.820  5.9e-09 ***
# factor(raceeth)4   -0.250682   0.152796  -1.641 0.100873    
# factor(educ)2      -0.256649   0.143162  -1.793 0.073018 .  
# factor(educ)3      -0.060024   0.146147  -0.411 0.681286    
# factor(educ)4       0.470659   0.176235   2.671 0.007571 ** 
# factor(marital)2    0.033478   0.121958   0.275 0.783695    
# factor(household)2 -0.109821   0.196421  -0.559 0.576087    
# factor(household)3 -0.095248   0.207804  -0.458 0.646696    
# factor(household)4 -0.263762   0.213943  -1.233 0.217627    
# factor(household)5 -0.366306   0.223922  -1.636 0.101868    
# factor(household)6 -0.249541   0.229755  -1.086 0.277428    
# factor(income)2     0.024217   0.115202   0.210 0.833499    
# factor(income)3    -0.202953   0.150150  -1.352 0.176482    
# factor(snoring)2    0.063241   0.149085   0.424 0.671426    
# factor(snoring)3   -0.106954   0.155668  -0.687 0.492040    
# factor(snoring)4   -0.177607   0.147607  -1.203 0.228883    
# factor(apnea)1     -0.063786   0.108132  -0.590 0.555262    
# bmi                -0.054857   0.024035  -2.282 0.022464 *  
# waist               0.019071   0.009613   1.984 0.047261 *  
# factor(smoke)1     -0.002851   0.102511  -0.028 0.977812    
# factor(alcohol)1    0.109962   0.130380   0.843 0.399007    
# factor(alcohol)2    0.008028   0.131784   0.061 0.951428    
# factor(depressed)1 -0.537253   0.178907  -3.003 0.002674 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

exp(cbind(OR = coef(glm.slpexcov), confint(glm.slpexcov)))
#                          OR     2.5 %     97.5 %
# (Intercept)        5.4110489 2.2290591 13.2134288
# targetex           1.4517548 1.1812176  1.7880032
# age                0.9924043 0.9837344  1.0011336
# factor(raceeth)2   0.8494228 0.6462309  1.1168344
# factor(raceeth)3   0.4592402 0.3530287  0.5964448
# factor(raceeth)4   0.7782697 0.5773499  1.0514048
# factor(educ)2      0.7736399 0.5836186  1.0232825
# factor(educ)3      0.9417421 0.7063489  1.2530444
# factor(educ)4      1.6010486 1.1344335  2.2646448
# factor(marital)2   1.0340451 0.8129502  1.3115780
# factor(household)2 0.8959945 0.6071855  1.3125144
# factor(household)3 0.9091472 0.6029635  1.3628389
# factor(household)4 0.7681564 0.5033569  1.1653424
# factor(household)5 0.6932909 0.4457925  1.0732542
# factor(household)6 0.7791585 0.4956179  1.2208539
# factor(income)2    1.0245129 0.8162507  1.2824760
# factor(income)3    0.8163170 0.6080652  1.0957938
# factor(snoring)2   1.0652832 0.7948516  1.4265405
# factor(snoring)3   0.8985669 0.6620063  1.2191784
# factor(snoring)4   0.8372717 0.6259125  1.1167359
# factor(apnea)1     0.9382055 0.7596079  1.1607789
# bmi                0.9466202 0.9030462  0.9923266
# waist              1.0192541 1.0002701  1.0386996
# factor(smoke)1     0.9971530 0.8156805  1.2192572
# factor(alcohol)1   1.1162354 0.8633103  1.4396635
# factor(alcohol)2   1.0080598 0.7773772  1.3034999
# factor(depressed)1 0.5843512 0.4132842  0.8343249

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
# Risk Difference                                 0.055 (0.025, 0.091)
# Risk Ratio                                      1.070 (1.031, 1.118)
# Odds Ratio                                      1.452 (1.196, 1.902)
# Number needed to treat/harm                                   18.076

# We get the same results for odds ratio, and we get a risk difference of 0.055
# plot(slpex.results.sat)
# Obtain risk difference value
risk_diff.sat <- as.numeric(slpex.results.sat$results.df[1,4])
risk_diff.sat
# 0.0553
# Obtain odds ratio from logistic regression
odds_ratio.sat <- exp(coef(glm.slpexcov)["targetex"])
odds_ratio.sat
# 1.451755
ee.sat <- c(rd = risk_diff.sat, or = odds_ratio.sat)
ee.sat
# rd          or.targetex
# 0.055300    1.451755
# Smaller risk difference, but larger odds ratio (vs. ee.bin)

# Removing covariates that have many NAs or seem unimportant, try parsimonious model
glm.slpexcov2 = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                      bmi + waist + factor(depressed),
                   family = binomial(link = "logit"), data = slpexcov1517)
summary(glm.slpexcov2)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.367665   0.346292   3.949 7.83e-05 ***
# targetex            0.328647   0.093760   3.505 0.000456 ***
# age                -0.008757   0.003680  -2.379 0.017341 *  
# factor(raceeth)2   -0.155546   0.122161  -1.273 0.202915    
# factor(raceeth)3   -0.704612   0.118080  -5.967 2.41e-09 ***
# factor(raceeth)4   -0.208653   0.136504  -1.529 0.126376    
# factor(educ)2      -0.142141   0.124364  -1.143 0.253060    
# factor(educ)3      -0.049859   0.124047  -0.402 0.687730    
# factor(educ)4       0.447969   0.144199   3.107 0.001892 ** 
# factor(marital)2   -0.015536   0.092465  -0.168 0.866570    
# bmi                -0.081716   0.021169  -3.860 0.000113 ***
# waist               0.027740   0.008479   3.272 0.001069 ** 
# factor(depressed)1 -0.475600   0.151744  -3.134 0.001723 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
exp(cbind(OR = coef(glm.slpexcov2), confint(glm.slpexcov2)))
#                          OR     2.5 %    97.5 %
# (Intercept)        3.9261707 1.9934057 7.7501641
# targetex           1.3890867 1.1567584 1.6707580
# age                0.9912809 0.9841461 0.9984517
# factor(raceeth)2   0.8559473 0.6735931 1.0875914
# factor(raceeth)3   0.4943005 0.3918898 0.6226879
# factor(raceeth)4   0.8116768 0.6217147 1.0620424
# factor(educ)2      0.8674986 0.6793976 1.1064715
# factor(educ)3      0.9513636 0.7454649 1.2125541
# factor(educ)4      1.5651299 1.1807391 2.0786851
# factor(marital)2   0.9845844 0.8207332 1.1794098
# bmi                0.9215338 0.8840135 0.9605409
# waist              1.0281279 1.0112292 1.0454153
# factor(depressed)1 0.6215118 0.4631588 0.8402080

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
# Risk Difference                                 0.051 (0.025, 0.076)
# Risk Ratio                                      1.065 (1.032, 1.100)
# Odds Ratio                                      1.389 (1.177, 1.661)
# Number needed to treat/harm                                   19.616

# We get the same results for odds ratio, and we get a risk difference of 0.051
# plot(slpex.results.pars)
# Obtain risk difference value
risk_diff.pars <- as.numeric(slpex.results.pars$results.df[1,4])
risk_diff.pars
# 0.051
# Obtain odds ratio from logistic regression
odds_ratio.pars <- exp(coef(glm.slpexcov2)["targetex"])
odds_ratio.pars
# 1.389087
ee.pars <- c(rd = risk_diff.pars, or = odds_ratio.pars)
ee.pars
# rd          or.targetex
# 0.051000    1.389087
# Smaller risk difference, but larger odds ratio (vs. ee.bin)

# Compare results for binary, saturated, and parsimonious models
ee_all <- bind_rows(binary = ee.bin, saturated = ee.sat, parsimonious = ee.pars, .id = "id")
ee_all
save(ee_all, file = "ee_all.RData")
ee_all_gt <- ee_all %>% gt()
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
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.367665   0.346292   3.949 7.83e-05 ***
# targetex            0.328647   0.093760   3.505 0.000456 ***
# age                -0.008757   0.003680  -2.379 0.017341 *  
# factor(raceeth)2   -0.155546   0.122161  -1.273 0.202915    
# factor(raceeth)3   -0.704612   0.118080  -5.967 2.41e-09 ***
# factor(raceeth)4   -0.208653   0.136504  -1.529 0.126376    
# factor(educ)2      -0.142141   0.124364  -1.143 0.253060    
# factor(educ)3      -0.049859   0.124047  -0.402 0.687730    
# factor(educ)4       0.447969   0.144199   3.107 0.001892 ** 
# factor(marital)2   -0.015536   0.092465  -0.168 0.866570    
# bmi                -0.081716   0.021169  -3.860 0.000113 ***
# waist               0.027740   0.008479   3.272 0.001069 ** 
# factor(depressed)1 -0.475600   0.151744  -3.134 0.001723 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
exp(cbind(OR = coef(glm.slpexcov3), confint(glm.slpexcov3)))
#                          OR     2.5 %    97.5 %
# (Intercept)        3.9261707 1.9934057 7.7501641
# targetex           1.3890867 1.1567584 1.6707580
# age                0.9912809 0.9841461 0.9984517
# factor(raceeth)2   0.8559473 0.6735931 1.0875914
# factor(raceeth)3   0.4943005 0.3918898 0.6226879
# factor(raceeth)4   0.8116768 0.6217147 1.0620424
# factor(educ)2      0.8674986 0.6793976 1.1064715
# factor(educ)3      0.9513636 0.7454649 1.2125541
# factor(educ)4      1.5651299 1.1807391 2.0786851
# factor(marital)2   0.9845844 0.8207332 1.1794098
# bmi                0.9215338 0.8840135 0.9605409
# waist              1.0281279 1.0112292 1.0454153
# factor(depressed)1 0.6215118 0.4631588 0.8402080

# ATE (risk difference - marginal effect)
# Probability for each individual based on the model fit under targetex = 1
prob.1.naom <- slpexcov1517.naom %>% mutate(targetex=1)
table(prob.1.naom$targetex)
#    1 
# 3406
# Probability for each individual based on the model fit under targetex = 1
p1.parsna <- mean(predict(glm.slpexcov3, prob.1.naom, type = "response"))
# Probability for each individual based on the model fit under targetex = 0
prob.0.naom <- slpexcov1517.naom %>% mutate(targetex=0)
table(prob.0.naom$targetex)
#    0
# 3406
# Probability for each individual based on the model fit under targetex = 0
p0.parsna <- mean(predict(glm.slpexcov3, prob.0.naom, type = "response"))
# Marginal probabilities
c(p1.parsna, p0.parsna)
# 0.8102022 0.7565222
risk_diff.parsna <- p1.parsna - p0.parsna
risk_diff.parsna
# 0.05367993
# Compare with odds ratio
odds_ratio.parsna <- exp(coef(glm.slpexcov3)["targetex"])
odds_ratio.parsna
# 1.389087
# Check it matches values obtained calculating OR with probabilities
(p1.parsna/(1-p1.parsna))/(p0.parsna/(1-p0.parsna))
# 1.373851 - it doesn't match because I'm dropping NAs???
c(rd = risk_diff.parsna, or = odds_ratio.parsna)
# rd          or.targetex
# 0.05367993  1.38908674
# rd result is not trustworthy

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
# Risk Difference                                 0.051 (0.021, 0.077)
# Risk Ratio                                      1.065 (1.027, 1.100)
# Odds Ratio                                      1.389 (1.153, 1.661)
# Number needed to treat/harm                                   19.616

# We get the same results for odds ratio, and we get a risk difference of 0.051
# plot(slpex.results.pars)
# Obtain risk difference value
risk_diff.pars.naom <- as.numeric(slpex.results.pars.naom$results.df[1,4])
risk_diff.pars.naom
# 0.051
# Obtain odds ratio from logistic regression
odds_ratio.pars.naom <- exp(coef(glm.slpexcov3)["targetex"])
odds_ratio.pars.naom
# 1.389087
ee.pars.naom <- c(rd = risk_diff.pars.naom, or = odds_ratio.pars.naom)
ee.pars.naom
# rd          or.targetex
# 0.051000    1.389087
# Smaller risk difference, but larger odds ratio (vs. ee.bin)

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
# Smallest cell: ex: 1, slp: 0, raceeth: 4, count: 48

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
# Smallest cell: ex: 1, slp: 0, educ: 1, count: 43

# Marital
mrg <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, marital) %>% 
  group_by(targetex, targetslp, marital) %>% 
  summarise(n = n())
mrg
# One cell has NA for marital (ex: 1, slp: 0, marital: NA)
# Smallest cell: ex: 1, slp: 0, marital: 1, count: 115

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
# Smallest cells: ex: 1, slp: 0, household: (1|6), count: 30

table(slpexcov1517[,c('marital', 'household', 'targetex')])

# Income
plata <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, income) %>% 
  group_by(targetex, targetslp, income) %>% 
  summarise(n = n())
plata
# Various cells have NAs for income (00NA-43, 01NA-126, 10NA-27, 11NA-109)
# Smallest cell: ex: 1, slp: 0, income: 3, count: 64

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
# Various cells have NAs for depressed (00NA-39, 01NA-144, 10NA-20, 11NA-98)
# Smallest cells: ex: 1, slp: 0, depressed: 1, count: 16

table(slpexcov1517[,c('depressed', 'income', 'targetex')])

# BMI Categories
peso <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, bmicat) %>% 
  group_by(targetex, targetslp, bmicat) %>% 
  summarise(n = n())
peso
# Various cells have NAs for depressed (00NA-7, 01NA-29, 10NA-1, 11NA-13)
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
