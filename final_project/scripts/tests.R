## Tests
# Libraries
# library(tidyverse)
# library(nhanesA)
# 
# setwd("final_project")
getwd()

slpexcov1517 <- read_csv("slpexcov1517.csv")
summary(slpexcov1517)

# Check imputed values compared with smaller dataset
eximprev <- merge(slpexcov1517, exer1517, by = 'SEQN')
# PAQ655 - 1 imputation made to 1 day (mean = 0.9856)
mean(eximprev$PAQ655) # mean 1.093618 -> imputation would be to 1 day
# PAD660 - 11 imputations made to 22 minutes (mean = 21.78)
mean(eximprev$PAD660) # mean 28.27611 -> imputation would be to 28 minutes -> correct
# PAQ670 - 6 imputation made to 1 day (mean = 1.445)
mean(eximprev$PAQ670) # mean 1.430643 -> imputation would be to 1 day
# PAD675 - 23 imputations made to 26 minutes (mean = 26)
mean(eximprev$PAD675) # mean 30.86181 -> imputation would be to 31 minutes -> correct

# Logistic regression tests
# Binary model - rd: 0.06152721, or: 1.44550399
a <- 1.12938
b.1 <- 0.36846
or <- exp(b.1)
or
# 1.445507
expab.1 <- exp(a+b.1)
expab.1
# 4.472019
p.1 = exp(a+b.1)/(1+exp(a+b.1))
p.1
# 0.8172521
pab.1 = expab.1/(1+expab.1)
pab.1
# 0.8172521
expab.0 <- exp(a)
expab.0
# 3.093738
p.0 = exp(a)/(1+exp(a))
p.0
# 0.7557245
pab.0 = expab.0/(1+expab.0)
pab.0
# 0.7557245
rd = pab.1-pab.0
rd
# 0.06152764

# Saturated model - rd: 0.055300, or: 1.451755
a <- 1.688443
b.1 <- 0.372773
or <- exp(b.1)
or
# 1.451755
expab.1 <- exp(a+b.1)
expab.1
# 7.855516
p.1 = exp(a+b.1)/(1+exp(a+b.1))
p.1
# 0.887076
pab.1 = expab.1/(1+expab.1)
pab.1
# 0.887076
expab.0 <- exp(a)
expab.0
# 5.411049
p.0 = exp(a)/(1+exp(a))
p.0
# 0.8440193
pab.0 = expab.0/(1+expab.0)
pab.0
# 0.8440193
rd = pab.1-pab.0
rd
# 0.04305675

######

slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, 
                          weights = ~WTMEC2YR, nest = TRUE, data = unimputed.00.comp)
slpex.design.sub <- subset(slpex.design, inAnalysis == 1) # design subset
# General prevalence estimates for age and BMI
mean_age_svy <- svymean(~age, slpex.design.sub, na.rm = TRUE) # mean age
mean_bmi_svy <- svymean(~bmi, slpex.design.sub, na.rm = TRUE) # mean BMI
mean_age_bmi <- c(mean_age_svy, mean_bmi_svy)
# Use survey GLM to calculate odds ratio estimates
glm.slpex.sub <- svyglm(targetslp ~ targetex, family = binomial(), 
                        data = x, design = slpex.design.sub)
sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
# glm_objects <- c(glm.slpex.sub, sum.glm.slpex.sub) #GLM objects
theta_i <- sum.glm.slpex.sub$coefficients[[2]] # Theta (point estimate)
var_win <- sum.glm.slpex.sub$coefficients[[4]]^2 # Variance
final_or_results <- c(coef = theta_i, var = var_win, standerr = sqrt(var_win), OR = exp(theta_i))
# print(exp(cbind(OR = coef(glm.slpex.sub), confint(glm.slpex.sub))))
# Use G-computation to calculate point estimate of risk difference
exp <- unexp <- x
exp$targetex <- 1
unexp$targetex <- 0
# Risk difference
SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
  mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
survey.results <- c(G_comp = SS.rd, final_or_results, mean_age_bmi, 
                    theta_i = theta_i, var_win = var_win)

