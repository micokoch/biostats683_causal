##### Survey GLM

# Libraries
library(tidyverse)
library(survey)

# Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

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
      SDMVSTRA = as.integer(SDMVSTRA), 
      WTINT4YR = as.numeric(WTINT4YR),
      WTMEC4YR = as.numeric(WTMEC4YR),
      combmvu = as.factor(combmvu),
      normwts = as.numeric(normwts)
    )
  return(x)
}

# Function to make exposure and outcome as factors
factor.exp.out <- function(x){
  x <- x %>% mutate(targetex = as.factor(targetex), targetslp = as.factor(targetslp))
  return(x)
}

### Survey Design GLMs
## Binary model
# Binary logistic survey function for single imputation
bin.logit.survey.single <- function(x){
  temp2 <- x %>% filter(inAnalysis == 1)
  # Based on: https://stackoverflow.com/questions/64474714/run-svymean-on-all-variables
  slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, 
                            weights = ~WTMEC4YR, nest = TRUE, data = x)
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
  exp <- unexp <- temp2
  exp$targetex <- 1
  unexp$targetex <- 0
  # Risk difference
  SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
    mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
  survey.results <- c(G_comp = SS.rd, final_or_results, mean_age_bmi, 
                      theta_i = theta_i, var_win = var_win)
  return(survey.results)
}

# Binary logistic survey function for multiple imputation
bin.logit.survey.mi <- function(x){
  # Initialize lists to be used in for loop
  mean_age_bmi <- c()
  glm_objects <- c()
  thetas <- c()
  var_win <- c()
  g_comp_ss <- c()
  for(i in 1:12){ 
    temp <- x %>% filter(.imp == i) # Subset for every imputation
    temp2 <- temp %>% filter(inAnalysis == 1)
    # Use survey design package
    slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = temp)
    slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
    # General prevalence estimates for age and BMI
    mean_age_sub <- svymean(~age, slpex.design.sub, na.rm = TRUE) # mean age subset
    mean_bmi_sub <- svymean(~bmi, slpex.design.sub, na.rm = TRUE) # mean BMI subset
    mean_age_bmi <- append(mean_age_bmi, c(mean_age_sub, mean_bmi_sub)) # Append mean age and bmi to list
    # Use survey GLM to calculate odds ratio estimates
    glm.slpex.sub <- svyglm(targetslp ~ targetex, family = binomial(), # GLM subset
                            data = temp, design = slpex.design.sub)
    sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
    # glm_objects <- append(glm_objects, c(i, glm.slpex.sub, sum.glm.slpex.sub)) #GLM objects
    thetas <- append(thetas, c(theta_i = sum.glm.slpex.sub$coefficients[[2]])) # Append thetas (point estimates)
    var_win <- append(var_win, c(var_win_i = sum.glm.slpex.sub$coefficients[[4]]^2)) # Append within variances
    # Use G-computation to calculate point estimate of risk difference
    exp <- unexp <- temp2
    exp$targetex <- 1
    unexp$targetex <- 0
    # Risk difference
    SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
      mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
    g_comp_ss <- append(g_comp_ss, c(G_comp_i = SS.rd))
    # print(exp(cbind(OR = coef(glm.slpex.sub), confint(glm.slpex.sub))))
  }
  # Determine pooled variance and point estimates for OR
  theta_i_avg <- mean(thetas)
  var_btwn <- (thetas - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  final_or_results <- c(coef = theta_i_avg, var = var_total, standerr = sqrt(var_total), 
                        OR = exp(theta_i_avg))
  # Determine pooled point estimate for g-computation
  g_comp_avg <- mean(g_comp_ss)
  survey.results <- c(G_comp = g_comp_avg, final_or_results, mean_age = mean(mean_age_bmi[1]), 
                      mean_bmi = mean(mean_age_bmi[2]), mean_thetas = mean(thetas), mean_var_win = mean(var_win))
  return(survey.results)
}

## Full model
# Full logistic survey function for single imputation
full.logit.survey.single <- function(x){
  temp2 <- x %>% filter(inAnalysis == 1)
  # Based on: https://stackoverflow.com/questions/64474714/run-svymean-on-all-variables
  slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, 
                            weights = ~WTMEC4YR, nest = TRUE, data = x)
  slpex.design.sub <- subset(slpex.design, inAnalysis == 1) # design subset
  # General prevalence estimates for age and BMI
  mean_age_svy <- svymean(~age, slpex.design.sub, na.rm = TRUE) # mean age
  mean_bmi_svy <- svymean(~bmi, slpex.design.sub, na.rm = TRUE) # mean BMI
  mean_age_bmi <- c(mean_age_svy, mean_bmi_svy)
  # Use survey GLM to calculate odds ratio estimates
  glm.slpex.sub <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                            factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                            waist + factor(smoke) + factor(alcohol) + factor(depressed), 
                          family = binomial(), data = x, design = slpex.design.sub)
  sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
  # glm_objects <- c(glm.slpex.sub, sum.glm.slpex.sub) #GLM objects
  theta_i <- sum.glm.slpex.sub$coefficients[[2]] # Theta (point estimate)
  var_win <- sum.glm.slpex.sub$coefficients[[4]]^2 # Variance
  final_or_results <- c(coef = theta_i, var = var_win, standerr = sqrt(var_win), OR = exp(theta_i))
  # print(exp(cbind(OR = coef(glm.slpex.sub), confint(glm.slpex.sub))))
  # Use G-computation to calculate point estimate of risk difference
  exp <- unexp <- temp2
  exp$targetex <- 1
  unexp$targetex <- 0
  # Risk difference
  SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
    mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
  survey.results <- c(G_comp = SS.rd, final_or_results, mean_age_bmi, 
                      theta_i = theta_i, var_win = var_win)
  return(survey.results)
}
# Full logistic survey function for multiple imputation
full.logit.survey.mi <- function(x){
  # Initialize lists to be used in for loop
  mean_age_bmi <- c()
  glm_objects <- c()
  thetas <- c()
  var_win <- c()
  g_comp_ss <- c()
  for(i in 1:12){ 
    temp <- x %>% filter(.imp == i) # Subset for every imputation
    temp2 <- temp %>% filter(inAnalysis == 1)
    # Use survey design package
    slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = temp)
    slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
    # General prevalence estimates for age and BMI
    mean_age_sub <- svymean(~age, slpex.design.sub, na.rm = TRUE) # mean age subset
    mean_bmi_sub <- svymean(~bmi, slpex.design.sub, na.rm = TRUE) # mean BMI subset
    mean_age_bmi <- append(mean_age_bmi, c(mean_age_sub, mean_bmi_sub)) # Append mean age and bmi to list
    # Use survey GLM to calculate odds ratio estimates
    glm.slpex.sub <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                              factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                              waist + factor(smoke) + factor(alcohol) + factor(depressed), 
                            family = binomial(), data = temp, design = slpex.design.sub)
    sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
    # glm_objects <- append(glm_objects, c(i, glm.slpex.sub, sum.glm.slpex.sub)) #GLM objects
    thetas <- append(thetas, c(theta_i = sum.glm.slpex.sub$coefficients[[2]])) # Append thetas (point estimates)
    var_win <- append(var_win, c(var_win_i = sum.glm.slpex.sub$coefficients[[4]]^2)) # Append within variances
    # Use G-computation to calculate point estimate of risk difference
    exp <- unexp <- temp2
    exp$targetex <- 1
    unexp$targetex <- 0
    # Risk difference
    SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
      mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
    g_comp_ss <- append(g_comp_ss, c(G_comp_i = SS.rd))
    # print(exp(cbind(OR = coef(glm.slpex.sub), confint(glm.slpex.sub))))
  }
  # Determine pooled variance and point estimates for OR
  theta_i_avg <- mean(thetas)
  var_btwn <- (thetas - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  final_or_results <- c(coef = theta_i_avg, var = var_total, standerr = sqrt(var_total), 
                        OR = exp(theta_i_avg))
  # Determine pooled point estimate for g-computation
  g_comp_avg <- mean(g_comp_ss)
  survey.results <- c(G_comp = g_comp_avg, final_or_results, mean_age = mean(mean_age_bmi[1]), 
                      mean_bmi = mean(mean_age_bmi[2]), mean_thetas = mean(thetas), mean_var_win = mean(var_win))
  return(survey.results)
}

## Final model
# Final logistic survey function for single imputation
final.logit.survey.single <- function(x){
  temp2 <- x %>% filter(inAnalysis == 1)
  # Based on: https://stackoverflow.com/questions/64474714/run-svymean-on-all-variables
  slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, 
                            weights = ~WTMEC4YR, nest = TRUE, data = x)
  slpex.design.sub <- subset(slpex.design, inAnalysis == 1) # design subset
  # General prevalence estimates for age and BMI
  mean_age_svy <- svymean(~age, slpex.design.sub, na.rm = TRUE) # mean age
  mean_bmi_svy <- svymean(~bmi, slpex.design.sub, na.rm = TRUE) # mean BMI
  mean_age_bmi <- c(mean_age_svy, mean_bmi_svy)
  # Use survey GLM to calculate odds ratio estimates
  glm.slpex.sub <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                            bmi + waist + factor(depressed), 
                          family = binomial(), data = x, design = slpex.design.sub)
  sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
  # glm_objects <- c(glm.slpex.sub, sum.glm.slpex.sub) #GLM objects
  theta_i <- sum.glm.slpex.sub$coefficients[[2]] # Theta (point estimate)
  var_win <- sum.glm.slpex.sub$coefficients[[4]]^2 # Variance
  final_or_results <- c(coef = theta_i, var = var_win, standerr = sqrt(var_win), OR = exp(theta_i))
  # print(exp(cbind(OR = coef(glm.slpex.sub), confint(glm.slpex.sub))))
  # Use G-computation to calculate point estimate of risk difference
  exp <- unexp <- temp2
  exp$targetex <- 1
  unexp$targetex <- 0
  # Risk difference
  SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
    mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
  survey.results <- c(G_comp = SS.rd, final_or_results, mean_age_bmi, 
                      theta_i = theta_i, var_win = var_win)
  return(survey.results)
}
# Final logistic survey function for multiple imputation
final.logit.survey.mi <- function(x){
  # Initialize lists to be used in for loop
  mean_age_bmi <- c()
  glm_objects <- c()
  thetas <- c()
  var_win <- c()
  g_comp_ss <- c()
  for(i in 1:12){ 
    temp <- x %>% filter(.imp == i) # Subset for every imputation
    temp2 <- temp %>% filter(inAnalysis == 1)
    # Use survey design package
    slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = temp)
    slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
    # General prevalence estimates for age and BMI
    mean_age_sub <- svymean(~age, slpex.design.sub, na.rm = TRUE) # mean age subset
    mean_bmi_sub <- svymean(~bmi, slpex.design.sub, na.rm = TRUE) # mean BMI subset
    mean_age_bmi <- append(mean_age_bmi, c(mean_age_sub, mean_bmi_sub)) # Append mean age and bmi to list
    # Use survey GLM to calculate odds ratio estimates
    glm.slpex.sub <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                              bmi + waist + factor(depressed), 
                            family = binomial(), data = temp, design = slpex.design.sub)
    sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
    # glm_objects <- append(glm_objects, c(i, glm.slpex.sub, sum.glm.slpex.sub)) #GLM objects
    thetas <- append(thetas, c(theta_i = sum.glm.slpex.sub$coefficients[[2]])) # Append thetas (point estimates)
    var_win <- append(var_win, c(var_win_i = sum.glm.slpex.sub$coefficients[[4]]^2)) # Append within variances
    # Use G-computation to calculate point estimate of risk difference
    exp <- unexp <- temp2
    exp$targetex <- 1
    unexp$targetex <- 0
    # Risk difference
    SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
      mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
    g_comp_ss <- append(g_comp_ss, c(G_comp_i = SS.rd))
    # print(exp(cbind(OR = coef(glm.slpex.sub), confint(glm.slpex.sub))))
  }
  # Determine pooled variance and point estimates for OR
  theta_i_avg <- mean(thetas)
  var_btwn <- (thetas - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  final_or_results <- c(coef = theta_i_avg, var = var_total, standerr = sqrt(var_total), 
                        OR = exp(theta_i_avg))
  # Determine pooled point estimate for g-computation
  g_comp_avg <- mean(g_comp_ss)
  survey.results <- c(G_comp = g_comp_avg, final_or_results, mean_age = mean(mean_age_bmi[1]), 
                      mean_bmi = mean(mean_age_bmi[2]), mean_thetas = mean(thetas), mean_var_win = mean(var_win))
  return(survey.results)
}


##### Read in and format datasets
## Read in datasets
unimputed.00.comp <- read_csv("unimputed.00.comp.csv") %>% format.slpex.data()
# imputed.01.comp <- read_csv("imputed.01.comp.csv") %>% format.slpex.data()
# imputed.02.comp <- read_csv("imputed.02.comp.csv") %>% format.slpex.data()
# imputed.03.comp <- read_csv("imputed.03.comp.csv") %>% format.slpex.data()
# imputed.04.comp <- read_csv("imputed.04.comp.csv") %>% format.slpex.data()
# imputed.05.comp <- read_csv("imputed.05.comp.csv") %>% format.slpex.data()
# imputed.06.comp <- read_csv("imputed.06.comp.csv") %>% format.slpex.data()
# imputed.07.comp <- read_csv("imputed.07.comp.csv") %>% format.slpex.data()
# imputed.08.comp <- read_csv("imputed.08.comp.csv") %>% format.slpex.data()
# imputed.09.comp <- read_csv("imputed.09.comp.csv") %>% format.slpex.data()
# imputed.10.comp <- read_csv("imputed.10.comp.csv") %>% format.slpex.data()
# imputed.11.comp <- read_csv("imputed.11.comp.csv") %>% format.slpex.data()
# imputed.12.comp <- read_csv("imputed.12.comp.csv") %>% format.slpex.data()
long.imputed.comp <- read_csv("long.imputed.comp.csv") %>% format.slpex.data()


### Survey Design with weights
## Binary model
# Unimputed subset
bin.unimp.results <- bin.logit.survey.single(unimputed.00.comp)
bin.unimp.results
# Imputed dataset
bin.mi.results <- bin.logit.survey.mi(long.imputed.comp)
bin.mi.results

## Full model
# Unimputed subset
full.unimp.results <- full.logit.survey.single(unimputed.00.comp)
full.unimp.results
# Imputed dataset
full.mi.results <- full.logit.survey.mi(long.imputed.comp)
full.mi.results

## Final model
# Unimputed subset
final.unimp.results <- final.logit.survey.single(unimputed.00.comp)
final.unimp.results
# Imputed dataset
final.mi.results <- final.logit.survey.mi(long.imputed.comp)
final.mi.results

########
### Can I bootstrap full.mi.results for B=200 bootstrapped samples
## Full logistic survey functions for multiple imputation
# Run survey GLM once
run.full.survey <- function(y){
  g_comp_ss <- c()
  temp2 <- y %>% filter(inAnalysis == 1)
  # Use survey design package
  slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = y)
  slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
  # Use survey GLM
  glm.slpex.sub <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                            factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                            waist + factor(smoke) + factor(alcohol) + factor(depressed), 
                          family = binomial(), data = temp, design = slpex.design.sub)
  sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
  # Use G-computation to calculate point estimate of risk difference
  exp <- unexp <- temp2
  exp$targetex <- 1
  unexp$targetex <- 0
  # Risk difference
  SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
    mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
  return(SS.rd)
}

# Create Confidence Intervals
create.CI <- function(pt, boot, alpha=0.05){
  Zquant <- qnorm(alpha/2, lower.tail=F)
  CI.normal <- c(pt - Zquant*sd(boot), pt + Zquant*sd(boot))
  CI.quant <- quantile(boot, prob=c(0.025,0.975) )
  out <- data.frame(rbind(CI.normal, CI.quant))*100
  colnames(out) <- c('CI.lo', 'CI.hi')
  out
  }

# Full logistic survey multiple imputation and bootstratp function
full.logit.survey.mi.boot <- function(x){
  # Initialize lists to be used in for loop
  thetas <- c()
  var_win <- c()
  g_comp_ss <- c()
  var_bs <- c()
  B = 200
  bootstimates <- data.frame(matrix(NA, nrow=B, ncol=12))
  for(i in 1:12){ 
    temp <- x %>% filter(.imp == i) # Subset for every imputation
    temp2 <- temp %>% filter(inAnalysis == 1)
    # Use survey design package
    slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = temp)
    slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
    # Use survey GLM to calculate odds ratio estimates
    glm.slpex.sub <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                              factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                              waist + factor(smoke) + factor(alcohol) + factor(depressed), 
                            family = binomial(), data = temp, design = slpex.design.sub)
    sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
    # Use G-computation to calculate point estimate of risk difference
    exp <- unexp <- temp2
    exp$targetex <- 1
    unexp$targetex <- 0
    # Risk difference
    SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
      mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
    g_comp_ss <- append(g_comp_ss, SS.rd)
    # print(exp(cbind(OR = coef(glm.slpex.sub), confint(glm.slpex.sub))))
    # Bootstrap G-computation
    n <- nrow(temp2)
    # data frame for estimates based on the boot strap sample
    estimates <- rep(NA, B)
    # for loop from b=1 to total number of bootstrap samples
    for(b in 1:B){
      # sample the indices 1 to n with replacement
      bootIndices <- sample(1:n, replace=T)
      bootData <- temp2[bootIndices,]
      # calling the above function
      estimates[b] <- run.full.survey(bootData)
    }
    bootstimates[,i] <- estimates # Append boot estimates
    conf_int_i <- create.CI(SS.rd, estimates, alpha=0.05)
    # print(conf_int_i)
    var.boot_i <- var(estimates)
    var_bs <- append(var_bs, var.boot_i)
  }
  survey.results <- tibble(bootstimates)
  save(survey.results, file='bootstimates.Rdata')
  save(var_bs, file = 'var_bs.Rdata')
  # Determine pooled point estimate for g-computation
  g_comp_avg <- mean(g_comp_ss)
  var_btwn <- (g_comp_ss - g_comp_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_bs)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  #Print CIs
  Zquant.final <- qnorm(0.05/2, lower.tail=F)
  CI.normal.final <- c(g_comp_avg - Zquant.final*(sqrt(var_total)), g_comp_avg + Zquant.final*(sqrt(var_total)))
  # Print final results
  final_or_results <- c(G_comp = g_comp_avg, var = var_total, standerr = sqrt(var_total), CI = CI.normal.final)
  print(final_or_results)
  return(survey.results)
}

full.mi.results.boot <- full.logit.survey.mi.boot(long.imputed.comp)
summary(full.mi.results.boot)


##### Look at depression and effect estimate
# Run survey GLM once
run.full.survey.2 <- function(y){
  y <- y %>% mutate(depressed = as.numeric(depressed), targetex = as.factor(targetex))
  g_comp_ss <- c()
  temp2 <- y %>% filter(inAnalysis == 1)
  # Use survey design package
  slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = y)
  slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
  # Use survey GLM
  glm.slpex.sub <- svyglm(targetslp ~ depressed + factor(targetex) + age + factor(raceeth) + factor(educ) + factor(marital) + 
                            factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                            waist + factor(smoke) + factor(alcohol), 
                          family = binomial(), data = temp, design = slpex.design.sub)
  sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
  # Use G-computation to calculate point estimate of risk difference
  exp <- unexp <- temp2
  exp$depressed <- 1
  unexp$depressed <- 0
  # Risk difference
  SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
    mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
  return(SS.rd)
}

# Create Confidence Intervals
create.CI.2 <- function(pt, boot, alpha=0.05){
  Zquant <- qnorm(alpha/2, lower.tail=F)
  CI.normal <- c(pt - Zquant*sd(boot), pt + Zquant*sd(boot))
  CI.quant <- quantile(boot, prob=c(0.025,0.975) )
  out <- data.frame(rbind(CI.normal, CI.quant))*100
  colnames(out) <- c('CI.lo', 'CI.hi')
  out
}

# Full logistic survey multiple imputation and bootstratp function
full.logit.survey.mi.boot.2 <- function(x){
  x <- x %>% mutate(depressed = as.numeric(depressed), targetex = as.factor(targetex))
  # Initialize lists to be used in for loop
  thetas <- c()
  var_win <- c()
  g_comp_ss <- c()
  var_bs <- c()
  B = 200
  bootstimates <- data.frame(matrix(NA, nrow=B, ncol=12))
  for(i in 1:12){ 
    temp <- x %>% filter(.imp == i) # Subset for every imputation
    temp2 <- temp %>% filter(inAnalysis == 1)
    # Use survey design package
    slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = temp)
    slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
    # Use survey GLM to calculate odds ratio estimates
    glm.slpex.sub <- svyglm(targetslp ~ depressed + factor(targetex) + age + factor(raceeth) + factor(educ) + factor(marital) + 
                              factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                              waist + factor(smoke) + factor(alcohol), 
                            family = binomial(), data = temp, design = slpex.design.sub)
    sum.glm.slpex.sub <- summary(glm.slpex.sub) # GLM summary
    # Use G-computation to calculate point estimate of risk difference
    exp <- unexp <- temp2
    exp$depressed <- 1
    unexp$depressed <- 0
    # Risk difference
    SS.rd <- mean(predict(glm.slpex.sub, newdata=exp, type='response')) - 
      mean(predict(glm.slpex.sub, newdata=unexp, type='response'))
    g_comp_ss <- append(g_comp_ss, SS.rd)
    # print(exp(cbind(OR = coef(glm.slpex.sub), confint(glm.slpex.sub))))
    # Bootstrap G-computation
    n <- nrow(temp2)
    # data frame for estimates based on the boot strap sample
    estimates <- rep(NA, B)
    # for loop from b=1 to total number of bootstrap samples
    for(b in 1:B){
      # sample the indices 1 to n with replacement
      bootIndices <- sample(1:n, replace=T)
      bootData <- temp2[bootIndices,]
      # calling the above function
      estimates[b] <- run.full.survey.2(bootData)
    }
    bootstimates[,i] <- estimates # Append boot estimates
    conf_int_i <- create.CI.2(SS.rd, estimates, alpha=0.05)
    # print(conf_int_i)
    var.boot_i <- var(estimates)
    var_bs <- append(var_bs, var.boot_i)
  }
  survey.results <- tibble(bootstimates)
  save(survey.results, file='bootstimates2.Rdata')
  save(var_bs, file = 'var_bs2.Rdata')
  # Determine pooled point estimate for g-computation
  g_comp_avg <- mean(g_comp_ss)
  var_btwn <- (g_comp_ss - g_comp_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_bs)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  #Print CIs
  Zquant.final <- qnorm(0.05/2, lower.tail=F)
  CI.normal.final <- c(g_comp_avg - Zquant.final*(sqrt(var_total)), g_comp_avg + Zquant.final*(sqrt(var_total)))
  # Print final results
  final_or_results <- c(G_comp = g_comp_avg, var = var_total, standerr = sqrt(var_total), CI = CI.normal.final)
  print(final_or_results)
  return(survey.results)
}

full.mi.results.boot.2 <- full.logit.survey.mi.boot.2(long.imputed.comp)
summary(full.mi.results.boot.2)


