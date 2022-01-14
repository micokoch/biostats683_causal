####### Sensitivity Analyses

##### Libraries
library(tidyverse)
library(survey)
library(riskCommunicator)

##### Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

##### Functions
## Formatting the dataset appropriately
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

## Function to make exposure and outcome as factors
factor.exp.out <- function(x){
  x <- x %>% mutate(targetex = as.factor(targetex), targetslp = as.factor(targetslp))
  return(x)
}

#### Models
### Binary models
## Binary logistic glm function for multiple imputation
bin.logit.mi.rc <- function(x){
  theta_i <- c()
  var_win <- c()
  for(i in 1:12){
    temp <- x %>% filter(.imp == i)
    temp2 <- temp %>% filter(inAnalysis == 1) %>% 
      mutate(targetex = as.factor(targetex)) %>% 
      na.exclude()
    slpex.results.bin <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                               outcome.type = "binary", R = 200)
    theta_i <- append(theta_i, slpex.results.bin$results.df$Estimate[1])
    std.err.ci <- ((slpex.results.bin[["results.df"]][["97.5% CL"]][1] - 
                      slpex.results.bin[["results.df"]][["2.5% CL"]][1]) / 
                     (qnorm(0.975)*2))
    var_win <- append(var_win, std.err.ci^2)
  }
  theta_i_avg <- mean(theta_i)
  var_btwn <- (theta_i - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  std.err.pool <- sqrt(var_total)
  ci_low = (theta_i_avg - (qnorm(0.975) * std.err.pool))
  ci_high = (theta_i_avg + (qnorm(0.975) * std.err.pool))
  print(c(coef = theta_i_avg, standerr = std.err.pool, ci_low = ci_low, ci_high = ci_high))
}

### Partial models
## Partial logistic glm function for multiple imputation
partial.logit.mi.rc <- function(x){
  theta_i <- c()
  var_win <- c()
  for(i in 1:12){
    temp <- x %>% filter(.imp == i)
    temp2 <- temp %>% filter(inAnalysis == 1) %>% 
      mutate(targetex = as.factor(targetex)) %>% 
      na.exclude()
    slpex.results.partial <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                                 Z = c("age", "raceeth", "educ", "marital", "household", 
                                       "bmi", "waist", "smoke"), outcome.type = "binary", R = 200)
    theta_i <- append(theta_i, slpex.results.partial$results.df$Estimate[1])
    std.err.ci <- ((slpex.results.partial[["results.df"]][["97.5% CL"]][1] - 
                      slpex.results.partial[["results.df"]][["2.5% CL"]][1]) / 
                     (qnorm(0.975)*2))
    var_win <- append(var_win, std.err.ci^2)
  }
  theta_i_avg <- mean(theta_i)
  var_btwn <- (theta_i - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  std.err.pool <- sqrt(var_total)
  ci_low = (theta_i_avg - (qnorm(0.975) * std.err.pool))
  ci_high = (theta_i_avg + (qnorm(0.975) * std.err.pool))
  print(c(coef = theta_i_avg, standerr = std.err.pool, ci_low = ci_low, ci_high = ci_high))
}

### Full models
## Full logistic glm function for multiple imputation
full.logit.mi.rc <- function(x){
  theta_i <- c()
  var_win <- c()
  for(i in 1:12){
    temp <- x %>% filter(.imp == i)
    temp2 <- temp %>% filter(inAnalysis == 1) %>% 
      mutate(targetex = as.factor(targetex)) %>% 
      na.exclude()
    slpex.results.full <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                                Z = c("age", "raceeth", "educ", "marital", "household", 
                                      "bmi", "waist", "smoke", "snoring", "apnea", "income", 
                                      "alcohol", "depressed"), outcome.type = "binary", R = 200)
    theta_i <- append(theta_i, slpex.results.full$results.df$Estimate[1])
    std.err.ci <- ((slpex.results.full[["results.df"]][["97.5% CL"]][1] - 
                      slpex.results.full[["results.df"]][["2.5% CL"]][1]) / 
                     (qnorm(0.975)*2))
    var_win <- append(var_win, std.err.ci^2)
  }
  theta_i_avg <- mean(theta_i)
  var_btwn <- (theta_i - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  std.err.pool <- sqrt(var_total)
  ci_low = (theta_i_avg - (qnorm(0.975) * std.err.pool))
  ci_high = (theta_i_avg + (qnorm(0.975) * std.err.pool))
  print(c(coef = theta_i_avg, standerr = std.err.pool, ci_low = ci_low, ci_high = ci_high))
}

### No snoring or apnea models
## No snoring or apnea logistic glm function for multiple imputation
nosnoap.logit.mi.rc <- function(x){
  theta_i <- c()
  var_win <- c()
  for(i in 1:12){
    temp <- x %>% filter(.imp == i)
    temp2 <- temp %>% filter(inAnalysis == 1) %>% 
      mutate(targetex = as.factor(targetex)) %>% 
      na.exclude()
    slpex.results.full <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                                Z = c("age", "raceeth", "educ", "marital", "household", 
                                      "bmi", "waist", "smoke", "income", 
                                      "alcohol", "depressed"), outcome.type = "binary", R = 200)
    theta_i <- append(theta_i, slpex.results.full$results.df$Estimate[1])
    std.err.ci <- ((slpex.results.full[["results.df"]][["97.5% CL"]][1] - 
                      slpex.results.full[["results.df"]][["2.5% CL"]][1]) / 
                     (qnorm(0.975)*2))
    var_win <- append(var_win, std.err.ci^2)
  }
  theta_i_avg <- mean(theta_i)
  var_btwn <- (theta_i - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  std.err.pool <- sqrt(var_total)
  ci_low = (theta_i_avg - (qnorm(0.975) * std.err.pool))
  ci_high = (theta_i_avg + (qnorm(0.975) * std.err.pool))
  print(c(coef = theta_i_avg, standerr = std.err.pool, ci_low = ci_low, ci_high = ci_high))
}

### Survey models for B=200 bootstrapped samples
## Create Confidence Intervals
create.CI <- function(pt, boot, alpha=0.05){
  Zquant <- qnorm(alpha/2, lower.tail=F)
  CI.normal <- c(pt - Zquant*sd(boot), pt + Zquant*sd(boot))
  CI.quant <- quantile(boot, prob=c(0.025,0.975) )
  out <- data.frame(rbind(CI.normal, CI.quant))*100
  colnames(out) <- c('CI.lo', 'CI.hi')
  out
}

## Binary logistic survey functions for multiple imputation
# Run survey GLM once
run.binary.survey <- function(y){
  g_comp_ss <- c()
  temp2 <- y %>% filter(inAnalysis == 1)
  # Use survey design package
  slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = y)
  slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
  # Use survey GLM
  glm.slpex.sub <- svyglm(targetslp ~ targetex, 
                          family = binomial(), data = y, design = slpex.design.sub)
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

# Binary logistic survey multiple imputation and bootstratp function
bin.logit.survey.mi.boot <- function(x){
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
    glm.slpex.sub <- svyglm(targetslp ~ targetex, 
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
      estimates[b] <- run.binary.survey(bootData)
    }
    bootstimates[,i] <- estimates # Append boot estimates
    conf_int_i <- create.CI(SS.rd, estimates, alpha=0.05)
    # print(conf_int_i)
    var.boot_i <- var(estimates)
    var_bs <- append(var_bs, var.boot_i)
  }
  survey.results <- tibble(bootstimates)
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

## Partial logistic survey functions for multiple imputation
# Run survey GLM once
run.partial.survey <- function(y){
  g_comp_ss <- c()
  temp2 <- y %>% filter(inAnalysis == 1)
  # Use survey design package
  slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = y)
  slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
  # Use survey GLM
  glm.slpex.sub <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                            factor(household) + bmi + waist + factor(smoke), 
                          family = binomial(), data = y, design = slpex.design.sub)
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

# Partial logistic survey multiple imputation and bootstratp function
partial.logit.survey.mi.boot <- function(x){
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
                              factor(household) + bmi + waist + factor(smoke), 
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
      estimates[b] <- run.partial.survey(bootData)
    }
    bootstimates[,i] <- estimates # Append boot estimates
    conf_int_i <- create.CI(SS.rd, estimates, alpha=0.05)
    # print(conf_int_i)
    var.boot_i <- var(estimates)
    var_bs <- append(var_bs, var.boot_i)
  }
  survey.results <- tibble(bootstimates)
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
                            factor(household) + bmi + waist + factor(smoke) + factor(snoring) + factor(apnea) + 
                            factor(income) + factor(alcohol) + factor(depressed), 
                          family = binomial(), data = y, design = slpex.design.sub)
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
                              factor(household) + bmi + waist + factor(smoke) + factor(snoring) + factor(apnea) + 
                              factor(income) + factor(alcohol) + factor(depressed), 
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

## No snoring or apnea logistic survey functions for multiple imputation
# Run survey GLM once
run.nosnoap.survey <- function(y){
  g_comp_ss <- c()
  temp2 <- y %>% filter(inAnalysis == 1)
  # Use survey design package
  slpex.design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC4YR, nest = TRUE, data = y)
  slpex.design.sub <- subset(slpex.design, inAnalysis==1) # design subset
  # Use survey GLM
  glm.slpex.sub <- svyglm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                            factor(household) + bmi + waist + factor(smoke) + 
                            factor(income) + factor(alcohol) + factor(depressed), 
                          family = binomial(), data = y, design = slpex.design.sub)
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

# Full logistic survey multiple imputation and bootstratp function
nosnoap.logit.survey.mi.boot <- function(x){
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
                              factor(household) + bmi + waist + factor(smoke) + 
                              factor(income) + factor(alcohol) + factor(depressed), 
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
      estimates[b] <- run.nosnoap.survey(bootData)
    }
    bootstimates[,i] <- estimates # Append boot estimates
    conf_int_i <- create.CI(SS.rd, estimates, alpha=0.05)
    # print(conf_int_i)
    var.boot_i <- var(estimates)
    var_bs <- append(var_bs, var.boot_i)
  }
  survey.results <- tibble(bootstimates)
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


##### Read in and format datasets
unimputed.00.comp <- read_csv("unimputed.00.comp.csv") %>% format.slpex.data()
# 10,739 obs of 50 variables
long.imputed.comp <- read_csv("long.imputed.comp.csv") %>% format.slpex.data()
# 139,607 obs of 50 variables
long.only_imputed.comp <- read_csv("long.only_imputed.comp.csv") %>% format.slpex.data()
# 128,868 obs of 50 variables

## Check files
names(long.imputed.comp)
str(unimputed.00.comp)
# Looks correct
unimp.reduced <- unimputed.00.comp %>% filter(inAnalysis == 1)
# 3,818 obs of 50 variables
summary(unimp.reduced)
# NAs - 11 in targetex, 25 in targetslp, 208 in snoring, 194 in apnea, 1 in educ, 1 in marital,
# 310 in income, 52 in BMI, 160 in waist, 2 in smoking, 448 in alcohol, 303 in depression
# >10% missing -> alcohol, >5% missing -> snoring, apnea, income, alcohol, depression
long.imp.reduced <- long.imputed.comp %>% filter(inAnalysis == 1)
# 49,634 obs of 50 variables
long.only_imp.reduced <- long.only_imputed.comp %>% filter(inAnalysis == 1)
# 45,816 obs of 50 variables
long.imputed.comp %>% filter(.imp != 0) %>% is.na() %>% sum()
sum(is.na(long.only_imputed.comp))
# No NAs in any of the imputed sets

##### Analyses
std.err.ci <- function(x){
  ((x[["results.df"]][["97.5% CL"]][1] - 
      x[["results.df"]][["2.5% CL"]][1]) /
     (qnorm(0.975)*2))
}

#### Raw analyses
temp2 <- unimp.reduced %>% mutate(targetex = as.factor(targetex))
summary(temp2)
count(temp2)
# Was worried about "inAnalysis, since it's a factor, but result seems correct
### Unadjusted model
unadj.raw <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                   outcome.type = "binary", R = 200)
unadj.raw

unadj.complete.rd.boot <- as.numeric(unadj.raw[["boot.result"]][[1]])
save(unadj.complete.rd.boot, file = "unadj.complete.rd.boot.Rdata")

# ATE = 0.042 (0.012, 0.069)
std.err.ci(unadj.raw)
# Standard error: 0.01449134

### Partial model
partial.raw <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                     Z = c("age", "raceeth", "educ", "marital", "household", "bmi", "waist", "smoke"), 
                     outcome.type = "binary", R = 200)
partial.raw
# ATE = 0.038 (0.007, 0.068)
std.err.ci(partial.raw)
# Standard error: 0.01554302

### Full model
full.raw <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                  Z = c("age", "raceeth", "educ", "marital", "household", "bmi", "waist", "smoke",
                        "snoring", "apnea", "income", "alcohol", "depressed"), 
                  outcome.type = "binary", R = 200)
full.raw
# ATE = 0.036 (-0.005, 0.066)
std.err.ci(full.raw)
# Standard error: 0.01821845

### Nosnoap model
nosnoap.raw <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                  Z = c("age", "raceeth", "educ", "marital", "household", "bmi", "waist", "smoke",
                        "income", "alcohol", "depressed"), 
                  outcome.type = "binary", R = 200)
nosnoap.raw
# ATE = 0.037 (0.007, 0.072)
std.err.ci(nosnoap.raw)
# Standard error: 0.0165915
adj.complete.rd.boot <- as.numeric(nosnoap.raw[["boot.result"]][[1]])
save(adj.complete.rd.boot, file = "adj.complete.rd.boot.Rdata")



#### MI analyses
temp2 <- long.imp.reduced %>% mutate(targetex = as.factor(targetex))
summary(temp2)
count(temp2)
# Was worried about "inAnalysis, since it's a factor, but result seems correct
### Unadjusted model
unadj.mi <- bin.logit.mi.rc(temp2)
unadj.mi
# ATE = 0.039316667 (0.009786873, 0.068846461)
# Round 2:       coef   standerr     ci_low    ci_high 
#           0.03931667 0.01411674 0.01164836 0.06698497 

### Partial model
partial.mi <- partial.logit.mi.rc(temp2)
partial.mi
# ATE = 0.027383333 (-0.002908232, 0.057674898)
# Round 2:       coef   standerr     ci_low    ci_high 
#       0.027383333  0.015455164 -0.002908232  0.057674898 

### Full model
full.mi <- full.logit.mi.rc(temp2)
full.mi
# ATE = 0.026283333 (-0.004724788, 0.057291455)
# Round 2:       coef   standerr     ci_low    ci_high 
#       0.026283333  0.015820761 -0.004724788  0.057291455

### Nosnoap model
nosnoap.mi <- nosnoap.logit.mi.rc(temp2)
nosnoap.mi
# ATE = 0.028258333 (-0.002264851, 0.058781517)
# Round 1: coef     standerr       ci_low      ci_high 
#     0.028258333  0.015573339 -0.002264851  0.058781517 


#### Survey final analyses
temp2 <- long.imputed.comp %>% mutate(targetex = as.numeric(targetex))
summary(temp2)
count(temp2)
temp2 %>% filter(inAnalysis == 1) %>% summary()
# Was worried about "inAnalysis, since it's a factor, but result seems correct
### Unadjusted model
unadj.svy <- bin.logit.survey.mi.boot(temp2)
unadj.svy
# ATE = 0.023183293 (-0.019078096, 0.065444682)
# Round 2: G_comp          var     standerr          CI1          CI2 
#         0.023183293  0.000471812  0.021721234 -0.019389544  0.065756129 

### Partial model
partial.svy <- partial.logit.survey.mi.boot(temp2)
partial.svy
# ATE = 0.0065377122 (-0.0393483531, 0.0524237774)
# Round 2: G_comp          var     standerr          CI1          CI2 
#         0.0065377122  0.0005402283  0.0232428120 -0.0390173622  0.0520927865

### Full model
full.svy <- full.logit.survey.mi.boot(temp2)
full.svy
# ATE = 0.0041991015 (-0.0416974711, 0.0500956741)
# Round 2: G_comp          var     standerr          CI1          CI2 
#         0.0041991015  0.0005636733  0.0237418050 -0.0423339811  0.0507321841

### Nosnoap model
nosnoap.svy <- nosnoap.logit.survey.mi.boot(temp2)
nosnoap.svy
# ATE = 0.0051591883 (-0.0400409803, 0.0503593570)
# Round 1: G_comp           var      standerr           CI1           CI2 
#     0.0051591883  0.0005318436  0.0230617343 -0.0400409803  0.0503593570 

#### Sensitivity analyses
temp2 <- long.imputed.comp %>% mutate(targetex = as.numeric(targetex)) %>% 
  mutate(inAnalysis = as.integer(inAnalysis) - 1)
summary(temp2)
str(temp2$inAnalysis)
table(temp2$inAnalysis)
head(temp2$inAnalysis, 10)
# Looks okay right now
temp2 %>% filter(inAnalysis == 1) %>% summary()
temp2 %>% filter(inAnalysis == 1) %>% count()
# 49,634 people
### Age
younger <- temp2 %>% mutate(inAnalysis = ifelse(age > 42, 0, inAnalysis))
younger %>% select(inAnalysis) %>% table()
younger %>% filter(inAnalysis == 1) %>% summary()
younger %>% filter(inAnalysis == 1) %>% select(age) %>% count()
# 24,310 people
older <- temp2 %>% mutate(inAnalysis = ifelse(age <= 42, 0, inAnalysis))
older %>% filter(inAnalysis == 1) %>% summary()
older %>% filter(inAnalysis == 1) %>% select(age) %>% count()
# 25,324 people
# Sum is correct!!!

## Younger
younger.svy <- full.logit.survey.mi.boot(younger)
younger.svy
# ATE = 0.0041991015 (-0.0416974711, 0.0500956741)
# Round 1: G_comp          var     standerr          CI1          CI2 
#         0.0121920771  0.0007293959  0.0270073307 -0.0407413184  0.0651254725 

## Older
older.svy <- full.logit.survey.mi.boot(older)
older.svy
# Round 1: G_comp          var     standerr          CI1          CI2 
#       -0.005799553  0.001406618  0.037504909 -0.079307824  0.067708718

# ### Race / Ethnicity - didn't work because some strata only had one PSU - survey didn't work
# white <- temp2 %>% mutate(inAnalysis = ifelse(raceeth != 1, 0, inAnalysis))
# white %>% filter(inAnalysis == 1) %>% select(raceeth) %>% table()
# # 14,911 people
# latin <- temp2 %>% mutate(inAnalysis = ifelse(raceeth != 2, 0, inAnalysis))
# latin %>% filter(inAnalysis == 1) %>% select(raceeth) %>% table()
# # 13,572 people
# black <- temp2 %>% mutate(inAnalysis = ifelse(raceeth != 3, 0, inAnalysis))
# black %>% filter(inAnalysis == 1) %>% select(raceeth) %>% table()
# # 11,167 people
# other <- temp2 %>% mutate(inAnalysis = ifelse(raceeth != 4, 0, inAnalysis))
# other %>% filter(inAnalysis == 1) %>% select(raceeth) %>% table()
# # 9,984 people
# # Total is 49,634 - sum is correct!!!
# 
# white.svy <- full.logit.survey.mi.boot(white)
# # Round 1: G_comp          var     standerr          CI1          CI2 
# #         0
# latin.svy <- full.logit.survey.mi.boot(latin)
# # Round 1: G_comp          var     standerr          CI1          CI2 
# #         0
# black.svy <- full.logit.survey.mi.boot(black)
# # Round 1: G_comp          var     standerr          CI1          CI2 
# #         0
# other.svy <- full.logit.survey.mi.boot(other)
# # Round 1: G_comp          var     standerr          CI1          CI2 
# #         0

### Obesity - didn't work! Only one PSU in some strata
obese <- temp2 %>% mutate(inAnalysis = ifelse(bmi <30, 0, inAnalysis))
obese %>% filter(inAnalysis == 1) %>% select(bmi) %>% summary()
# Range: 30.0 - 86.2
obese %>% filter(inAnalysis == 1) %>% select(bmi) %>% count()
# 19,272 people
nonobese <- temp2 %>% mutate(inAnalysis = ifelse(bmi >=30, 0, inAnalysis))
nonobese %>% filter(inAnalysis == 1) %>% select(bmi) %>% summary()
# Range: 14.9 - 29.9
nonobese %>% filter(inAnalysis == 1) %>% select(bmi) %>% count()
# 30,310 people
# Total is 49,582 - slightly smaller, maybe because of NAs
temp2 %>% filter(inAnalysis == 1) %>% select(bmi) %>% summary()
# 52 Nas
# Total is 49,634 - sum is correct!!!

obese.svy <- full.logit.survey.mi.boot(obese)
# Round 1: G_comp          var     standerr          CI1          CI2 
#         0

nonobese.svy <- full.logit.survey.mi.boot(nonobese)
# Round 1: G_comp          var     standerr          CI1          CI2 
#         0.0422506862  0.0008288905  0.0287904584 -0.0141775754  0.0986789478

# Overweight
overwt <- temp2 %>% mutate(inAnalysis = ifelse(bmi <25, 0, inAnalysis))
overwt %>% filter(inAnalysis == 1) %>% select(bmi) %>% summary()
# Range: 25.0 - 86.2
overwt %>% filter(inAnalysis == 1) %>% select(bmi) %>% count()
# 36,721 people
nonoverwt <- temp2 %>% mutate(inAnalysis = ifelse(bmi >=25, 0, inAnalysis))
nonoverwt %>% filter(inAnalysis == 1) %>% select(bmi) %>% summary()
# Range: 14.9 - 24.9
nonoverwt %>% filter(inAnalysis == 1) %>% select(bmi) %>% count()
# 12,861 people

overwt.svy <- full.logit.survey.mi.boot(overwt)
# Round 1: G_comp          var     standerr          CI1          CI2 
#         0.0003691242  0.0007392665  0.0271894548 -0.0529212279  0.0536594763

nonoverwt.svy <- full.logit.survey.mi.boot(nonoverwt)
# Round 1: G_comp          var     standerr          CI1          CI2 
#         0


