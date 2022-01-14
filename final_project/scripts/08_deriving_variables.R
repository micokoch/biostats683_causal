##### Cleaning datasets and making new variables

### Libraries
library(tidyverse)
library(mice)
library(riskCommunicator)
# library(VIM)
# library(nhanesA)
# library(survey)
# library(gtools)

### Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

## Functions
clean.slpex.data <- function(x){
  x <- mutate(x, .id = row_number(), .before = "vigex")
  x <- mutate(x, vigexminwk = unlist((x[,"daysvigex"] * x[,"minvigex"])), .after = "minvigex")
  x <- mutate(x, modexminwk = unlist((x[,"daysmodex"] * x[,"minmodex"])), .after = "minmodex")
  x <- mutate(x, exminwk = unlist(((x[,"vigexminwk"]*2) + x[,"modexminwk"])), .before = "slphrs")
  x <- mutate(x, targetex = unlist(ifelse(x[,"exminwk"] < 150, 0, 1)), .before = "slphrs")
  x <- mutate(x, targetslp = unlist(ifelse(x[,"slphrs"] >= 7, 1, 0)), .after = "slphrs")
  x <- mutate(x, phqsum = rowSums(x[,29:37], na.rm = FALSE), .after = "phq09")
  x <- mutate(x, depressed = unlist(ifelse(x[,"phqsum"] > 9, 1, ifelse(x[,"phqsum"] < 10, 0, NA))), 
              .after = "phqsum")
  return(x)
}

format.slpex.data <- function(x){
  x <- x %>% 
    mutate(
      .imp = as.integer(.imp),
      .id = as.integer(.id),
      vigex = as.factor(vigex),
      daysvigex = as.integer(daysvigex),
      minvigex = as.integer(minvigex),
      vigexminwk = as.numeric(vigexminwk),
      modex = as.factor(modex),
      daysmodex = as.integer(daysmodex),
      minmodex = as.integer(minmodex),
      modexminwk = as.numeric(modexminwk),
      exminwk = as.numeric(exminwk),
      targetex = as.numeric(targetex),
      slphrs = as.numeric(slphrs),
      targetslp = as.numeric(targetslp),
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
      phq01 = as.numeric(phq01),
      phq02 = as.numeric(phq02),
      phq03 = as.numeric(phq03),
      phq04 = as.numeric(phq04),
      phq05 = as.numeric(phq05),
      phq06 = as.numeric(phq06),
      phq07 = as.numeric(phq07),
      phq08 = as.numeric(phq08),
      phq09 = as.numeric(phq09),
      phqsum = as.integer(phqsum),
      depressed = as.factor(depressed)
    )
    return(x)
}

men.20.64 <- function(x){
  x <- x %>% mutate(inAnalysis = as.factor(ifelse((gender == 1 & (age >= 20 & age < 65)), 1, 0)))
  return(x)
}

add.seqn.wts <- function(x){
  x <- merge(x, seqn.wts, by = '.id')
  return(x)
}

organize.slpex <- function(x){
  nm <- deparse(substitute(x))
  x <- mutate(x, .imp = str_sub(nm, -2, -1))
  a <- clean.slpex.data(x)
  b <- format.slpex.data(a)
  c <- men.20.64(b)
  d <- add.seqn.wts(c)
  e <- d %>% relocate(.imp)
  return(e)
}

factor.exp.out <- function(x){
  x <- x %>% mutate(targetex = as.factor(targetex), targetslp = as.factor(targetslp))
  return(x)
}

binary.logistic <- function(x){
  temp2 <- x %>% filter(inAnalysis == 1)
  glm.slpex = glm(targetslp ~ targetex, family = binomial(link = "logit"), data = temp2)
  print(summary(glm.slpex))
  print(exp(cbind(OR = coef(glm.slpex), confint(glm.slpex))))
  # ATE (risk difference - marginal effect)
  # Probability for each individual based on the model fit under targetex = 1
  prob.1 <- temp2 %>% mutate(targetex=1)
  p1.bin <- mean(predict(glm.slpex, prob.1, type = "response"))
  # Probability for each individual based on the model fit under targetex = 0
  prob.0 <- temp2 %>% mutate(targetex=0)
  p0.bin <- mean(predict(glm.slpex, prob.0, type = "response"))
  # Marginal probabilities
  bin.marg.prob <- (bin.marg.prob = c(p1.bin, p0.bin))
  print(bin.marg.prob)
  risk_diff.bin <- p1.bin - p0.bin
  print(risk_diff.bin)
  # Try Risk Communicator results
  temp2 <- temp2 %>% mutate(targetex = as.factor(targetex))
  slpex.results.bin <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                             outcome.type = "binary", R = 200)
  print(slpex.results.bin)
}

bin.logit.mi<- function(x){
  theta_i <- c()
  var_win <- c()
  for(i in 1:12){
    temp <- x %>% filter(.imp == i)
    temp2 <- temp %>% filter(inAnalysis == 1)
    glm.slpex.bin.mi <- glm(targetslp ~ targetex, family = binomial(link = "logit"), data = temp, 
                          weights = NULL, subset = inAnalysis == 1, na.action = na.exclude)
    sum.glm.slpex.bin.mi <- summary(glm.slpex.bin.mi)
    theta_i <- append(theta_i, sum.glm.slpex.bin.mi$coefficients[[2]])
    var_win <- append(var_win, sum.glm.slpex.bin.mi$coefficients[[4]]^2)
  }
  theta_i_avg <- mean(theta_i)
  var_btwn <- (theta_i - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  print(c(coef = theta_i_avg, standerr = sqrt(var_total), OR = exp(theta_i_avg)))
}

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

full.logistic <- function(x){
  temp2 <- x %>% filter(inAnalysis == 1)
  glm.slpexcov = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                       factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                       waist + factor(smoke) + factor(alcohol) + factor(depressed),
                     family = binomial(link = "logit"), data = temp2)
  print(summary(glm.slpexcov))
  print(exp(cbind(OR = coef(glm.slpexcov), confint(glm.slpexcov))))
  # ATE (risk difference - marginal effect)
  # Probability for each individual based on the model fit under targetex = 1
  prob.1 <- temp2 %>% mutate(targetex=1)
  p1.full <- mean(predict(glm.slpexcov, prob.1, type = "response"), na.rm = T)
  # Probability for each individual based on the model fit under targetex = 0
  prob.0 <- temp2 %>% mutate(targetex=0)
  p0.full <- mean(predict(glm.slpexcov, prob.0, type = "response"), na.rm = T)
  # Marginal probabilities
  full.marg.prob <- (full.marg.prob = c(p1.full, p0.full))
  print(full.marg.prob)
  risk_diff.full <- p1.full - p0.full
  print(risk_diff.full)
  # Try Risk Communicator results
  temp2 <- temp2 %>% mutate(targetex = as.factor(targetex))
  slpex.results.full <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                             Z = c("age", "raceeth", "educ", "marital", "household", 
                                   "income", "snoring", "apnea", "bmi", "waist", "smoke", 
                                   "alcohol", "depressed"), outcome.type = "binary", R = 200)
  print(slpex.results.full)
  # Obtain risk difference value
  risk_diff.full <- as.numeric(slpex.results.full$results.df[1,4])
  print(risk_diff.full)
}

full.logit.mi<- function(x){
  theta_i <- c()
  var_win <- c()
  for(i in 1:12){
    temp <- x %>% filter(.imp == i)
    temp2 <- temp %>% filter(inAnalysis == 1)
    glm.slpex.full.mi = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                              factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                              waist + factor(smoke) + factor(alcohol) + factor(depressed), 
                            family = binomial(link = "logit"), data = temp, 
                            weights = NULL, subset = inAnalysis == 1, na.action = na.exclude)
    sum.glm.slpex.full.mi <- summary(glm.slpex.full.mi)
    theta_i <- append(theta_i, sum.glm.slpex.full.mi$coefficients[[2]])
    var_win <- append(var_win, sum.glm.slpex.full.mi$coefficients[[4]]^2)
  }
  theta_i_avg <- mean(theta_i)
  var_btwn <- (theta_i - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  print(c(coef = theta_i_avg, standerr = sqrt(var_total), OR = exp(theta_i_avg)))
}

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
                                     "income", "snoring", "apnea", "bmi", "waist", "smoke", 
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

final.logistic <- function(x){
  temp2 <- x %>% filter(inAnalysis == 1)
  glm.slpexcov2 = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                        bmi + waist + factor(depressed),
                      family = binomial(link = "logit"), data = temp2)
  print(summary(glm.slpexcov2))
  print(exp(cbind(OR = coef(glm.slpexcov2), confint(glm.slpexcov2))))
  # ATE (risk difference - marginal effect)
  # Probability for each individual based on the model fit under targetex = 1
  prob.1 <- temp2 %>% mutate(targetex=1)
  p1.pars <- mean(predict(glm.slpexcov2, prob.1, type = "response"), na.rm = T)
  # Probability for each individual based on the model fit under targetex = 0
  prob.0 <- temp2 %>% mutate(targetex=0)
  p0.pars <- mean(predict(glm.slpexcov2, prob.0, type = "response"), na.rm = T)
  # Marginal probabilities
  pars.marg.prob <- (full.marg.prob = c(p1.pars, p0.pars))
  print(pars.marg.prob)
  risk_diff.pars <- p1.pars - p0.pars
  print(risk_diff.pars)
  # Try Risk Communicator results
  temp2 <- temp2 %>% mutate(targetex = as.factor(targetex))
  slpex.results.pars <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                              Z = c("age", "raceeth", "educ", "marital", "bmi", "waist", 
                                    "depressed"), outcome.type = "binary", R = 200)
  print(slpex.results.pars)
  risk_diff.pars <- as.numeric(slpex.results.pars$results.df[1,4])
  print(risk_diff.pars)
}

final.logit.mi<- function(x){
  theta_i <- c()
  var_win <- c()
  for(i in 1:12){
    temp <- x %>% filter(.imp == i)
    temp2 <- temp %>% filter(inAnalysis == 1)
    glm.slpex.final.mi = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                              bmi + waist + factor(depressed), 
                            family = binomial(link = "logit"), data = temp, 
                            weights = NULL, subset = inAnalysis == 1, na.action = na.exclude)
    sum.glm.slpex.final.mi <- summary(glm.slpex.final.mi)
    theta_i <- append(theta_i, sum.glm.slpex.final.mi$coefficients[[2]])
    var_win <- append(var_win, sum.glm.slpex.final.mi$coefficients[[4]]^2)
  }
  theta_i_avg <- mean(theta_i)
  var_btwn <- (theta_i - theta_i_avg)^2
  var_btwn_avg <- sum(var_btwn)/11
  var_win_avg <- mean(var_win)
  var_total <- var_win_avg + var_btwn_avg + var_btwn_avg/12
  print(c(coef = theta_i_avg, standerr = sqrt(var_total), OR = exp(theta_i_avg)))
}

final.logit.mi.rc <- function(x){
  theta_i <- c()
  var_win <- c()
  for(i in 1:12){
    temp <- x %>% filter(.imp == i)
    temp2 <- temp %>% filter(inAnalysis == 1) %>% 
      mutate(targetex = as.factor(targetex)) %>% 
      na.exclude()
    slpex.results.final <- gComp(data = temp2, Y = "targetslp", X = "targetex", 
                                 Z = c("age", "raceeth", "educ", "marital", "bmi", "waist", 
                                       "depressed"), outcome.type = "binary", R = 200)
    theta_i <- append(theta_i, slpex.results.final$results.df$Estimate[1])
    std.err.ci <- ((slpex.results.final[["results.df"]][["97.5% CL"]][1] - 
                      slpex.results.final[["results.df"]][["2.5% CL"]][1]) / 
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

##### Compile datasets
## First, create unimputed dataset
clean_exslp_1517 <- read_csv("clean_exslp_1517.csv")
# Create dataset with SEQN and weights
seqn.wts <- clean_exslp_1517 %>% select(-1, -usyears) %>% 
  select(SEQN, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>% 
  mutate(.id = row_number(), .before = "SEQN") %>% 
  mutate(WTINT4YR = 0.5 * WTINT2YR, WTMEC4YR = 0.5 * WTMEC2YR) %>% 
  mutate(combmvu = SDMVSTRA * 2 + SDMVPSU) %>% 
  mutate(normwts = (WTINT4YR * (length(WTINT4YR)/sum(WTINT4YR))))
write_csv(seqn.wts, "seqn.wts.csv")
unimputed.00 <- clean_exslp_1517 %>% select(-1, -usyears) %>% 
  select(-SEQN, -WTINT2YR, -WTMEC2YR, -SDMVPSU, -SDMVSTRA)
write_csv(unimputed.00, "unimputed.00.csv")

## Read in datasets
unimputed.00 <- read_csv("unimputed.00.csv")
imputed.01 <- read_csv("imputedData.01.csv")
imputed.02 <- read_csv("imputedData.02.csv")
imputed.03 <- read_csv("imputedData.03.csv")
imputed.04 <- read_csv("imputedData.04.csv")
imputed.05 <- read_csv("imputedData.05.csv")
imputed.06 <- read_csv("imputedData.06.csv")
imputed.07 <- read_csv("imputedData.07.csv")
imputed.08 <- read_csv("imputedData.08.csv")
imputed.09 <- read_csv("imputedData.09.csv")
imputed.10 <- read_csv("imputedData.10.csv")
imputed.11 <- read_csv("imputedData.11.csv")
imputed.12 <- read_csv("imputedData.12.csv")
## Compare names and sizes of unimputed and imputed datasets
names(unimputed.00)
names(imputed.03)
# str(unimputed.00)
# str(imputed.12)

# Quick tests
# test <- clean.slpex.data(imputed.01)
# test2 <- format.slpex.data(test)
# test3 <- men.20.64(test2)
# test4 <- add.seqn.wts(test3)
test5 <- organize.slpex(imputed.03)
View(test5)
str(test5)
names(test5)
### IT WORKS!!!!

# Clean datasets
unimputed.00.comp <- organize.slpex(unimputed.00)
imputed.01.comp <- organize.slpex(imputed.01)
imputed.02.comp <- organize.slpex(imputed.02)
imputed.03.comp <- organize.slpex(imputed.03)
imputed.04.comp <- organize.slpex(imputed.04)
imputed.05.comp <- organize.slpex(imputed.05)
imputed.06.comp <- organize.slpex(imputed.06)
imputed.07.comp <- organize.slpex(imputed.07)
imputed.08.comp <- organize.slpex(imputed.08)
imputed.09.comp <- organize.slpex(imputed.09)
imputed.10.comp <- organize.slpex(imputed.10)
imputed.11.comp <- organize.slpex(imputed.11)
imputed.12.comp <- organize.slpex(imputed.12)

# Check cleaning
names(unimputed.00.comp)
names(imputed.10.comp)
str(unimputed.00.comp)
str(imputed.02.comp)
table(unimputed.00.comp$vigexminwk)
table(imputed.04.comp$vigexminwk)
# View(unimputed.00.comp)
# View(imputed.05.comp)
# View(filter(unimputed.00.comp, !complete.cases(unimputed.00.comp$vigex)))
# View(filter(imputed.06.comp, !complete.cases(imputed.06.comp$vigex)))
# View(filter(unimputed.00.comp, !complete.cases(unimputed.00.comp$daysmodex)))
# View(filter(imputed.08.comp, !complete.cases(imputed.08.comp$daysmodex)))
# View(filter(unimputed.00.comp, !complete.cases(unimputed.00.comp$minmodex)))
# View(filter(imputed.11.comp, !complete.cases(imputed.11.comp$minmodex)))
# View(filter(unimputed.00.comp, !complete.cases(unimputed.00.comp$phq01)))
# View(filter(imputed.09.comp, !complete.cases(imputed.09.comp$phq01)))

# Write full and clean datasets
write_csv(unimputed.00.comp, "unimputed.00.comp.csv")
write_csv(imputed.01.comp, "imputed.01.comp.csv")
write_csv(imputed.02.comp, "imputed.02.comp.csv")
write_csv(imputed.03.comp, "imputed.03.comp.csv")
write_csv(imputed.04.comp, "imputed.04.comp.csv")
write_csv(imputed.05.comp, "imputed.05.comp.csv")
write_csv(imputed.06.comp, "imputed.06.comp.csv")
write_csv(imputed.07.comp, "imputed.07.comp.csv")
write_csv(imputed.08.comp, "imputed.08.comp.csv")
write_csv(imputed.09.comp, "imputed.09.comp.csv")
write_csv(imputed.10.comp, "imputed.10.comp.csv")
write_csv(imputed.11.comp, "imputed.11.comp.csv")
write_csv(imputed.12.comp, "imputed.12.comp.csv")

##### Analysis of results
## Binary results
unimputed.00.comp %>% select(targetex, targetslp) %>% table()
imputed.01.comp %>% select(targetex, targetslp) %>% table()
imputed.07.comp %>% select(targetex, targetslp) %>% table()
imputed.12.comp %>% select(targetex, targetslp) %>% table()

unimputed.00.comp %>% select(targetex, targetslp) %>% table %>% prop.table(margin = 1)
imputed.02.comp %>% select(targetex, targetslp) %>% table %>% prop.table(margin = 1)
imputed.06.comp %>% select(targetex, targetslp) %>% table %>% prop.table(margin = 1)
imputed.11.comp %>% select(targetex, targetslp) %>% table %>% prop.table(margin = 1)

unimputed.00.comp %>% select(targetex, targetslp) %>% table %>% prop.table(margin = 2)
imputed.03.comp %>% select(targetex, targetslp) %>% table %>% prop.table(margin = 2)
imputed.05.comp %>% select(targetex, targetslp) %>% table %>% prop.table(margin = 2)
imputed.10.comp %>% select(targetex, targetslp) %>% table %>% prop.table(margin = 2)

## Look at effect estimates
# Read in dataset
unimputed.00.comp <- read.csv("unimputed.00.comp.csv")

# Unimputed data
unimputed.bin.nona <- unimputed.00.comp %>% 
  select(targetex, targetslp, inAnalysis) %>% 
  na.omit()
unimputed.bin.nona %>% filter(inAnalysis == 1) %>% count()

unimputed.full.nona <- unimputed.00.comp %>% 
  select(targetex, targetslp, age, raceeth, educ, marital, household, income, 
         bmi, waist, smoke, alcohol, depressed, inAnalysis) %>% 
  na.omit()
unimputed.full.nona %>% filter(inAnalysis == 1) %>% count()

unimputed.final.nona <- unimputed.00.comp %>% 
  select(targetex, targetslp, age, raceeth, educ, marital, bmi, waist, depressed, inAnalysis) %>% 
  na.omit()
# Logistic regressions with unimputed data (with no NAs)
binary.logistic(unimputed.00.comp)
binary.logistic(unimputed.bin.nona)
full.logistic(unimputed.00.comp)
full.logistic(unimputed.full.nona)
final.logistic(unimputed.00.comp)
final.logistic(unimputed.final.nona)

# binary.logistic(imputed.02.comp)
# 
# unimputed.00.comp %>% filter(inAnalysis == 1) %>% binary.logistic()
# imputed.06.comp %>% filter(inAnalysis == 1) %>% binary.logistic()
# imputed.11.comp %>% filter(inAnalysis == 1) %>% binary.logistic()
# 
# full.logistic(unimputed.00.comp)
# full.logistic(imputed.03.comp)
# 
# unimputed.00.comp %>% filter(inAnalysis == 1) %>% full.logistic()
# imputed.08.comp %>% filter(inAnalysis == 1) %>% full.logistic()
# imputed.10.comp %>% filter(inAnalysis == 1) %>% full.logistic()
# 
# final.logistic(unimputed.00.comp)
# final.logistic(imputed.04.comp)
# 
# unimputed.00.comp %>% filter(inAnalysis == 1) %>% final.logistic()
# imputed.05.comp %>% filter(inAnalysis == 1) %>% final.logistic()
# imputed.09.comp %>% filter(inAnalysis == 1) %>% final.logistic()


### Look at combined imputed dataset
# First combine unimputed and imputed datasets into a long one
load("imputed.Rdata")
long.imputed.comp <- bind_rows(unimputed.00.comp, imputed.01.comp, imputed.02.comp, imputed.03.comp, 
                               imputed.04.comp, imputed.05.comp, imputed.06.comp, imputed.07.comp,
                               imputed.08.comp, imputed.09.comp, imputed.10.comp, imputed.11.comp, 
                               imputed.12.comp)
# 139,607 obs of 46 variables - 13 sets of 10,739 obs - correct!
write_csv(long.imputed.comp, "long.imputed.comp.csv")

# Second, make a long imputed dataset (without unimputed)
long.only_imputed.comp <- bind_rows(imputed.01.comp, imputed.02.comp, imputed.03.comp, 
                               imputed.04.comp, imputed.05.comp, imputed.06.comp, imputed.07.comp,
                               imputed.08.comp, imputed.09.comp, imputed.10.comp, imputed.11.comp, 
                               imputed.12.comp)
# 128,868 obs of 46 variables - 13 sets of 10,739 obs - correct!
write_csv(long.only_imputed.comp, "long.only_imputed.comp.csv")

# Make datasets into mids objects - full, long
imputed_processed <- as.mids(long.imputed.comp, where = NULL, .imp = ".imp", .id = ".id")
save(imputed_processed, file = "imputed_processed.Rdata")
# Without imputed - can't save as mids object


# Run pooled logistic regressions - without weights - doesn't seem to work
bin.fit <- with(imputed_processed, glm(targetslp ~ targetex, family = binomial(link = "logit"), 
                                       subset = inAnalysis == 1, na.action = na.exclude))
pool(bin.fit)
summary(pool(bin.fit))
summary(pool(bin.fit), conf.int = TRUE, exponentiate = TRUE)

bin.logit.mi(long.imputed.comp)
### IT WORKS! My manual use of Rubin's Rules calculated correct estimate and standard error
bin.logit.mi.rc(long.imputed.comp)

full.logit.mi(long.imputed.comp)
full.logit.mi.rc(long.imputed.comp)

final.logit.mi(long.imputed.comp)
final.logit.mi.rc(long.imputed.comp)

# ### Test using RiskCommunicator to get risk difference
# imp8.bin.rc <- imputed.08.comp %>% 
#   filter(inAnalysis == 1) %>% 
#   mutate(targetex = as.factor(targetex)) %>% 
#   na.exclude()
# imp8.results.bin.rc <- gComp(data = imp8.bin.rc, Y = "targetslp", X = "targetex", 
#                              outcome.type = "binary", R = 200)
# print(imp8.results.bin.rc)
# sum.imp8.results.bin.rc <- summary(imp8.results.bin.rc$glm.result)
# print(sum.imp8.results.bin.rc)
# 
# glm.imp8.bin.mi <- glm(targetslp ~ targetex, family = binomial(link = "logit"), data = imputed.08.comp, 
#                           weights = NULL, subset = inAnalysis == 1, na.action = na.exclude)
# sum.glm.imp8.bin.mi <- summary(glm.imp8.bin.mi)
# sum.glm.imp8.bin.mi$coefficients[[2]]
# sum.glm.imp8.bin.mi$coefficients[[4]]^2

# imp8.rd <- imp8.results.bin.rc$results.df$Estimate[1]
# imp8.rd
# std.err.ci <- ((imp8.results.bin.rc[["results.df"]][["97.5% CL"]][1] - 
#                  imp8.results.bin.rc[["results.df"]][["2.5% CL"]][1]) / 
#                  (qnorm(0.975)*2))
# std.err.ci^2

### Using the MICE functions
full.fit <- with(imputed_processed, glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + 
                                          factor(marital) + factor(household) + factor(income) + 
                                          factor(snoring) + factor(apnea) + bmi + waist + 
                                          factor(smoke) + factor(alcohol) + factor(depressed), 
                                        family = binomial(link = "logit"), 
                 subset = inAnalysis == 1, na.action = na.exclude))
pool(full.fit)
summary(pool(full.fit))
summary(pool(full.fit), conf.int = TRUE, exponentiate = TRUE)

final.fit <- with(imputed_processed, glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + 
                                           factor(marital) + bmi + waist + factor(depressed), 
                                         family = binomial(link = "logit"), 
                                         subset = inAnalysis == 1, na.action = na.exclude))
pool(final.fit)
summary(pool(final.fit))
summary(pool(final.fit), conf.int = TRUE, exponentiate = TRUE)


