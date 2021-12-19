##### Cleaning datasets and making new variables

### Libraries
library(tidyverse)
library(mice)
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
  nm <- deparse(substitute(x))
  x <- mutate(x, roworder = row_number(), .before = "vigex")
  x <- mutate(x, vigexminwk = unlist((x[,"daysvigex"] * x[,"minvigex"])), .after = "minvigex")
  x <- mutate(x, modexminwk = unlist((x[,"daysmodex"] * x[,"minmodex"])), .after = "minmodex")
  x <- mutate(x, exminwk = unlist(((x[,"vigexminwk"]*2) + x[,"modexminwk"])), .before = "slphrs")
  x <- mutate(x, targetex = unlist(ifelse(x[,"exminwk"] < 150, 0, 1)), .before = "slphrs")
  x <- mutate(x, targetslp = unlist(ifelse(x[,"slphrs"] >= 7, 1, 0)), .after = "slphrs")
  x <- mutate(x, phqsum = rowSums(x[,29:37], na.rm = FALSE), .after = "phq09")
  x <- mutate(x, depressed = unlist(ifelse(x[,"phqsum"] > 9, 1, ifelse(x[,"phqsum"] < 10, 0, NA))), .after = "phqsum")
  x <- mutate(x, imputation = str_sub(nm, -1), .after = last_col())
  return(x)
}

format.slpex.data <- function(x){
  x <- x %>% 
    mutate(
      roworder = as.integer(roworder),
      vigex = as.factor(vigex),
      daysvigex = as.integer(daysvigex),
      minvigex = as.integer(minvigex),
      vigexminwk = as.numeric(vigexminwk),
      modex = as.factor(modex),
      daysmodex = as.integer(daysmodex),
      minmodex = as.integer(minmodex),
      modexminwk = as.numeric(modexminwk),
      exminwk = as.numeric(exminwk),
      targetex = as.factor(targetex),
      slphrs = as.numeric(slphrs),
      targetslp = as.factor(targetslp),
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
      depressed = as.factor(depressed),
      imputation = as.integer(imputation)
    )
    return(x)
}

men.20.64 <- function(x){
  x <- x %>% mutate(inAnalysis = as.factor(ifelse((gender == 1 & (age >= 20 & age < 65)), 1, 0)))
  return(x)
}

add.seqn.wts <- function(x){
  x <- merge(x, seqn.wts, by = 'roworder')
  return(x)
}

organize.slpex <- function(x){
  a <- clean.slpex.data(x)
  b <- format.slpex.data(a)
  c <- men.20.64(b)
  d <- add.seqn.wts(c)
  return(d)
}

binary.logistic <- function(x){
  glm.slpex = glm(targetslp ~ targetex, family = binomial(link = "logit"), data = x)
  print(summary(glm.slpex))
  print(exp(cbind(OR = coef(glm.slpex), confint(glm.slpex))))
  # ATE (risk difference - marginal effect)
  # Probability for each individual based on the model fit under targetex = 1
  prob.1 <- x %>% mutate(targetex=1)
  p1.bin <- mean(predict(glm.slpex, prob.1, type = "response"))
  # Probability for each individual based on the model fit under targetex = 0
  prob.0 <- x %>% mutate(targetex=0)
  p0.bin <- mean(predict(glm.slpex, prob.0, type = "response"))
  # Marginal probabilities
  bin.marg.prob <- (bin.marg.prob = c(p1.bin, p0.bin))
  print(bin.marg.prob)
  risk_diff.bin <- p1.bin - p0.bin
  print(risk_diff.bin)
  # Try Risk Communicator results
  slpex.results.bin <- gComp(data = x, Y = "targetslp", X = "targetex", 
                             outcome.type = "binary", R = 200)
  print(slpex.results.bin)
}

full.logistic <- function(x){
  glm.slpexcov = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                       factor(household) + factor(income) + factor(snoring) + factor(apnea) + bmi + 
                       waist + factor(smoke) + factor(alcohol) + factor(depressed),
                     family = binomial(link = "logit"), data = x)
  print(summary(glm.slpexcov))
  print(exp(cbind(OR = coef(glm.slpexcov), confint(glm.slpexcov))))
  # ATE (risk difference - marginal effect)
  # Probability for each individual based on the model fit under targetex = 1
  prob.1 <- x %>% mutate(targetex=1)
  p1.full <- mean(predict(glm.slpexcov, prob.1, type = "response"), na.rm = T)
  # Probability for each individual based on the model fit under targetex = 0
  prob.0 <- x %>% mutate(targetex=0)
  p0.full <- mean(predict(glm.slpexcov, prob.0, type = "response"), na.rm = T)
  # Marginal probabilities
  full.marg.prob <- (full.marg.prob = c(p1.full, p0.full))
  print(full.marg.prob)
  risk_diff.full <- p1.full - p0.full
  print(risk_diff.full)
  # Try Risk Communicator results
  slpex.results.full <- gComp(data = x, Y = "targetslp", X = "targetex", 
                             Z = c("age", "raceeth", "educ", "marital", "household", 
                                   "income", "snoring", "apnea", "bmi", "waist", "smoke", 
                                   "alcohol", "depressed"), outcome.type = "binary", R = 200)
  print(slpex.results.sat)
  # Obtain risk difference value
  risk_diff.full <- as.numeric(slpex.results.full$results.df[1,4])
  print(risk_diff.full)
}

final.logistic <- function(x){
  glm.slpexcov2 = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                        bmi + waist + factor(depressed),
                      family = binomial(link = "logit"), data = x)
  print(summary(glm.slpexcov2))
  print(exp(cbind(OR = coef(glm.slpexcov2), confint(glm.slpexcov2))))
  # ATE (risk difference - marginal effect)
  # Probability for each individual based on the model fit under targetex = 1
  prob.1 <- x %>% mutate(targetex=1)
  p1.pars <- mean(predict(glm.slpexcov2, prob.1, type = "response"), na.rm = T)
  # Probability for each individual based on the model fit under targetex = 0
  prob.0 <- x %>% mutate(targetex=0)
  p0.pars <- mean(predict(glm.slpexcov2, prob.0, type = "response"), na.rm = T)
  # Marginal probabilities
  pars.marg.prob <- (full.marg.prob = c(p1.pars, p0.pars))
  print(pars.marg.prob)
  risk_diff.pars <- p1.pars - p0.pars
  print(risk_diff.pars)
  # Try Risk Communicator results
  slpex.results.pars <- gComp(data = x, Y = "targetslp", X = "targetex", 
                              Z = c("age", "raceeth", "educ", "marital", "bmi", "waist", 
                                    "depressed"), outcome.type = "binary", R = 200)
  print(slpex.results.pars)
  risk_diff.pars <- as.numeric(slpex.results.pars$results.df[1,4])
  print(risk_diff.pars)
}

##### Compile datasets
## First, create unimputed dataset
clean_exslp_1517 <- read_csv("clean_exslp_1517.csv")
# Create dataset with SEQN and weights
seqn.wts <- clean_exslp_1517 %>% select(-1, -usyears) %>% 
  select(SEQN, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) %>% 
  mutate(roworder = row_number(), .before = "SEQN")
write_csv(seqn.wts, "seqn.wts.csv")
unimputed.0 <- clean_exslp_1517 %>% select(-1, -usyears) %>% 
  select(-SEQN, -WTINT2YR, -WTMEC2YR, -SDMVPSU, -SDMVSTRA)
write_csv(unimputed.0, "unimputed.0.csv")

## Read in datasets
unimputed.0 <- read_csv("unimputed.0.csv")
imputed.1 <- read_csv("imputedData.1.csv")
imputed.2 <- read_csv("imputedData.2.csv")
imputed.3 <- read_csv("imputedData.3.csv")
imputed.4 <- read_csv("imputedData.4.csv")
imputed.5 <- read_csv("imputedData.5.csv")
imputed.6 <- read_csv("imputedData.6.csv")
imputed.7 <- read_csv("imputedData.7.csv")
imputed.8 <- read_csv("imputedData.8.csv")
imputed.9 <- read_csv("imputedData.9.csv")
imputed.10 <- read_csv("imputedData.10.csv")
imputed.11 <- read_csv("imputedData.11.csv")
imputed.12 <- read_csv("imputedData.12.csv")
## Compare names and sizes of unimputed and imputed datasets
names(unimputed.0)
names(imputed.3)
str(unimputed.0)
str(imputed.12)

# Quick tests
test <- clean.slpex.data(imputed.1)
test2 <- format.slpex.data(test)
test3 <- men.20.64(test2)
test4 <- add.seqn.wts(test3)
test5 <- organize.slpex(imputed.3)
View(test4)
str(test5)
names(test5)

# organize.slpex <- function(x, seqn.wts = seqn.wts){
#   a <- clean.slpex.data(x)
#   b <- format.slpex.data(a)
#   c <- men.20.64(b)
#   d <- add.seqn.wts(c, seqn.wts = seqn.wts)
#   return(d)
# }



# Clean datasets
unimputed.0.comp <- organize.slpex(unimputed.0)
imputed.1.comp <- organize.slpex(imputed.1)
imputed.2.comp <- organize.slpex(imputed.2)
imputed.3.comp <- organize.slpex(imputed.3)
imputed.4.comp <- organize.slpex(imputed.4)
imputed.5.comp <- organize.slpex(imputed.5)
imputed.6.comp <- organize.slpex(imputed.6)
imputed.7.comp <- organize.slpex(imputed.7)
imputed.8.comp <- organize.slpex(imputed.8)
imputed.9.comp <- organize.slpex(imputed.9)
imputed.10.comp <- organize.slpex(imputed.10)
imputed.11.comp <- organize.slpex(imputed.11)
imputed.12.comp <- organize.slpex(imputed.12)

# Check cleaning
names(unimputed.0.comp)
names(imputed.10.comp)
str(unimputed.0.comp)
str(imputed.2.comp)
table(unimputed.0.comp$vigexminwk)
table(imputed.4.comp$vigexminwk)
View(unimputed.0.comp)
View(imputed.5.comp)
View(filter(unimputed.0.comp, !complete.cases(unimputed.0.comp$vigex)))
View(filter(imputed.6.comp, !complete.cases(imputed.6.comp$vigex)))
View(filter(unimputed.0.comp, !complete.cases(unimputed.0.comp$daysmodex)))
View(filter(imputed.8.comp, !complete.cases(imputed.8.comp$daysmodex)))
View(filter(unimputed.0.comp, !complete.cases(unimputed.0.comp$minmodex)))
View(filter(imputed.11.comp, !complete.cases(imputed.11.comp$minmodex)))
View(filter(unimputed.0.comp, !complete.cases(unimputed.0.comp$phq01)))
View(filter(imputed.9.comp, !complete.cases(imputed.9.comp$phq01)))

# Write full and clean datasets
write_csv(unimputed.0.comp, "unimputed.0.comp.csv")
write_csv(imputed.1.comp, "imputed.1.comp.csv")
write_csv(imputed.2.comp, "imputed.2.comp.csv")
write_csv(imputed.3.comp, "imputed.3.comp.csv")
write_csv(imputed.4.comp, "imputed.4.comp.csv")
write_csv(imputed.5.comp, "imputed.5.comp.csv")
write_csv(imputed.6.comp, "imputed.6.comp.csv")
write_csv(imputed.7.comp, "imputed.7.comp.csv")
write_csv(imputed.8.comp, "imputed.8.comp.csv")
write_csv(imputed.9.comp, "imputed.9.comp.csv")
write_csv(imputed.10.comp, "imputed.10.comp.csv")
write_csv(imputed.11.comp, "imputed.11.comp.csv")
write_csv(imputed.12.comp, "imputed.12.comp.csv")

##### Analysis of results
## Binary results
unimputed.0.comp %>% selectselect(targetex, targetslp) %>% table()
imputed.1.comp %>% selectselect(targetex, targetslp) %>% table()
imputed.7.comp %>% selectselect(targetex, targetslp) %>% table()
imputed.12.comp %>% selectselect(targetex, targetslp) %>% table()

## Look at effect estimates
# Individual datasets
binary.logistic(unimputed.0.comp)
binary.logistic(imputed.2.comp)
binary.logistic(imputed.6.comp)
binary.logistic(imputed.11.comp)

full.logistic(unimputed.0.comp)
full.logistic(imputed.3.comp)
full.logistic(imputed.8.comp)
full.logistic(imputed.10.comp)

final.logistic(unimputed.0.comp)
final.logistic(imputed.4.comp)
final.logistic(imputed.5.comp)
final.logistic(imputed.9.comp)

# Look at combined imputed dataset
bin.fit <- with(imputed, glm(targetslp ~ targetex, family = binomial(link = "logit")))
summary(pool(bin.fit))



