## Exercise and sleep and covariates
#Libraries
# library(tidyverse)
# library(nhanesA)
# 
# setwd("final_project")
getwd()

slpexcov <- read_csv("slpexconf.csv")
summary(slpexcov)

slpexcov %>% 
  dplyr::select(targetex, targetslp) %>% 
  group_by(targetex, targetslp) %>% 
  summarise(n = n())

# Fit a logistic model to the data without confounders and look at results
glm.slpex = glm(targetslp ~ targetex, family = binomial(link = "logit"), data = slpexcov)
summary(glm.slpex)
exp(cbind(OR = coef(glm.slpex), confint(glm.slpex)))

# # Fit data with full covariates and see how data changes
# glm.slpexcov = glm(targetslp ~ targetex + factor(gender) + age + factor(raceeth) + factor(usborn) + usyears + factor(educ) + 
#                      factor(marital) + factor(pregnancy) + household + factor(income), 
#                    family = binomial(link = "logit"), data = slpexcov)
# summary(glm.slpexcov)
# exp(cbind(OR = coef(glm.slpexcov), confint(glm.slpexcov)))

# Fit data with fewer covariates and see how data changes
glm.slpexcov2 = glm(targetslp ~ targetex + factor(gender) + age + factor(raceeth) + factor(usborn) + factor(educ) + 
                      factor(marital) + household + factor(income),
                   family = binomial(link = "logit"), data = slpexcov)
summary(glm.slpexcov2)
exp(cbind(OR = coef(glm.slpexcov2), confint(glm.slpexcov2)))

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
library(summarytools)
dfSummary(slpexcov, style = "grid", plain.ascii = TRUE)

# Race/ethnicity
racet <- slpexcov %>% 
  dplyr::select(targetex, targetslp, raceeth) %>% 
  group_by(targetex, targetslp, raceeth) %>% 
  summarise(n = n())
racet
# Smallest cell: ex: 1, slp: 0, raceeth: 2, count: 64

# Years in the US
usys <- slpexcov %>% 
  dplyr::select(targetex, targetslp, usyears) %>% 
  group_by(targetex, targetslp, usyears) %>% 
  summarise(n = n())
usys
# There are a couple of cells with very few values -> tarxetex==1, targetslp=0, usyears=(1|9)
# Smallest cell: ex: 1, slp: 0, usyears: 1, count: 1
# Small cell: ex: 1, slp: 0, usyears: 9, count: 2
## Shouldn't include this variable in our model

# Education
ed <- slpexcov %>% 
  dplyr::select(targetex, targetslp, educ) %>% 
  group_by(targetex, targetslp, educ) %>% 
  summarise(n = n())
ed
# Smallest cell: ex: 1, slp: 0, educ: 1, count: 60

# Marital
mrg <- slpexcov %>% 
  dplyr::select(targetex, targetslp, marital) %>% 
  group_by(targetex, targetslp, marital) %>% 
  summarise(n = n())
mrg
# Smallest cell: ex: 1, slp: 0, marital: 2, count: 220

# Pregnancy
slpexcov %>% 
  dplyr::select(targetex, targetslp, pregnancy) %>% 
  group_by(targetex, targetslp, pregnancy) %>% 
  summarise(n = n())
## There are empty cells: ex: 1, slp: 0, pregnancy: 1
# Smallest cell: ex: 0, slp: 0, pregnancy 1, count: 8
## Too many NAs, shouldn't include this variable in our model

# Household
home <- slpexcov %>% 
  dplyr::select(targetex, targetslp, household) %>% 
  group_by(targetex, targetslp, household) %>% 
  summarise(n = n())
home
# Smallest cell: ex: 1, slp: 0, household: (1|6), count: 59

# Income
plata <- slpexcov %>% 
  dplyr::select(targetex, targetslp, income) %>% 
  group_by(targetex, targetslp, income) %>% 
  summarise(n = n())
plata
# Smallest cell: ex: 1, slp: 0, income: 1, count: 60

### Old income info:
# There are a couple of cells with very few values -> tarxetex==1, targetslp=0, income=(1|2|13)
# Smallest cell: ex: 1, slp: 0, income: 13, count: 5
# Small cell: ex: 1, slp: 0, income: 1, count: 6
# Small cell: ex: 1, slp: 0, income: 2, count: 9

#####
# Fix some covariates and try again
# gender, age, raceeth, usborn, usyears, educ, marital, pregnancy, household, income
# Get rid of: usyears and pregnancy (too many missing)

# New csv file with fewer (and fixed) covariates:
slpexcov2 <- slpexcov %>% 
  dplyr::select(-usyears, -pregnancy)
summary(slpexcov2)
head(slpexcov2)
tail(slpexcov2)
write_csv(slpexcov2, "slpexcov2.csv")

binslpex2 <- as_tibble(slpexcov2)
binslpex2 %>% 
  dplyr::select(targetex, targetslp) %>% 
  group_by(targetex, targetslp) %>% 
  summarise(n = n())

#####

## Should add BMI, smoking status, alcohol consumption, depression.
## Consider running multiple imputation for income (722 NAs)
## Once covariates are complete, replace simple imputations for exposure with IPTW
## Use NHANES weights for data
