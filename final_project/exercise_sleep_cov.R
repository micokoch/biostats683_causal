## Exercise and sleep and covariates
#Libraries
# library(tidyverse)
# library(nhanesA)
# 
# setwd("final_project")
getwd()

slpexcov1517 <- read_csv("slpexcov1517.csv")
summary(slpexcov1517)

slpexcov1517 %>% 
  dplyr::select(targetex, targetslp) %>% 
  group_by(targetex, targetslp) %>% 
  summarise(n = n())

# Fit a logistic model to the data without confounders and look at results
glm.slpex = glm(targetslp ~ targetex, family = binomial(link = "logit"), data = slpexcov1517)
summary(glm.slpex)
exp(cbind(OR = coef(glm.slpex), confint(glm.slpex)))

# Fit data with covariates and see how effects change
glm.slpexcov = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + household + 
                     factor(income) + factor(snoring) + factor(apnea) + bmi + waist + factor(smoke) + 
                     factor(alcohol) + factor(depressed),
                   family = binomial(link = "logit"), data = slpexcov1517)
summary(glm.slpexcov)
exp(cbind(OR = coef(glm.slpexcov), confint(glm.slpexcov)))

# Removed covariates that have many NAs or seem unimportant
glm.slpexcov2 = glm(targetslp ~ targetex + age + factor(raceeth) + factor(educ) + factor(marital) + 
                      factor(household) + bmi + waist + factor(depressed),
                   family = binomial(link = "logit"), data = slpexcov1517)
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
# library(summarytools)
# dfSummary(slpexcov, style = "grid", plain.ascii = TRUE)

# Race/ethnicity
racet <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, raceeth) %>% 
  group_by(targetex, targetslp, raceeth) %>% 
  summarise(n = n())
racet
# Smallest cell: ex: 1, slp: 0, raceeth: 4, count: 48

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
# Smallest cell: ex: 1, slp: 0, marital: 2, count: 115

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

# Household
depression <- slpexcov1517 %>% 
  dplyr::select(targetex, targetslp, depressed) %>% 
  group_by(targetex, targetslp, depressed) %>% 
  summarise(n = n())
depression
# Various cells have NAs for depressed (00NA-39, 01NA-144, 10NA-20, 11NA-98)
# Smallest cells: ex: 1, slp: 0, depressed: 1, count: 16

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
