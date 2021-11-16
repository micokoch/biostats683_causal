## Confounders for exercise and sleep
# Libraries
# library(tidyverse)
# library(nhanesA)
# 
# setwd("final_project")
getwd()

##### Confounders

# Select variables for confounders
# Only use 2015 and 2017 because sleep data collected differently before, and issues with demographic variables

# # 2007
# demogconf2007 <- nhanes('DEMO_E') %>% 
#   dplyr::select(c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'DMDYRSUS', 'DMDEDUC3',
#                   'DMDEDUC2', 'DMDMARTL', 'DMDHHSIZ', 'DMDFMSIZ', 'INDHHIN2', 'INDFMIN2',
#                   'INDFMPIR', 'RIDEXPRG'))
# #2009
# demogconf2009 <- nhanes('DEMO_F') %>% 
#   dplyr::select(c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'DMDYRSUS', 'DMDEDUC3',
#                   'DMDEDUC2', 'DMDMARTL', 'DMDHHSIZ', 'DMDFMSIZ', 'INDHHIN2', 'INDFMIN2',
#                   'INDFMPIR', 'RIDEXPRG'))
# # 2011
# demogconf2011 <- nhanes('DEMO_G') %>% 
#   dplyr::select(c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'DMDYRSUS', 'DMDEDUC3',
#                   'DMDEDUC2', 'DMDMARTL', 'DMDHHSIZ', 'DMDFMSIZ', 'INDHHIN2', 'INDFMIN2',
#                   'INDFMPIR', 'RIDEXPRG'))
# # 2013
# demogconf2013 <- nhanes('DEMO_H') %>% 
#   dplyr::select(c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'DMDYRSUS', 'DMDEDUC3',
#                   'DMDEDUC2', 'DMDMARTL', 'DMDHHSIZ', 'DMDFMSIZ', 'INDHHIN2', 'INDFMIN2',
#                   'INDFMPIR', 'RIDEXPRG'))

# 2015
demogconf2015 <- nhanes('DEMO_I') %>% 
  dplyr::select(c('SEQN', 'RIAGENDR', 'RIDAGEYR', 'RIDRETH1', 'DMDBORN4', 'DMDYRSUS', 'DMDEDUC2',
                  'DMDMARTL', 'RIDEXPRG', 'DMDHHSIZ', 'INDHHIN2'))
# 2017
demogconf2017 <- nhanes('DEMO_J') %>% 
  dplyr::select(c('SEQN', 'RIAGENDR', 'RIDAGEYR', 'RIDRETH1', 'DMDBORN4', 'DMDYRSUS', 'DMDEDUC2',
                  'DMDMARTL', 'RIDEXPRG', 'DMDHHSIZ', 'INDHHIN2'))
# demogconf1720 <- nhanes('P_DEMO')

# BMI, smoking status, 

# Combine all confounder tables
# demogconf0709 <- dplyr::bind_rows(demogconf2007, demogconf2009)
# demogconf1113 <- dplyr::bind_rows(demogconf2011, demogconf2013)
demogconf1517 <- dplyr::bind_rows(demogconf2015, demogconf2017)
summary(demogconf1517)

## Clean confounders
# RIAGENDR
hist(demogconf1517$RIAGENDR, breaks = 2)
demogconf1517 %>% count(RIAGENDR)
# Check that there are no missing values
which(is.na(demogconf1517$RIAGENDR))
# Verified that there is no missing data for age in NHANES codebooks
demogconf1517 <- demogconf1517 %>% 
  mutate(gender = factor(RIAGENDR))
# Check that after factoring, gender codes are the same
demogconf1517 %>% count(gender)

# RIDAGEYR
hist(demogconf1517$RIDAGEYR)
# Check that there are no missing values
which(is.na(demogconf1517$RIDAGEYR))
# Verified that there is no missing data for age in NHANES codebooks
# Restrict participants to 20-64
demogconf1517 <- demogconf1517 %>%
  mutate(age = RIDAGEYR) %>% 
  subset(age >= 20 & age < 65)
hist(demogconf1517$age)

# RIDRETH1
hist(demogconf1517$RIDRETH1, breaks = 0:5)
demogconf1517 %>% count(RIDRETH1)
# There are participants in each category
# Check that there are no missing values
which(is.na(demogconf1517$RIDRETH1))
# Verified that there is no missing data for race/ethnicity in NHANES codebooks
demogconf1517 <- demogconf1517 %>% 
  mutate(raceeth = factor(RIDRETH1))
# Check that after factoring, race/ethnicity codes are the same
demogconf1517 %>% count(raceeth)

# DMDBORN4
demogconf1517 %>% count(DMDBORN4)
# There are two participants in refused & don't know -> set to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(usborn = ifelse(DMDBORN4 == 77, NA, ifelse(DMDBORN4 == 99, NA, factor(DMDBORN4))))
demogconf1517 %>% count(usborn)
# Check that there are only four missing values
which(is.na(demogconf1517$usborn))
# Check that after factoring, codes are the same
demogconf1517 %>% count(usborn)
hist(demogconf1517$usborn, breaks = 2)

# DMDYRSUS
demogconf1517 %>% count(DMDYRSUS)
# There are 5,470 NA, 82 don't know and 60 refused -> set all to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(usyears = ifelse(DMDYRSUS == 77, NA, ifelse(DMDYRSUS == 99, NA, factor(DMDYRSUS))))
# Check that after factoring, codes are the same
demogconf1517 %>% count(usyears)
hist(demogconf1517$usyears, breaks = 0:9)

# DMDEDUC2
demogconf1517 %>% count(DMDEDUC2)
hist(demogconf1517$DMDEDUC2, breaks = 0:9)
# There are 4 don't know and 1 refused -> set all to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(educ = ifelse(DMDEDUC2 == 7, NA, ifelse(DMDEDUC2 == 9, NA, factor(DMDEDUC2))))
# Check that after factoring, codes are the same
demogconf1517 %>% count(educ)
hist(demogconf1517$educ, breaks = 0:5)
## There are few-ish people with education less than high school -> combine anyone with less than high school (1 & 2)
demogconf1517 <- demogconf1517 %>% 
  mutate(educ = ifelse(educ == 1, 2, educ)) %>% 
  mutate(educ = (educ - 1))
# Check numbers
demogconf1517 %>% count(educ)
hist(demogconf1517$educ, breaks = 0:4)

# DMDMARTL
demogconf1517 %>% count(DMDMARTL)
# There are 5 refused -> set to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(marital = ifelse(DMDMARTL == 77, NA, ifelse(DMDMARTL == 99, NA, factor(DMDMARTL))))
# Check that after factoring, codes are the same
demogconf1517 %>% count(marital)
hist(demogconf1517$marital, breaks = 0:6)
## There are few-ish unmarried people -> combine unmarried & living with partner (1&6), and living alone (2-5)
demogconf1517 <- demogconf1517 %>% 
  mutate(marital = ifelse(marital == 6, 1, marital)) %>% 
  mutate(marital = ifelse(marital >1, 2, marital))
# Check numbers
demogconf1517 %>% count(marital)
hist(demogconf1517$marital, breaks = 0:2)

# RIDEXPRG
demogconf1517 %>% count(RIDEXPRG)
# There are 182 unsure -> set to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(pregnancy = ifelse(RIDEXPRG == 3, NA, factor(RIDEXPRG)))
# Check that after factoring, codes are the same
demogconf1517 %>% count(pregnancy)
hist(demogconf1517$pregnancy, breaks = 0:2)

# DMDHHSIZ
demogconf1517 %>% count(DMDHHSIZ)
hist(demogconf1517$DMDHHSIZ, breaks = 0:7)
# Check that there are no missing values
which(is.na(demogconf1517$DMDHHSIZ))
# Verified that there is no missing data for household size in NHANES codebooks
demogconf1517 <- demogconf1517 %>%
  mutate(household = DMDHHSIZ)
hist(demogconf1517$household, breaks = 0:7)
## Combine households with 6 & 7+ to make bigger cells
demogconf1517 <- demogconf1517 %>% 
  mutate(household = ifelse(household == 7, 6, household))
# Check numbers
demogconf1517 %>% count(household)
hist(demogconf1517$household, breaks = 0:6)

# INDHHIN2
demogconf1517 %>% count(INDHHIN2)
# There are 384 NA, 180 don't know and 166 refused -> set all to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(income = ifelse(INDHHIN2 == 77, NA, ifelse(INDHHIN2 == 99, NA, INDHHIN2)))
# Check that after factoring, codes are the same
demogconf1517 %>% count(income)
hist(demogconf1517$income, breaks = 0:15)
## Household income is very confusing -> makes sense to collapse and combine cells
demogconf1517 <- demogconf1517 %>% 
  mutate(income = ifelse(income < 5, 1, ifelse(income < 13, 2, ifelse(income == 13, 1, 3))))
# Check numbers
demogconf1517 %>% count(income)
hist(demogconf1517$income, breaks = 0:3)

#####
## Create table with covariates
cov1517 <- dplyr::select(demogconf1517, SEQN, gender, age, raceeth, usborn, usyears, educ, marital, 
                         pregnancy, household, income)
summary(cov1517)
write_csv(slpex, "cov1517.csv")

# Merge slpex and confounders
slpexconf <- merge(slpex, cov1517, by = 'SEQN')
summary(slpexconf)
head(slpexconf)
tail(slpexconf)
write_csv(slpexconf, "slpexconf.csv")

#####
