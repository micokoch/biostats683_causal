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

### Deomgraphic confounders
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

# Combine all confounder tables
# demogconf0709 <- dplyr::bind_rows(demogconf2007, demogconf2009)
# demogconf1113 <- dplyr::bind_rows(demogconf2011, demogconf2013)
demogconf1517 <- dplyr::bind_rows(demogconf2015, demogconf2017) # 19,225 obs of 11 variables
summary(demogconf1517)

## Clean demographic confounders
# RIAGENDR
hist(demogconf1517$RIAGENDR, breaks = 2)
demogconf1517 %>% count(RIAGENDR) # 9,449 males (1), 9,776 females
# Check that there are no missing values
which(is.na(demogconf1517$RIAGENDR)) # Confirmed that no missing data for gender in NHANES codebooks
# Create gender (factor variable)
demogconf1517 <- demogconf1517 %>% 
  mutate(gender = factor(RIAGENDR))
# Check that after factoring, gender codes are the same
demogconf1517 %>% count(gender)
# Decided to limit study to males
demogconf1517 <- demogconf1517 %>% 
  mutate(males = ifelse(gender == 1, gender, NA)) %>% 
  drop_na(males)
demogconf1517 %>% count(males) # Now tibble has 9,449 obs of 13 variables

# RIDAGEYR
hist(demogconf1517$RIDAGEYR)
# Check that there are no missing values
which(is.na(demogconf1517$RIDAGEYR))
# Confirmed that there is no missing data for age in NHANES codebooks
# Restrict participants to 20-64
demogconf1517 <- demogconf1517 %>%
  mutate(age = RIDAGEYR) %>% 
  subset(age >= 20 & age < 65)
hist(demogconf1517$age)
# Now demogconf1517 has 4,009 obs of 14 variables

# RIDRETH1
hist(demogconf1517$RIDRETH1, breaks = 0:5)
demogconf1517 %>% count(RIDRETH1) # 655 Mex/Am (1), 448 Hisp(2), 1,208 White (3), 895 Black (4), 803 other (5)
# There are participants in each category
# Check that there are no missing values
which(is.na(demogconf1517$RIDRETH1))
# Confirmed that there is no missing data for race/ethnicity in NHANES codebooks
# Combine Mexican American and other Hispanic and factor race/eth
demogconf1517 <- demogconf1517 %>% 
  mutate(raceeth = ifelse(RIDRETH1 == 1, RIDRETH1, RIDRETH1 - 1)) %>% 
  mutate(raceeth = factor(raceeth))
# Check that after factoring, race/ethnicity codes are the same
demogconf1517 %>% count(raceeth) # 1,103 Hisp, 1,208 White, 895 Black, 803 Other
# Now demogconf1517 has 4,009 obs of 15 variables

# DMDBORN4
demogconf1517 %>% count(DMDBORN4)
# Set participants in refused & don't know to NA and factor
demogconf1517 <- demogconf1517 %>% 
  mutate(usborn = ifelse(DMDBORN4 == 77, NA, ifelse(DMDBORN4 == 99, NA, factor(DMDBORN4))))
demogconf1517 %>% count(usborn)
# Check for missing values
which(is.na(demogconf1517$usborn))
# Check that after factoring, codes are the same
demogconf1517 %>% count(usborn)
hist(demogconf1517$usborn, breaks = 2)
# Now demogconf1517 has 4,009 obs of 16 variables

# DMDYRSUS
demogconf1517 %>% count(DMDYRSUS)
# There are 2,618 NA, 39 don't know and 33 refused -> set all to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(usyears = ifelse(DMDYRSUS == 77, NA, ifelse(DMDYRSUS == 99, NA, factor(DMDYRSUS))))
# Check that after factoring, codes are the same
demogconf1517 %>% count(usyears) # Now 2,690 NAs, and factors up to 9
hist(demogconf1517$usyears, breaks = 0:9)
# Now demogconf1517 has 4,009 obs of 17 variables

# DMDEDUC2
demogconf1517 %>% count(DMDEDUC2)
hist(demogconf1517$DMDEDUC2, breaks = 0:9)
# Set don't know (9) and refused (7) to NA
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
demogconf1517 %>% count(educ) # 883 less than HS, 981 HS grad, 1,175 some college, 968 college +, 2 NA
hist(demogconf1517$educ, breaks = 0:4)
# Now demogconf1517 has 4,009 obs of 18 variables

# DMDMARTL
demogconf1517 %>% count(DMDMARTL)
# There are 2 refused (77) -> set to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(marital = ifelse(DMDMARTL == 77, NA, ifelse(DMDMARTL == 99, NA, factor(DMDMARTL))))
# Check that after factoring, codes are the same
demogconf1517 %>% count(marital)
hist(demogconf1517$marital, breaks = 0:6)
## There are few-ish unmarried people -> combine married & living with partner (1&6), and living alone (2-5)
demogconf1517 <- demogconf1517 %>% 
  mutate(marital = ifelse(marital == 6, 1, marital)) %>% 
  mutate(marital = ifelse(marital >1, 2, marital))
# Check numbers
demogconf1517 %>% count(marital) # 2,542 living with partner, 1,645 living without partner
hist(demogconf1517$marital, breaks = 0:2)
# Now demogconf1517 has 4,009 obs of 19 variables

# # RIDEXPRG - decided to only look at men
# demogconf1517 %>% count(RIDEXPRG)
# # There are 182 unsure -> set to NA
# demogconf1517 <- demogconf1517 %>% 
#   mutate(pregnancy = ifelse(RIDEXPRG == 3, NA, factor(RIDEXPRG)))
# # Check that after factoring, codes are the same
# demogconf1517 %>% count(pregnancy)
# hist(demogconf1517$pregnancy, breaks = 0:2)
demogconf1517 <- demogconf1517 %>% select(-RIDEXPRG)
# Now demogconf1517 has 4,009 obs of 18 variables

# DMDHHSIZ
demogconf1517 %>% count(DMDHHSIZ)
hist(demogconf1517$DMDHHSIZ, breaks = 0:7)
# Check that there are no missing values
which(is.na(demogconf1517$DMDHHSIZ))
# Confirmed that there is no missing data for household size in NHANES codebooks
# Create new variable for household size
demogconf1517 <- demogconf1517 %>%
  mutate(household = DMDHHSIZ)
hist(demogconf1517$household, breaks = 0:7)
## Combine households with 6 & 7+ to make bigger cells
demogconf1517 <- demogconf1517 %>% 
  mutate(household = ifelse(household == 7, 6, household))
# Check numbers
demogconf1517 %>% count(household)
hist(demogconf1517$household, breaks = 0:6) # HH size: 444-1, 962-2, 789-3, 764-4,553-5, 497-6+
# Now demogconf1517 has 4,009 obs of 19 variables

# INDHHIN2
demogconf1517 %>% count(INDHHIN2)
# There are 201 NA, 81 don't know (99) and 79 refused (77) -> set all to NA
demogconf1517 <- demogconf1517 %>% 
  mutate(income = ifelse(INDHHIN2 == 77, NA, ifelse(INDHHIN2 == 99, NA, INDHHIN2)))
# Check that after factoring, codes are the same
demogconf1517 %>% count(income)
hist(demogconf1517$income, breaks = 0:15)
## Household income is very confusing -> makes sense to collapse and combine cells
# Less than $20k is 1, $20k-$75k is 2, $75k and more is 3
demogconf1517 <- demogconf1517 %>% 
  mutate(income = ifelse(income < 5, 1, ifelse(income < 13, 2, ifelse(income == 13, 1, 3))))
# Check numbers
demogconf1517 %>% count(income) # 592 under $20k (1), 1,874 are $20-74.9k (2), 1,182 over $75k (3), 361 NA
hist(demogconf1517$income, breaks = 0:3)

### BMI confounders

## Should add BMI, smoking status, alcohol consumption, depression, snoring/apnea
#####
## Create table with covariates
cov1517 <- dplyr::select(demogconf1517, SEQN, gender, age, raceeth, usborn, usyears, educ, marital, 
                         household, income)
summary(cov1517)
write_csv(slpex, "cov1517.csv")

# Merge slpex and confounders
slpexconf <- merge(slpex, cov1517, by = 'SEQN')
summary(slpexconf)
head(slpexconf)
tail(slpexconf)
write_csv(slpexconf, "slpexconf.csv")

#####
