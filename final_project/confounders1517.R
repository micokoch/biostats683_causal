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
  mutate(income = ifelse(INDHHIN2 == 77, NA, ifelse(INDHHIN2 == 99, NA, factor(INDHHIN2))))
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
# Now demogconf1517 has 4,009 obs of 20 variables

## Create table with demographic covariates
demogcov1517 <- dplyr::select(demogconf1517, SEQN, gender, males, age, raceeth, usborn, usyears, educ,
                         marital, household, income)
summary(demogcov1517) # 4,009 obs of 11 variables
# NAs: 2,690 in usyears, 2 in educ, 2 in marital, 361 in income

# Merge with slpex and demographic confounders
slpexdem <- merge(slpex, demogcov1517, by = 'SEQN') # 3,981 obs of 17 variables
summary(slpexdem)
head(slpexdem)
tail(slpexdem)

### Sleep confounders
# 2015
sleepcov2015 <- nhanes('SLQ_I') %>% 
  dplyr::select(c('SEQN', 'SLQ030', 'SLQ040'))
# 2017
sleepcov2017 <- nhanes('SLQ_J') %>% 
  dplyr::select(c('SEQN', 'SLQ030', 'SLQ040'))

# Combine years
sleepcov1517 <- bind_rows(sleepcov2015, sleepcov2017) # 12,488 obs of 3 variables
# Merge with slpex
slpexcov <- merge(slpexdem, sleepcov1517, by = 'SEQN') # 3,981 obs of 19 variables
summary(slpexcov)

# SLQ030 - Snoring
slpexcov %>% count(SLQ030)
hist(slpexcov$SLQ030, breaks = 0:9)
# There are 214 don't know (9) and 3 refused (7) -> set all to NA
slpexcov <- slpexcov %>% 
  mutate(snoring = ifelse(SLQ030 == 7, NA, ifelse(SLQ030 == 9, NA, factor(SLQ030))))
# Check numbers after factoring
slpexcov %>% count(snoring)
# 809 - no snoring, 942 - 1-2 times/wk, 736 - 3-4 times/wk, 1,277 - 5-7 times/wk, 217 NA
hist(slpexcov$snoring, breaks = 0:4)
# Remove SLQ030
slpexcov <- slpexcov %>% select(-SLQ030)
# Now slpexcov has 3,981 obs of 19 variables

# SLQ040 - Apnea
slpexcov %>% count(SLQ040)
hist(slpexcov$SLQ040, breaks = 0:9)
# There are 195 don't know (9) and 4 refused (7) -> set all to NA
slpexcov <- slpexcov %>% 
  mutate(apnea = ifelse(SLQ040 == 7, NA, ifelse(SLQ040 == 9, NA, factor(SLQ040))))
# Check numbers after factoring
slpexcov %>% count(apnea)
# 2,641 - no apnea, 589 - 1-2 times/wk, 304 - 3-4 times/wk, 248 - 5-7 times/wk, 199 NA
hist(slpexcov$apnea, breaks = 0:4)
# Not very well distributed - combine all "yes" answers
slpexcov <- slpexcov %>% mutate(apnea = ifelse(apnea == 1, 0, 1))
# Check numbers after factoring
slpexcov %>% count(apnea)
# 2,641 - no apnea, 1,141 - apnea, 199 NA
hist(slpexcov$apnea, breaks = -1:1)
# Remove SLQ040
slpexcov <- slpexcov %>% select(-SLQ040)
# Now slpexcov has 3,981 obs of 19 variables

### Weight confounders
# Using BMI and waist circumference (WC)
# Didn't use sagittal abdominal diameter because more missing values than WC
# 2015
bmicov2015 <- nhanes('BMX_I') %>%
  dplyr::select(c('SEQN', 'BMXBMI', 'BMXWAIST'))
# 2017
bmicov2017 <- nhanes('BMX_J') %>%
  dplyr::select(c('SEQN', 'BMXBMI', 'BMXWAIST'))

# Combine years
wtcov1517 <- bind_rows(bmicov2015, bmicov2017) # 18,248 obs of 3 variables
# Merge with slpex
slpexcov <- merge(slpexcov, wtcov1517, by = 'SEQN') # 3,792 obs of 21 variables
summary(slpexcov)

# BMXBMI - BMI
summary(slpexcov$BMXBMI)
hist(slpexcov$BMXBMI)
# Range: 15.10 - 86.20, median: 28.30, NA: 50
slpexcov <- slpexcov %>% mutate(bmi = BMXBMI) %>% select(-BMXBMI)
summary(slpexcov)
# Now slpexcov has 3,792 obs of 21 variables

# BMXWAIST - Waist Cirucmference
summary(slpexcov$BMXWAIST)
hist(slpexcov$BMXWAIST)
# Range: 62.3 - 169.6, median: 99.2, NA: 159
slpexcov <- slpexcov %>% mutate(waist = BMXWAIST) %>% select(-BMXWAIST)
summary(slpexcov)
# Now slpexcov has 3,792 obs of 21 variables

### Smoking confounders
# Decided to use SMQ020 - Smoked at least 100 cigarettes in life
# It was the question with the smallest number of missing values (in others >50%)
# 2015
smoke2015 <- nhanes('SMQ_I') %>%
  dplyr::select(c('SEQN', 'SMQ020'))
# 2017
smoke2017 <- nhanes('SMQ_J') %>%
  dplyr::select(c('SEQN', 'SMQ020'))

# Combine years
smoke1517 <- bind_rows(smoke2015, smoke2017) # 13,725 obs of 2 variables
# Merge with slpex
slpexcov <- merge(slpexcov, smoke1517, by = 'SEQN') # 3,792 obs of 22 variables
summary(slpexcov)

# SMQ020 - Smoked at least 100 cigarettes in life
summary(slpexcov$SMQ020)
table(slpexcov$SMQ020) # 1,848 yes (1), 1,942 no (2), 2 refused (7)
# Set don't know (9) and refused (7) to NA
slpexcov <- slpexcov %>% 
  mutate(smoke = ifelse(SMQ020 == 7, NA, ifelse(SMQ020 == 9, NA, factor(SMQ020)))) %>% 
  mutate(smoke = ifelse(smoke == 2, smoke - 2, smoke)) %>% 
  select(-SMQ020)
# Check that after factoring, codes are the same
slpexcov %>% count(smoke) # 1,942 no (0), 1,848 yes (1), 2 refused (NA)
hist(slpexcov$smoke, breaks = -1:1)
summary(slpexcov)
# Now slpexcov has 3,792 obs of 22 variables

### Alcohol confounders
# Decided to use ALQ130 - Avg # alcoholic drinks/day - past 12 mos
# Questions phrased differently in 2015 and 2017
# 2015
alcohol2015 <- nhanes('ALQ_I') %>%
  dplyr::select(c('SEQN', 'ALQ101', 'ALQ110', 'ALQ130'))
summary(alcohol2015)
table(alcohol2015$ALQ130, useNA = "always") # 2,356 in NA, 4 in 999, 1,164 in 1, range 1:15
# Large numbers of NAs in ALQ130 - try to fix
alcohol2015 <- alcohol2015 %>% 
  mutate(alcohol = 
           case_when(
             ALQ101 == 2 ~ as.integer(0), # If didn't have at least 12 alcohol drinks/1 yr - set to zero
             ALQ110 == 2 ~ as.integer(0), # If didn't have at least 12 alcohol drinks/lifetime? - set to zero
             TRUE ~ as.integer(ALQ130) # For rest keep ALQ130
           )) %>% 
  mutate(alcohol = ifelse(alcohol == 777, NA, ifelse(alcohol == 999, NA, alcohol))) # Set refused & don't know to NA
table(alcohol2015$alcohol, useNA = "always") # 1,025 in NA, 1,728 in 0, range 1:15
hist(alcohol2015$alcohol, breaks = -1:15)
# Very skewed values with most in zero -> reduce categories to 0-4+
alcohol2015 <- alcohol2015 %>% 
  mutate(alcohol = ifelse(alcohol > 4, 4, alcohol))
table(alcohol2015$alcohol, useNA = "always") # 1,025 in NA, 1,728 in 0, 883 in 1, 895 in 2, 459 in 3, 745 in 4+
hist(alcohol2015$alcohol, breaks = -1:4)
# Now remove unnecessary variables from alcohol
alcohol2015 <- alcohol2015 %>% select(-ALQ101, -ALQ110, -ALQ130)
summary(alcohol2015) # 5,735 obs of 2 variables
# 2017
alcohol2017 <- nhanes('ALQ_J') %>%
  dplyr::select(c('SEQN', 'ALQ111', 'ALQ121', 'ALQ130'))
summary(alcohol2017)
table(alcohol2017$ALQ130, useNA = "always") # 2,038 in NA, 5 in 999, 1 in 777, 1,317 in 1, range 1:15
# Large numbers of NAs in ALQ130 - try to fix
alcohol2017 <- alcohol2017 %>% 
  mutate(alcohol = 
           case_when(
             ALQ111 == 2 ~ as.integer(0), # If never had a drink of any kind of alcohol - set to zero
             ALQ121 == 0 ~ as.integer(0), # If never had a drink in last year - set to zero
             TRUE ~ as.integer(ALQ130) # For rest keep ALQ130
           )) %>% 
  mutate(alcohol = ifelse(alcohol == 777, NA, ifelse(alcohol == 999, NA, alcohol))) # Set refused & don't know to NA
table(alcohol2017$alcohol, useNA = "always") # 410 in NA, 1,634 in 0, 1,317 in 1, range 1:15
hist(alcohol2017$alcohol, breaks = -1:15)
# Very skewed values with most in zero -> reduce categories to 0-4+
alcohol2017 <- alcohol2017 %>% 
  mutate(alcohol = ifelse(alcohol > 4, 4, alcohol))
table(alcohol2017$alcohol, useNA = "always") # 410 in NA, 1,634 in 0, 1,317 in 1, 1,040 in 2, 479 in 3, 653 in 4+
hist(alcohol2017$alcohol, breaks = -1:4)
# Now remove unnecessary variables from alcohol
alcohol2017 <- alcohol2017 %>% select(-ALQ111, -ALQ121, -ALQ130)
summary(alcohol2017) # 5,533 obs of 2 variables
# Combine years
alcohol1517 <- bind_rows(alcohol2015, alcohol2017) # 11,268 obs of 2 variables
# Merge with slpex
slpexcov <- merge(slpexcov, alcohol1517, by = 'SEQN') # 3,792 obs of 23 variables
summary(slpexcov)
# Now slpexcov has 3,792 obs of 23 variables

### Depression confounders
# Use PHQ-9 (sum first 9 questions) and dicotimize with 10 or more being depression
# 2015
depress2015 <- nhanes('DPQ_I')
# 2017
depress2017 <- nhanes('DPQ_J')

# Combine years
depress1517 <- bind_rows(depress2015, depress2017) # 11,268 obs of 11 variables
# Merge with slpex
slpexcov <- merge(slpexcov, depress1517, by = 'SEQN') # 3,792 obs of 33 variables
summary(slpexcov)
# Remove DPQ100, since we won't use it for PHQ-9
slpexcov <- select(slpexcov, -DPQ100)  # 3,792 obs of 32 variables
summary(slpexcov)

# PHQ-9 - Patient Health Questionnaire
# Subset dataframe to PHQ questions for ease of use
phq <- select(slpexcov, SEQN, DPQ010:DPQ090) # 3,792 obs of 10 variables
summary(phq)
# Check distribution of unique values
phq %>% 
  select(-SEQN) %>% 
  sapply(table) # 1-5 people refused (7) or didn't know (9) in each question
# Subset rows that have NAs
deprnas <- phq[!complete.cases(phq), ]
deprnas %>% summary() # DPQ010 - DPQ030 have some zeros
# Most are all NAs, but some, like 86784 have some zeros and NAs
# Set all values to NA
deprnas[deprnas == 0] <- NA
deprnas %>% summary() # All values are now set to NA in deprnas
# Subset unique responses of 7 or 9 across questions
temp <- deprincomp <- na.omit(deprnas) # Use last set to create an empty subset with same column names
for(i in 2:10){
  temp <- filter(phq, (phq[, i] == 7 | phq[, i] == 9))
  deprincomp <- bind_rows(deprincomp, temp) %>% distinct(SEQN, .keep_all = T)
}
deprincomp
# Set all participants with 7 or 9 in any question to NA
for(i in seq_len(nrow(deprincomp))){
  for(j in 2:10){
    deprincomp[i, j] <- NA
  }
}
deprincomp
# Combine deprincomp and deprnas
nadepr <- bind_rows(deprincomp, deprnas)
head(nadepr, 20)
nadepr %>% summary()
# Create vector with list of SEQN that (should) have NAs
nadepr.v <- nadepr$SEQN
# Update phq so that it has NAs in desired places
for(i in seq_along(nadepr.v)){
  if(nadepr.v[i] %in% phq$SEQN){
    q <- which(phq$SEQN == nadepr.v[i])
    for(j in 2:10){
      phq[q, j] <- nadepr[i, j]
    }
  }
}
# Check that change was done correctly
head(phq, 20)
phq %>% filter(is.na(DPQ030)) %>% summary()
# Add values of DPQ for PHQ-9 score
phq <- phq %>% mutate(phq9 = rowSums(phq[,2:10]))
head(phq, 20)
hist(phq$phq9)
table(phq$phq9)
# Create Depression indicator if phq9 is 10 or more
phq <- phq %>% mutate(depressed = ifelse(phq9 > 9, 1, ifelse(phq9 < 10, 0, NA)))
head(phq, 20)
hist(phq$depressed, breaks = 2)
table(phq$depressed) # 3,241 depressed, and 250 not depressed - possible positivity violations
# Subset phq to only include sum and indicator
phq <- phq %>% select(SEQN, phq9, depressed)
summary(phq)
# Merge with slpexcov and remove individual DPQ question scores
slpexcov <- merge(slpexcov, phq, by = 'SEQN') %>% 
  select(-DPQ010, -DPQ020, -DPQ030, -DPQ040, -DPQ050, -DPQ060, -DPQ070, -DPQ080, -DPQ090)
summary(slpexcov) # 3,792 obs of 25 variables

### Review how many variables I actually imputed
slpexcov %>% filter(SEQN %in% imputed)
# Currently there are only 10 imputed values

#### Final steps for table
# After discussion with team, decided to drop some variables
# Create table with agreed upon variables
slpexcov1517 <- dplyr::select(slpexcov, SEQN, exminwk, targetex, slphrs, targetslp, age, raceeth, educ, marital, 
                         household, income, snoring, apnea, bmi, waist, smoke, alcohol, phq9, depressed)
summary(slpexcov1517)
write_csv(slpexcov1517, "slpexcov1517.csv")

#####
