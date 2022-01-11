## Using complete dataset

# Libraries
library(tidyverse)
library(nhanesA)
library(survey)
# library(gtools)

# Preliminaries
setwd("final_project")
getwd()
set.seed(252)

##### CREATE DATASET
# Download variables from NHANES
# As per NHANES guidelines, use wtmec2yr since we're using BMI
# 2015
demog2015 <- nhanes('DEMO_I') %>% 
  dplyr::select(c('SEQN', 'RIAGENDR', 'RIDAGEYR', 'RIDRETH1', 'DMDBORN4', 'DMDYRSUS', 'DMDEDUC2',
                  'DMDMARTL', 'RIDEXPRG', 'DMDHHSIZ', 'INDHHIN2', 'WTINT2YR', 'WTMEC2YR', 
                  'SDMVPSU', 'SDMVSTRA'))
# 9,971 obs of 15 variables
exercise2015 <- nhanes('PAQ_I') %>% 
  dplyr::select(c('SEQN', 'PAQ650', 'PAQ655', 'PAD660', 'PAQ665', 'PAQ670', 'PAD675'))
# 9,255 of 7 variables
sleep2015 <- nhanes('SLQ_I') %>% 
  dplyr::select(c('SEQN', 'SLD012', 'SLQ030', 'SLQ040'))
# 6,327 obs of 4 variables
bmicov2015 <- nhanes('BMX_I') %>%
  dplyr::select(c('SEQN', 'BMXBMI', 'BMXWAIST'))
# 9,544 obs of 3 variables
smoke2015 <- nhanes('SMQ_I') %>%
  dplyr::select(c('SEQN', 'SMQ020'))
# 7,001 of 2 variables
alcohol2015 <- nhanes('ALQ_I') %>%
  dplyr::select(c('SEQN', 'ALQ101', 'ALQ110', 'ALQ130'))
# 5,735 obs of 4 variables
depress2015 <- nhanes('DPQ_I')
# 5,735 obs of 11 variables
# Combine all 2015 variables into one big dataset
full_exslp_15 <- merge(demog2015, exercise2015, by = 'SEQN') %>% 
  merge(sleep2015, by = 'SEQN') %>% 
  merge(bmicov2015, by = 'SEQN') %>% 
  merge(smoke2015, by = 'SEQN') %>% 
  merge(alcohol2015, by = 'SEQN') %>% 
  merge(depress2015, by = 'SEQN')
# 5,735 obs of 40 variables
# Must format variables correctly for binding
full_exslp_15 <- as_tibble(lapply(full_exslp_15[], as.numeric))

# 2017
demog2017 <- nhanes('DEMO_J') %>% 
  dplyr::select(c('SEQN', 'RIAGENDR', 'RIDAGEYR', 'RIDRETH1', 'DMDBORN4', 'DMDYRSUS', 'DMDEDUC2',
                  'DMDMARTL', 'RIDEXPRG', 'DMDHHSIZ', 'INDHHIN2', 'WTINT2YR', 'WTMEC2YR', 
                  'SDMVPSU', 'SDMVSTRA'))
# 9,254 obs of 15 variables
exercise2017 <- nhanes('PAQ_J') %>% 
  dplyr::select(c('SEQN', 'PAQ650', 'PAQ655', 'PAD660', 'PAQ665', 'PAQ670', 'PAD675'))
# 5,856 obs of 7 variables
sleep2017 <- nhanes('SLQ_J') %>% 
  dplyr::select(c('SEQN', 'SLD012', 'SLQ030', 'SLQ040'))
# 6,161 obs of 4 variables
bmicov2017 <- nhanes('BMX_J') %>%
  dplyr::select(c('SEQN', 'BMXBMI', 'BMXWAIST'))
# 8,704 obs of 3 variables
smoke2017 <- nhanes('SMQ_J') %>%
  dplyr::select(c('SEQN', 'SMQ020'))
# 6,724 obs of 2 variables
alcohol2017 <- nhanes('ALQ_J') %>%
  dplyr::select(c('SEQN', 'ALQ111', 'ALQ121', 'ALQ130'))
# 5,533 obs of 4 variables
depress2017 <- nhanes('DPQ_J')
# 5,533 obs of 11 variables
# Combine all 2017 variables into one big dataset
full_exslp_17 <- merge(demog2017, exercise2017, by = 'SEQN') %>% 
  merge(sleep2017, by = 'SEQN') %>% 
  merge(bmicov2017, by = 'SEQN') %>% 
  merge(smoke2017, by = 'SEQN') %>% 
  merge(alcohol2017, by = 'SEQN') %>% 
  merge(depress2017, by = 'SEQN')
# 5,533 obs of 40 variables
# Must format variables correctly for binding
full_exslp_17 <- as_tibble(lapply(full_exslp_17[], as.numeric))

# Combine 2015 and 2017 variables to make large dataset
full_exslp_1517 <- bind_rows(full_exslp_15, full_exslp_17)
# 11,268 obs of 42 variables (no overlap in some alcohol variables)
summary(full_exslp_1517)
# Write to csv file
write.csv(full_exslp_1517, "full_exslp_1517.csv")

##### VARIABLE CLEANING
##### Read in variables
full_exslp_1517 <- read_csv("full_exslp_1517.csv")
clean_exslp_1517 <- full_exslp_1517
# A new column was added that indexes all participants

##### EXERCISE
### Rename exercise variables
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(
    vigex = PAQ650,
    daysvigex = PAQ655,
    minvigex = PAD660,
    modex = PAQ665,
    daysmodex = PAQ670,
    minmodex = PAD675
  ) %>% 
  select(-PAQ650, -PAQ655, -PAD660, -PAQ665, -PAQ670, -PAD675)
names(clean_exslp_1517)

### Vigorous exercise
# Look at responses for vigorous exercise
clean_exslp_1517 %>% 
  group_by(vigex) %>% 
  summarise(n = n()) # 2,830 are 1, 8,437 are 2, and 1 is 9
table(clean_exslp_1517$vigex, useNA = "always") # No NAs
# Change to NA respondents who didn't know or refused to answer q on vigorous exercise
# Also, set to zero participants who didn't do vigorous exercise
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(vigex = ifelse(vigex == 7, NA, 
                         ifelse(vigex == 9, NA, 
                                ifelse(vigex == 2, 0, vigex))))
table(clean_exslp_1517$vigex, useNA = "always")
# 8437 are 0, 2830 are 1, and 1 is NA
# If participant wrote No to vigorous exercise, set vigorous days and minutes to zero
table(clean_exslp_1517$daysvigex, useNA = "always")
# Only 1 in 99 (days 1:7 - 354 554 774 487 398 137 125), 8,438 NAs
clean_exslp_1517$daysvigex[clean_exslp_1517$vigex == 0] <- 0
table(clean_exslp_1517$daysvigex, useNA = 'always')
# 8,437 new zeros (0), 1 in 99, 1 in NA
table(clean_exslp_1517$minvigex, useNA = "always")
# Only 2 in 9999 (minutes 10:480), 8,445 NAs
clean_exslp_1517$minvigex[clean_exslp_1517$vigex == 0] <- 0
table(clean_exslp_1517$minvigex, useNA = "always")
# 8,437 new zeros, 2 in 9999, 8 NAs
summary(clean_exslp_1517[,38:43]) # One NA for vigex and daysvigex and eight for minvigex
#      vigex        daysvigex           minvigex          modex        daysmodex      minmodex 
# Min.   :0.000   Min.   : 0.0000   Min.   :   0.0   Min.   :1.0   Min.   : 1.000   Min.   :  10.00  
# 1st Qu.:0.000   1st Qu.: 0.0000   1st Qu.:   0.0   1st Qu.:1.0   1st Qu.: 2.000   1st Qu.:  30.00  
# Median :0.000   Median : 0.0000   Median :   0.0   Median :2.0   Median : 3.000   Median :  45.00  
# Mean   :0.2512  Mean   : 0.8448   Mean   :  20.7   Mean   :1.6   Mean   : 3.595   Mean   :  74.67  
# 3rd Qu.:1.000   3rd Qu.: 1.0000   3rd Qu.:  10.0   3rd Qu.:2.0   3rd Qu.: 5.000   3rd Qu.:  60.00  
# Max.   :1.000   Max.   :99.0000   Max.   :9999.0   Max.   :9.0   Max.   :99.000   Max.   :9999.00  
# NA's   :1       NA's   :1         NA's   :8                      NA's   :6728     NA's   :6742     

# Look at data that should be imputed
# vigex - Vigorous recreational activities (yes/no)
table(clean_exslp_1517$vigex, useNA = "always")
clean_exslp_1517 %>% filter(is.na(vigex))
# For vigex there is only one NA (SEQN == 92202)
paq650na <- clean_exslp_1517 %>% filter(SEQN == 92202)

# After reviewing lit, decided to use mice to impute data
# # Has incomplete data for vigorous exercise, but complete for moderate. Set values to 0
# clean_exslp_1517$vigex[clean_exslp_1517$SEQN == 92202] <- 0
# clean_exslp_1517$daysvigex[clean_exslp_1517$SEQN == 92202] <- 0
# clean_exslp_1517$minvigex[clean_exslp_1517$SEQN == 92202] <- 0
# # Check changes done correctly
# paq650na <- clean_exslp_1517 %>% filter(SEQN == 92202)

summary(clean_exslp_1517[,38:40])
# No changes in mean values
#     vigex          daysvigex          minvigex      
# Min.   :0.000   Min.   : 0.0000   Min.   :   0.0  
# 1st Qu.:0.000   1st Qu.: 0.0000   1st Qu.:   0.0  
# Median :0.000   Median : 0.0000   Median :   0.0  
# Mean   :0.2512  Mean   : 0.8448   Mean   :  20.7  
# 3rd Qu.:1.000   3rd Qu.: 1.0000   3rd Qu.:  10.0  
# Max.   :1.000   Max.   :99.0000   Max.   :9999.0  
# NA's   :1       NA's   :1         NA's   :8     

table(clean_exslp_1517$vigex, useNA = "always")
# 8437 are 0, 2830 are 1, , 1 NA

#daysvigex - Days vigorous recreational activities
table(clean_exslp_1517$daysvigex, useNA = "always")
# One is 99, no NAs, (1 imputation), 8437 are 0, 774 are 3...
# Change missing values to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(daysvigex = ifelse(daysvigex > 10, NA, daysvigex))
summary(clean_exslp_1517$daysvigex) # Now 2 NAs, mean = 0.8361

# Keep track of imputations (to check final results)
imputed_exer <- c()
imp <- clean_exslp_1517 %>% filter(is.na(daysvigex)) %>% select(SEQN)
for(i in imp){
  imputed_exer <- append(imputed_exer, i)
}
imputed_exer # SEQN = 92202 95778

#minvigex - Minutes vigorous recreational
table(clean_exslp_1517$minvigex, useNA = "always")
# 2 are 9999, 8 NAs (10 imputations), 8437 are 0, 910 are 60..
# Change missing values to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(minvigex = ifelse(minvigex > 1000, NA, minvigex))
summary(clean_exslp_1517$minvigex) # Went from 8 to 10 NAs, mean = 18.93

# Keep track of imputations (to check final results)
imp <- clean_exslp_1517 %>% filter(is.na(minvigex)) %>% select(SEQN)
for(i in imp){
  imputed_exer <- append(imputed_exer, i)
}
imputed_exer <- unique(imputed_exer)
imputed_exer # Ten total elements
# SEQN = (92202 95778) 86404 88224 89043 89543 92417 97157 97445 98134
save(imputed_exer, file='imputed_exer.Rdata')

### Moderate exercise
# Look at responses for moderate exercise
clean_exslp_1517 %>% 
  group_by(modex) %>% 
  summarise(n = n()) # 4540 are 1, 6724 are 2, and 4 are 9
table(clean_exslp_1517$modex, useNA = "always") # No NAs
# Change to NA respondents who didn't know or refused to answer q on moderate exercise
# Also, set to zero participants who didn't do vigorous exercise
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(modex = ifelse(modex == 7, NA, 
                         ifelse(modex == 9, NA, 
                                ifelse(modex == 2, 0, modex))))
table(clean_exslp_1517$modex, useNA = "always")
# 6724 are 2, 4540 are 1, and 4 are NA
# If participant wrote No to moderate exercise, set vigorous days and minutes to zero
table(clean_exslp_1517$daysmodex, useNA = "always")
# Four in 99 (days 1:7 - 512  992 1139  599  634  173  487), 6728 NAs
clean_exslp_1517$daysmodex[clean_exslp_1517$modex == 0] <- 0
table(clean_exslp_1517$daysmodex, useNA = "always")
# 6724 new zeros (0), four in 99, four NAs
table(clean_exslp_1517$minmodex, useNA = "always")
# Five in 9999 (minutes 10:600), 6742 NAs
clean_exslp_1517$minmodex[clean_exslp_1517$modex == 0] <- 0
table(clean_exslp_1517$minmodex, useNA = "always")
# 6724 new zeros, five in 9999, 18 NAs
summary(clean_exslp_1517[,41:43]) # Four NAs for modex and daysmodex and 18 for minmodex
#      modex          daysmodex           minmodex       
# Min.   :0.000   Min.   : 0.000   Min.   :   0.00  
# 1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:   0.00  
# Median :0.000   Median : 0.000   Median :   0.00  
# Mean   :0.4031  Mean   : 1.449   Mean   :  30.04  
# 3rd Qu.:1.000   3rd Qu.: 3.000   3rd Qu.:  30.00  
# Max.   :1.000   Max.   :99.000   Max.   :9999.00  
# NA's   :4       NA's   :4        NA's   :18 

# Look at data that should be imputed
# modex - Moderate recreational activities (yes/no)
table(clean_exslp_1517$modex, useNA = "always")
# For modex there are four NAs
temp <- clean_exslp_1517 %>% filter(is.na(modex))
# Three have complete data on vigorous exercise (86537 88264 89806) with no vigorous exercise 
# Missing days and minutes of moderate exercise -> decided to use mice
paq665na_0 <- temp$SEQN[2:4] # 86537 88264 89806
# One (86404) has data for vigorous exercise - 7 days, but no minutes -> use mice
paq665na_1 <- temp$SEQN[1] # 86404

# Decided to impute these with mice
# # Change modex to 1 and later impute missing values
# # Set paq665na_0 participants to no exercise
# for(i in 1:nrow(clean_exslp_1517)){
#   if(clean_exslp_1517[i, "SEQN"] %in% paq665na_0){
#     clean_exslp_1517[i, "modex"] <- 0
#     clean_exslp_1517[i, "daysmodex"] <- 0
#     clean_exslp_1517[i, "minmodex"] <- 0
#   }}
# temp <- clean_exslp_1517 %>% filter(SEQN %in% paq665na_0) # Correct
# # Set paq665na_1 to yes exercise (but no days or minutes)
# clean_exslp_1517$modex[clean_exslp_1517$SEQN == paq665na_1] <- 1
# temp <- clean_exslp_1517 %>% filter(SEQN %in% paq665na_1) # Correct

# Check table
summary(clean_exslp_1517[,41:43])
#      modex          daysmodex           minmodex       
# Min.   :0.000   Min.   : 0.000   Min.   :   0.00  
# 1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:   0.00  
# Median :0.000   Median : 0.000   Median :   0.00  
# Mean   :0.4031  Mean   : 1.449   Mean   :  30.04  
# 3rd Qu.:1.000   3rd Qu.: 3.000   3rd Qu.:  30.00  
# Max.   :1.000   Max.   :99.000   Max.   :9999.00  
# NA's   :4       NA's   :4        NA's   :18   
# No changes in mean values
table(clean_exslp_1517$modex, useNA = "always")
# 6724 are 0, 4540 are 1, , 4 NA

#daysmodex - Days moderate recreational activities
table(clean_exslp_1517$daysmodex, useNA = "always")
# Four are 99, four are NA, (5 imputations), 6724 are 0, 1139 are 3...
# Change missing values to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(daysmodex = ifelse(daysmodex > 10, NA, daysmodex))
summary(clean_exslp_1517$daysmodex) # Now eight NAs, mean = 1.414

# Keep track of imputations (to check final results)
imp <- clean_exslp_1517 %>% filter(is.na(daysmodex)) %>% select(SEQN)
for(i in imp){
  imputed_exer <- append(imputed_exer, i)
}
imputed_exer <- unique(imputed_exer)
imputed_exer
# 17 total elements, 7 new ones: 86537  88264  89806  91394  94019 101043 102534

#minmodex - Minutes moderate recreational activities
table(clean_exslp_1517$minmodex, useNA = "always")
# 5 are 9999, 18 NAs (23 imputations), 6724 are 0, 1254 are 60..
# Change missing values to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(minmodex = ifelse(minmodex > 1000, NA, minmodex))
summary(clean_exslp_1517$minmodex) # Went from 18 to 23 NAs, mean = 25.61

# Keep track of imputations (to check final results)
imp <- clean_exslp_1517 %>% filter(is.na(minmodex)) %>% select(SEQN)
for(i in imp){
  imputed_exer <- append(imputed_exer, i)
}
imputed_exer <- unique(imputed_exer)
imputed_exer # 27 total elements, 10 new ones:
# 83986  88348  88830  89159  91377  92357  92924  94506  97054 101252
save(imputed_exer, file='imputed_exer.Rdata')

# Check cleaned exercise results
summary(clean_exslp_1517[,38:43])
#      vigex        daysvigex       minvigex           modex          daysmodex       minmodex
# Min.   :0.000   Min.   :0.000   Min.   :  0.00   Min.   :0.000   Min.   :0.000   Min.   :  0.0  
# 1st Qu.:0.000   1st Qu.:0.000   1st Qu.:  0.00   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:  0.0  
# Median :0.000   Median :0.000   Median :  0.00   Median :0.000   Median :0.000   Median :  0.0  
# Mean   :0.2512  Mean   :0.8361  Mean   : 18.93   Mean   :0.4031  Mean   :1.414   Mean   : 25.61  
# 3rd Qu.:1.000   3rd Qu.:1.000   3rd Qu.: 10.00   3rd Qu.:1.000   3rd Qu.:3.000   3rd Qu.: 30.0  
# Max.   :1.000   Max.   :7.000   Max.   :480.00   Max.   :1.000   Max.   :7.000   Max.   :600.0  
# NA's   :1       NA's   :2       NA's   :10       NA's   :4       NA's   :8       NA's   :23 


##### SLEEP
### Rename sleep variables
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(
    slphrs = SLD012,
    snoring = SLQ030,
    apnea = SLQ040
  ) %>% 
  select(-SLD012, -SLQ030, -SLQ040)
names(clean_exslp_1517)

# First look at variables carefully
summary(clean_exslp_1517[41:43])
#      slphrs           snoring          apnea      
# Min.   : 2.000   Min.   :0.000   Min.   :0.0000  
# 1st Qu.: 7.000   1st Qu.:0.000   1st Qu.:0.0000  
# Median : 8.000   Median :2.000   Median :0.0000  
# Mean   : 7.679   Mean   :2.031   Mean   :0.9253  
# 3rd Qu.: 8.500   3rd Qu.:3.000   3rd Qu.:1.0000  
# Max.   :14.500   Max.   :9.000   Max.   :9.0000  
# NA's   :72       

# slphrs - Sleep hours
table(clean_exslp_1517$slphrs, useNA = 'always')
hist(clean_exslp_1517$slphrs)
# Range 2 - 14.5, 72 NAs - 2015 and 2017 values slightly different -> correct
# In 2017, no 2.5, as 2 means less than 3, and nothing greater than 14
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(slphrs = case_when(
    slphrs == 2.5 ~ 2,
    slphrs == 14.5 ~ 14,
    TRUE ~ slphrs
  ))
table(clean_exslp_1517$slphrs, useNA = 'always') # Correct changes

# snoring - Snoring
clean_exslp_1517 %>% count(snoring)
# 0 - never, 1 - 1-2/wk, 2 - 3-4/wk, 3 - >5/wk, 7|9 - refuse/don't know
table(clean_exslp_1517$snoring, useNA = 'always')
#    0    1    2    3    7    9 <NA> 
# 2938 2577 1996 2913    9  835    0 
hist(clean_exslp_1517$snoring, breaks = -1:9)
# There are 835 don't know (9) and 9 refused (7) -> set all to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(snoring = ifelse(snoring == 7, NA, ifelse(snoring == 9, NA, snoring)))
# Check numbers after changes
clean_exslp_1517 %>% count(snoring)
# 2938 - no snoring, 2577 - 1-2 times/wk, 1996 - 3-4 times/wk, 2913 - 5-7 times/wk, 844 NA
hist(clean_exslp_1517$snoring, breaks = -1:3)
# Now clean_exslp_1517 has 11,268 obs of 43 variables

# apnea - Apnea
clean_exslp_1517 %>% count(apnea)
# 0 - never, 1 - 1-2/wk, 2 - 3-4/wk, 3 - >5/wk, 7|9 - refuse/don't know
table(clean_exslp_1517$apnea, useNA = 'always')
#    0    1    2    3    7    9 <NA> 
# 8016 1356  676  542    5  673    0 
hist(clean_exslp_1517$apnea, breaks = 0:9)
# There are 673 don't know (9) and 5 refused (7) -> set all to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(apnea = ifelse(apnea == 7, NA, ifelse(apnea == 9, NA, factor(apnea))))
# Check numbers after factoring - pushed up factors from 1-4
clean_exslp_1517 %>% count(apnea)
# 8016: no apnea (1), 1356: 1-2 times/wk (2), 676: 3-4 times/wk (3), 542: 5-7 times/wk (4), 678: NA
hist(clean_exslp_1517$apnea, breaks = 0:4)
# Not very well distributed - combine all "yes" answers
clean_exslp_1517 <- clean_exslp_1517 %>% mutate(apnea = ifelse(apnea == 1, 0, 1))
# Check numbers after factoring
clean_exslp_1517 %>% count(apnea)
# 8016 - no apnea (0), 2574 - apnea (1), 678 NA
hist(clean_exslp_1517$apnea, breaks = -1:1)
# Now clean_exslp_1517 has 11268 obs of 43 variables


##### DEMOGRAPHIC VARIABLES
### Rename sleep variables
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(
    gender = RIAGENDR,
    age = RIDAGEYR,
    raceeth = RIDRETH1,
    usborn = DMDBORN4,
    usyears = DMDYRSUS,
    educ = DMDEDUC2,
    marital = DMDMARTL,
    pregnancy = RIDEXPRG,
    household = DMDHHSIZ,
    income = INDHHIN2
  ) %>% 
  select(-RIAGENDR, -RIDAGEYR, -RIDRETH1, -DMDBORN4, -DMDYRSUS, -DMDEDUC2, -DMDMARTL, 
         -RIDEXPRG, -DMDHHSIZ, -INDHHIN2)
names(clean_exslp_1517)

# First look at variables carefully
summary(clean_exslp_1517[34:43])
# gender           age           raceeth          usborn          usyears            educ      
# Min.   :1.000   Min.   :18.00   Min.   :1.000   Min.   : 1.000   Min.   : 1.000   Min.   :1.000  
# 1st Qu.:1.000   1st Qu.:33.00   1st Qu.:2.000   1st Qu.: 1.000   1st Qu.: 4.000   1st Qu.:3.000  
# Median :2.000   Median :49.00   Median :3.000   Median : 1.000   Median : 6.000   Median :4.000  
# Mean   :1.518   Mean   :48.92   Mean   :3.148   Mean   : 1.351   Mean   : 9.201   Mean   :3.483  
# 3rd Qu.:2.000   3rd Qu.:64.00   3rd Qu.:4.000   3rd Qu.: 2.000   3rd Qu.: 7.000   3rd Qu.:4.000  
# Max.   :2.000   Max.   :80.00   Max.   :5.000   Max.   :99.000   Max.   :99.000   Max.   :9.000  
#                                                                  NA's   :7665     NA's   :529    

# marital         pregnancy       household         income     
# Min.   : 1.000   Min.   :1.000   Min.   :1.000   Min.   : 1.00  
# 1st Qu.: 1.000   1st Qu.:2.000   1st Qu.:2.000   1st Qu.: 6.00  
# Median : 1.000   Median :2.000   Median :3.000   Median : 8.00  
# Mean   : 2.646   Mean   :1.981   Mean   :3.248   Mean   :12.07  
# 3rd Qu.: 5.000   3rd Qu.:2.000   3rd Qu.:4.000   3rd Qu.:14.00  
# Max.   :77.000   Max.   :3.000   Max.   :7.000   Max.   :99.00  
# NA's   :529      NA's   :8971                    NA's   :475    


# gender
hist(clean_exslp_1517$gender, breaks = 2)
clean_exslp_1517 %>% count(gender) # 5431 males (1), 5837 females (2)
# Check that there are no missing values
which(is.na(clean_exslp_1517$gender)) # Confirmed that no missing data for gender in NHANES codebooks
# Now tibble has 11268 obs of 43 variables

# age
hist(clean_exslp_1517$age)
# Check that there are no missing values
which(is.na(clean_exslp_1517$age))
# Confirmed that there is no missing data for age in NHANES codebooks

# raceeth
hist(clean_exslp_1517$raceeth, breaks = 0:5)
clean_exslp_1517 %>% count(raceeth)
# 1770 Mex/Am (1), 1269 Hisp(2), 3737 White (3), 2510 Black (4), 1982 other (5)
# There are participants in each category
# Check that there are no missing values
which(is.na(clean_exslp_1517$raceeth))
# Confirmed that there is no missing data for race/ethnicity in NHANES codebooks
# Combine Mexican American and other Hispanic and factor race/eth, put White as reference
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(raceeth = ifelse(raceeth == 1, raceeth, raceeth - 1)) %>% 
  mutate(raceeth = ifelse(raceeth == 2, raceeth - 2, 
                          ifelse(raceeth == 1, raceeth + 1, raceeth))) %>% 
  mutate(raceeth = ifelse(raceeth == 0, raceeth + 1, raceeth)) %>% 
  mutate(raceeth = factor(raceeth))
# Check that after factoring, race/ethnicity codes are the same
clean_exslp_1517 %>% count(raceeth) # 3737 White, 3039 Hisp, 2510 Black, 1982 Other

# usborn
clean_exslp_1517 %>% count(usborn)
# 7661 are 1 - US  born, 3603 are 2 - other, 2 are 77 (refused), 2 are 99 (don't know)
# Set participants in refused & don't know to NA and factor
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(usborn = ifelse(usborn == 77, NA, ifelse(usborn == 99, NA, factor(usborn))))
clean_exslp_1517 %>% count(usborn)
# 7661 are 1, 3603 are 2, 4 are NA
# Check for missing values
which(is.na(clean_exslp_1517$usborn))
# 4 missing - index: 5066 7953 8787 8903
hist(clean_exslp_1517$usborn, breaks = 2)

# usyears
clean_exslp_1517 %>% count(usyears)
# There are 7665 NA, 102 don't know and 63 refused -> set all to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(usyears = ifelse(usyears == 77, NA, ifelse(usyears == 99, NA, factor(usyears))))
# Check that after factoring, codes are the same
clean_exslp_1517 %>% count(usyears) # Now 7830 NAs, and factors up to 9
hist(clean_exslp_1517$usyears, breaks = 0:9)

# educ
clean_exslp_1517 %>% count(educ)
# 1: 1109, 2: 1241, 3: 2437, 4: 3310, 5: 2627, 7: 2, 9: 13, NA: 529
hist(clean_exslp_1517$educ, breaks = 0:9)
# Set don't know (9) and refused (7) to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(educ = ifelse(educ == 7, NA, ifelse(educ == 9, NA, factor(educ))))
# Check that after factoring, codes are the same
clean_exslp_1517 %>% count(educ) # Correct, 15 added to NA
hist(clean_exslp_1517$educ, breaks = 0:5)
# There are few-ish people with education less than high school -> 
# Combine anyone with less than high school (1 & 2)
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(educ = ifelse(educ == 1, 2, educ)) %>% 
  mutate(educ = (educ - 1))
# Check numbers
clean_exslp_1517 %>% count(educ)
# 2350 less than HS, 2437 HS grad, 3310 some college, 2627 college +, 544 NA
hist(clean_exslp_1517$educ, breaks = 0:4)

# marital
clean_exslp_1517 %>% count(marital)
# There are 529 - NA, and 7 refused (77) -> set to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(marital = ifelse(marital == 77, NA, ifelse(marital == 99, NA, factor(marital))))
# Check that after factoring, codes are the same
clean_exslp_1517 %>% count(marital)
# 1: 5396, 2: 823, 3: 1176, 4: 380, 5: 1952, 6: 1005, NA: 536
hist(clean_exslp_1517$marital, breaks = 0:6)
# There are few-ish unmarried people ->
# Combine married & living with partner (1&6), and living alone (2-5)
# Put in increasing order, so that living alone is reference, and marriage is comparison
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(marital = ifelse(marital == 6, 1, marital)) %>% 
  mutate(marital = ifelse(marital >1, 2, marital)) %>% 
  mutate(marital = ifelse(marital == 2, marital - 2, marital)) %>% 
  mutate(marital = marital + 1)
# Check numbers
clean_exslp_1517 %>% count(marital)
# 4331 living without partner, 6401 living with partner, 536 NA
hist(clean_exslp_1517$marital, breaks = 0:2)

# pregnancy
clean_exslp_1517 %>% count(pregnancy)
# 8971 are NA, 125 pregnant(1), 2091 not (2), 81 unsure (3) -> set to NA
clean_exslp_1517 %>% select(c(gender, pregnancy)) %>% filter(gender == 1) %>% table(useNA = 'always')
# 5431 men with NA for pregnancy
# Set all men to 0, not pregnant to 0, unsure to 2
clean_exslp_1517 <- clean_exslp_1517 %>%
  mutate(pregnancy = ifelse(gender == 1, 0, 
                            ifelse(pregnancy == 2, 0, 
                                   ifelse(pregnancy == 3, pregnancy - 1, pregnancy))))
# Check results - 7522 is correct for zero, now change 3540 to NA
clean_exslp_1517 <- clean_exslp_1517 %>%
  mutate(pregnancy = ifelse(is.na(pregnancy), 2, pregnancy))
clean_exslp_1517 %>% count(pregnancy)
# Correct, now 7522 are 0, 125 are 1, 3621 are 3 (unsure)
hist(clean_exslp_1517$pregnancy, breaks = -1:2)

# household
clean_exslp_1517 %>% count(household)
# 1: 1527, 2: 3220, 3: 2057, 4: 1815, 5: 1319, 6: 704, 7: 626
hist(clean_exslp_1517$household, breaks = 0:7)
# Check that there are no missing values
which(is.na(clean_exslp_1517$household))
# Confirmed that there is no missing data for household size in NHANES codebooks
# Combine households with 6 & 7+ to make bigger cells
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(household = ifelse(household == 7, 6, household))
# Check numbers
clean_exslp_1517 %>% count(household)
# 1: 1527, 2: 3220, 3: 2057, 4: 1815, 5: 1319, 6: 1330
hist(clean_exslp_1517$household, breaks = 0:6)
# HH size: 1527-1, 3220-2, 2057-3, 1815-4, 1319-5, 1330-6+

# income
clean_exslp_1517 %>% count(income)
# There are 475 NA, 224 don't know (99) and 226 refused (77) -> set all to NA
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(income = ifelse(income == 77, NA, ifelse(income == 99, NA, factor(income))))
# Check that after factoring, codes are the same
clean_exslp_1517 %>% count(income)
# Now there are 925 NA - correct
hist(clean_exslp_1517$income, breaks = 0:15)
## Household income is very confusing -> makes sense to collapse and combine cells
# Less than $20k is 1, $20k-$75k is 2, $75k and more is 3
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(income = ifelse(income < 5, 1, ifelse(income < 13, 2, ifelse(income == 13, 1, 3))))
# Check numbers
clean_exslp_1517 %>% count(income)
# 2949 under $20k (1), 5518 are $20-74.9k (2), 1876 over $75k (3), 925 NA
hist(clean_exslp_1517$income, breaks = 0:3)


##### BODY SIZE VARIABLES
### Rename body size variables
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(
    bmi = BMXBMI,
    waist = BMXWAIST
  ) %>% 
  select(-BMXBMI, -BMXWAIST)
names(clean_exslp_1517)

# First look at variables carefully
summary(clean_exslp_1517[42:43])
# bmi            waist       
# Min.   :14.20   Min.   : 56.40  
# 1st Qu.:24.40   1st Qu.: 87.70  
# Median :28.40   Median : 98.60  
# Mean   :29.53   Mean   : 99.86  
# 3rd Qu.:33.30   3rd Qu.:110.03  
# Max.   :86.20   Max.   :171.60  
# NA's   :172     NA's   :716     

# bmi - BMI
summary(clean_exslp_1517$bmi)
# Range: 14.20 - 86.20, median: 29.53, NA: 172
hist(clean_exslp_1517$bmi)

# waist - Waist Cirucmference
summary(clean_exslp_1517$waist)
# Range: 56.40 - 171.60, median: 99.86, NA: 716
hist(clean_exslp_1517$waist)


##### SMOKING VARIABLES
# Decided to use SMQ020 - Smoked at least 100 cigarettes in life
# It was the question with the smallest number of missing values (in others >50%)
### Rename smoking variables
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(
    smoke = SMQ020
  ) %>% 
  select(-SMQ020)
names(clean_exslp_1517)

# First look at variables carefully
summary(clean_exslp_1517[43])
# smoke      
# Min.   :1.000  
# 1st Qu.:1.000  
# Median :2.000  
# Mean   :1.602  
# 3rd Qu.:2.000  
# Max.   :9.000  

# smoke - Smoked at least 100 cigarettes in life
summary(clean_exslp_1517$smoke)
table(clean_exslp_1517$smoke)
# 4551 yes (1), 6707 no (2), 2 refused (7), 8 don't know (9)
# Set don't know (9) and refused (7) to NA, and set no to 0, and yes to 1
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(smoke = ifelse(smoke == 7, NA, ifelse(smoke == 9, NA, factor(smoke)))) %>% 
  mutate(smoke = ifelse(smoke == 2, smoke - 2, smoke))
# Check that after factoring, codes are the same
clean_exslp_1517 %>% count(smoke)
# 6707 no (0), 4551 yes (1), 10 NA
hist(clean_exslp_1517$smoke, breaks = -1:1)
table(clean_exslp_1517$smoke, useNA = 'always')

##### ALCOHOL VARIABLES
# Decided to use ALQ130 - Avg # alcoholic drinks/day - past 12 mos
# Questions phrased differently in 2015 and 2017 - need to correct
### Rename alcohol variables
names(clean_exslp_1517)
# Five different, slightly separated alcohol variables
summary(clean_exslp_1517[7:9])
summary(clean_exslp_1517[20:21])
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(
    alc101 = ALQ101,
    alc110 = ALQ110,
    alc111 = ALQ111,
    alc121 = ALQ121,
    alcohol = ALQ130
  ) %>% 
  select(-ALQ101, -ALQ110, -ALQ111, -ALQ121, -ALQ130)
names(clean_exslp_1517)

# First look at variables carefully
summary(clean_exslp_1517[39:43])
# alc101          alc110          alc111          alc121          alcohol      
# Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   : 0.000   Min.   :  1.00  
# 1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.: 1.000   1st Qu.:  1.00  
# Median :1.000   Median :2.000   Median :1.000   Median : 5.000   Median :  2.00  
# Mean   :1.336   Mean   :1.588   Mean   :1.114   Mean   : 4.984   Mean   :  4.04  
# 3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:1.000   3rd Qu.: 8.000   3rd Qu.:  3.00  
# Max.   :9.000   Max.   :9.000   Max.   :2.000   Max.   :99.000   Max.   :999.00  
# NA's   :6060    NA's   :9537    NA's   :6138    NA's   :6723     NA's   :4394    

# alcohol - Avg # alcoholic drinks/day - past 12 mos
table(clean_exslp_1517$alcohol, useNA = "always")
# 4394 in NA, 9 in 999, 1 in 777, 2481 in 1, range 1:15
# Large numbers of NAs in alcohol - try to fix
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(alcohol = 
           case_when(
             alc101 == 2 ~ as.integer(0), # If didn't have ≥12 alcohol drinks/1 yr - set to zero
             alc110 == 2 ~ as.integer(0), # If didn't have ≥12 alcohol drinks/lifetime? - set to zero
             alc111 == 2 ~ as.integer(0), # If never had a drink of any kind of alcohol - set to zero
             alc121 == 0 ~ as.integer(0), # If never had a drink in last year - set to zero
             TRUE ~ as.integer(alcohol) # For rest keep alcohol
           )) %>% 
  # Set refused & don't know to NA
  mutate(alcohol = ifelse(alcohol == 777, NA, ifelse(alcohol == 999, NA, alcohol)))
table(clean_exslp_1517$alcohol, useNA = "always")
# 1435 in NA, 3362 in 0, range 1:15
hist(clean_exslp_1517$alcohol, breaks = -1:15)
# Very skewed values with most in zero -> reduce categories to 0-4+
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(alcohol = ifelse(alcohol > 4, 4, alcohol))
table(clean_exslp_1517$alcohol, useNA = "always")
# 1435 in NA, 3362 in 0, 2200 in 1, 1935 in 2, 938 in 3, 1398 in 4+
hist(clean_exslp_1517$alcohol, breaks = -1:4)
# Official guidelines for men are to limit to under 2 alcoholic drinks/day - reduce categories
# New categories where 0 = no alcohol, 1= 1-2 drinks/day, 2 = 3+ drinks/day
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(alcohol = ifelse(alcohol == 2, alcohol - 1, ifelse(alcohol > 2, 2, alcohol)))
table(clean_exslp_1517$alcohol, useNA = "always")
# 3,362 in 0, 4,135 in 1, 2,336 in 2, 1,435 in NA
hist(clean_exslp_1517$alcohol, breaks = -1:2)
# Remove auxiliary alcoholvariables
clean_exslp_1517 <- clean_exslp_1517 %>% select(-alc101, -alc110, -alc111, -alc121)
# 11,268 obs of 39 variables

##### DEPRESSION VARIABLES
# Use PHQ-9 (sum first 9 questions) and dichotomize with 10 or more being depression
### Rename depression variables
names(clean_exslp_1517)
# Ten variables, and don't need last one for PHQ-9
summary(clean_exslp_1517[7:16])
clean_exslp_1517 <- clean_exslp_1517 %>% 
  mutate(
    phq01 = DPQ010,
    phq02 = DPQ020,
    phq03 = DPQ030,
    phq04 = DPQ040,
    phq05 = DPQ050,
    phq06 = DPQ060,
    phq07 = DPQ070,
    phq08 = DPQ080,
    phq09 = DPQ090,
  ) %>% 
  select(-c(DPQ010:DPQ100))
names(clean_exslp_1517)

# First look at variables carefully
summary(clean_exslp_1517[30:38]) # 11,268 obs of 10 variables
# phq01           phq02            phq03            phq04            phq05            phq06          
# Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000 
# 1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000 
# Median :0.000   Median :0.0000   Median :0.0000   Median :1.0000   Median :0.0000   Median :0.0000 
# Mean   :0.411   Mean   :0.3529   Mean   :0.6318   Mean   :0.7728   Mean   :0.4035   Mean   :0.2527 
# 3rd Qu.:1.000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000 
# Max.   :9.000   Max.   :9.0000   Max.   :9.0000   Max.   :9.0000   Max.   :9.0000   Max.   :9.0000 
# NA's   :1010    NA's   :1011     NA's   :1011     NA's   :1014     NA's   :1014     NA's   :1015 

#     phq07            phq08            phq09       
# Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
# Median :0.0000   Median :0.0000   Median :0.0000  
# Mean   :0.2655   Mean   :0.1737   Mean   :0.0621  
# 3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
# Max.   :9.0000   Max.   :9.0000   Max.   :9.0000  
# NA's   :1016     NA's   :1016     NA's   :1017   

# PHQ-9 - Patient Health Questionnaire
# Check distribution of unique values
clean_exslp_1517 %>% 
  select(c(phq01:phq09)) %>% 
  sapply(table) # 2-11 people refused (7) or didn't know (9) in each question
# Set all refused (7) or don't know (9) to NA
for(j in 30:38){
  clean_exslp_1517[,j][clean_exslp_1517[,j] == 7] <- NA
  clean_exslp_1517[,j][clean_exslp_1517[,j] == 9] <- NA
}
clean_exslp_1517 %>% select(c(phq01:phq09)) %>% sapply(table) # No more 7s or 9s
summary(clean_exslp_1517[30:38])
# 7 to 21 new NAs in each phq category, range lowered to 3, mean slightly lowered
# Most NAs are in the first question, and the least are in phq07
# Subset rows where phq07 was answered and phq01 wasn't
clean_exslp_1517 %>% 
  filter((complete.cases(clean_exslp_1517$phq07)) & (!complete.cases(clean_exslp_1517$phq01))) %>% 
  select(c(30:38))
# 17 participants, mostly with a single missing value (phq01)
summary(clean_exslp_1517[30:38])
# phq01            phq02            phq03            phq04            phq05            phq06       
# Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
# Median :0.0000   Median :0.0000   Median :0.0000   Median :1.0000   Median :0.0000   Median :0.0000  
# Mean   :0.3955   Mean   :0.3463   Mean   :0.6244   Mean   :0.7671   Mean   :0.3963   Mean   :0.2441  
# 3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000  
# Max.   :3.0000   Max.   :3.0000   Max.   :3.0000   Max.   :3.0000   Max.   :3.0000   Max.   :3.0000  
# NA's   :1031     NA's   :1020     NA's   :1021     NA's   :1022     NA's   :1023     NA's   :1026    

# phq07            phq08            phq09       
# Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
# Median :0.0000   Median :0.0000   Median :0.0000  
# Mean   :0.2599   Mean   :0.1655   Mean   :0.0549  
# 3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
# Max.   :3.0000   Max.   :3.0000   Max.   :3.0000  
# NA's   :1023     NA's   :1026     NA's   :1026    

# Age goes from 18-80, but education is not available from 18-20
clean_exslp_1517 %>% filter(age < 22) %>% select(c(17,21)) %>% table()
#     educ
# age   1  2  3  4
#   18  0  0  0  0
#   19  0  0  0  0
#   20 23 57 70  1
#   21 14 50 57  4
# Remove 18 & 19 year olds
clean_exslp_1517 <- clean_exslp_1517 %>% filter(age >= 20)
# 10,739 obs of 38 variables
clean_exslp_1517 %>% select(17, 21) %>% table()


##### Finished with clean dataset, ready for imputations

# Remove first column that was added when reading the csv file
clean_exslp_1517 <- clean_exslp_1517 %>% select(-1)
# 10,739 obs of 37 variables
summary(clean_exslp_1517)

# SEQN           WTINT2YR         WTMEC2YR         SDMVPSU         SDMVSTRA         vigex       
# Min.   : 83732   Min.   :  4363   Min.   :  4580   Min.   :1.000   Min.   :119.0   Min.   :0.0000  
# 1st Qu.: 88576   1st Qu.: 16373   1st Qu.: 17211   1st Qu.:1.000   1st Qu.:126.0   1st Qu.:0.0000  
# Median : 93513   Median : 25132   Median : 26406   Median :2.000   Median :133.0   Median :0.0000  
# Mean   : 93386   Mean   : 42025   Mean   : 44068   Mean   :1.503   Mean   :133.5   Mean   :0.2384  
# 3rd Qu.: 98170   3rd Qu.: 44985   3rd Qu.: 48071   3rd Qu.:2.000   3rd Qu.:141.0   3rd Qu.:0.0000  
# Max.   :102956   Max.   :433085   Max.   :419763   Max.   :2.000   Max.   :148.0   Max.   :1.0000  
#                                                                                    NA's   :1       
# 
#    daysvigex         minvigex          modex         daysmodex        minmodex         slphrs      
#  Min.   :0.0000   Min.   :  0.00   Min.   :0.000   Min.   :0.000   Min.   :  0.0   Min.   : 2.000  
#  1st Qu.:0.0000   1st Qu.:  0.00   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:  0.0   1st Qu.: 7.000  
#  Median :0.0000   Median :  0.00   Median :0.000   Median :0.000   Median :  0.0   Median : 7.500  
#  Mean   :0.7865   Mean   : 17.56   Mean   :0.403   Mean   :1.412   Mean   : 25.5   Mean   : 7.646  
#  3rd Qu.:0.0000   3rd Qu.:  0.00   3rd Qu.:1.000   3rd Qu.:3.000   3rd Qu.: 30.0   3rd Qu.: 8.500  
#  Max.   :7.0000   Max.   :480.00   Max.   :1.000   Max.   :7.000   Max.   :600.0   Max.   :14.000  
#  NA's   :2        NA's   :10       NA's   :4       NA's   :7       NA's   :21      NA's   :71      
# 
#     snoring          apnea            gender           age        raceeth      usborn         usyears    
#  Min.   :0.000   Min.   :0.0000   Min.   :1.000   Min.   :20.00   1:3598   Min.   :1.000   Min.   :1.00  
#  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:35.00   2:2868   1st Qu.:1.000   1st Qu.:4.00  
#  Median :1.000   Median :0.0000   Median :2.000   Median :51.00   3:2401   Median :1.000   Median :6.00  
#  Mean   :1.501   Mean   :0.2487   Mean   :1.519   Mean   :50.42   4:1872   Mean   :1.327   Mean   :5.34  
#  3rd Qu.:3.000   3rd Qu.:0.0000   3rd Qu.:2.000   3rd Qu.:65.00            3rd Qu.:2.000   3rd Qu.:7.00  
#  Max.   :3.000   Max.   :1.0000   Max.   :2.000   Max.   :80.00            Max.   :2.000   Max.   :9.00  
#  NA's   :818     NA's   :671                                               NA's   :4       NA's   :7381 
# 
#       educ          marital        pregnancy       household         income         bmi       
#  Min.   :1.000   Min.   :1.000   Min.   :0.000   Min.   :1.000   Min.   :1.0   Min.   :14.20  
#  1st Qu.:2.000   1st Qu.:1.000   1st Qu.:0.000   1st Qu.:2.000   1st Qu.:1.0   1st Qu.:24.70  
#  Median :3.000   Median :2.000   Median :0.000   Median :3.000   Median :2.0   Median :28.50  
#  Mean   :2.579   Mean   :1.596   Mean   :0.637   Mean   :3.148   Mean   :1.9   Mean   :29.69  
#  3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:4.000   3rd Qu.:2.0   3rd Qu.:33.40  
#  Max.   :4.000   Max.   :2.000   Max.   :2.000   Max.   :6.000   Max.   :3.0   Max.   :86.20  
#  NA's   :15      NA's   :7                                       NA's   :868   NA's   :158    
# 
#      waist           smoke           alcohol           phq01            phq02           phq03       
#  Min.   : 57.9   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
#  1st Qu.: 88.6   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000  
#  Median : 99.1   Median :0.0000   Median :1.0000   Median :0.0000   Median :0.000   Median :0.0000  
#  Mean   :100.5   Mean   :0.4193   Mean   :0.9023   Mean   :0.3957   Mean   :0.349   Mean   :0.6221  
#  3rd Qu.:110.5   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.000   3rd Qu.:1.0000  
#  Max.   :171.6   Max.   :1.0000   Max.   :2.0000   Max.   :3.0000   Max.   :3.000   Max.   :3.0000  
#  NA's   :686     NA's   :10       NA's   :1397     NA's   :991      NA's   :981     NA's   :982     
# 
#      phq04            phq05           phq06            phq07            phq08            phq09       
#  Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#  1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
#  Median :1.0000   Median :0.000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
#  Mean   :0.7675   Mean   :0.395   Mean   :0.2432   Mean   :0.2582   Mean   :0.1663   Mean   :0.0541  
#  3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
#  Max.   :3.0000   Max.   :3.000   Max.   :3.0000   Max.   :3.0000   Max.   :3.0000   Max.   :3.0000  
#  NA's   :983      NA's   :984     NA's   :987      NA's   :984      NA's   :987      NA's   :987     
# 

# Write to csv file
write.csv(clean_exslp_1517, "clean_exslp_1517.csv")
# 10,739 obs of 37 variables

#-#-#-#-#

