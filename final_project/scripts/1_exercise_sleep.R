## Exercise and sleep
#Libraries
library(tidyverse)
library(nhanesA)

setwd("final_project")
getwd()
set.seed(252)

##### EXERCISE
# Exercise - first years of NHANES didn't collect variables as we want them.
# exercise1999 <- nhanes('PAQ')
# exercise2001 <- nhanes('PAQ_B')
# exercise2003 <- nhanes('PAQ_C')
# exercise2005 <- nhanes('PAQ_D')

# Select variables for moderate and vigorous exercise by year
# Decided to only try 2015 & 2017, because sleep is measured differently in these last two
# # 2007
# exercise2007 <- nhanes('PAQ_E') %>% 
#   select(c('SEQN', 'PAQ650', 'PAQ655', 'PAD660', 'PAQ665', 'PAQ670', 'PAD675'))
# #2009
# exercise2009 <- nhanes('PAQ_F') %>% 
#   select(c('SEQN', 'PAQ650', 'PAQ655', 'PAD660', 'PAQ665', 'PAQ670', 'PAD675'))
# # 2011
# exercise2011 <- nhanes('PAQ_G') %>% 
#   select(c('SEQN', 'PAQ650', 'PAQ655', 'PAD660', 'PAQ665', 'PAQ670', 'PAD675'))
# # 2013
# exercise2013 <- nhanes('PAQ_H') %>% 
#   select(c('SEQN', 'PAQ650', 'PAQ655', 'PAD660', 'PAQ665', 'PAQ670', 'PAD675'))
# 2015
exercise2015 <- nhanes('PAQ_I') %>% 
  dplyr::select(c('SEQN', 'PAQ650', 'PAQ655', 'PAD660', 'PAQ665', 'PAQ670', 'PAD675'))
# 2017
exercise2017 <- nhanes('PAQ_J') %>% 
  dplyr::select(c('SEQN', 'PAQ650', 'PAQ655', 'PAD660', 'PAQ665', 'PAQ670', 'PAD675'))
# exercise1720 <- nhanes('P_PAQ')

# Combine all tables and drop people who did not respond to whether they did exercise
# exer0717 <- bind_rows(exercise2007, exercise2009, exercise2011, exercise2013,
#                       exercise2015, exercise2017)
exer1517 <- bind_rows(exercise2015, exercise2017) # initially 15,111 observations
write_csv(exer1517, "exer1517_complete.csv")
exer1517 <- exer1517 %>% 
  drop_na('PAQ650', 'PAQ665') # fell to 12,819 obs
summary(exer1517) # PAQ655 & PAD660 have 9,192 & 9,199 NA each, & PAQ670 & PAD675 have 7,555 & 7,572 NA each

### Vigorous exercise
# Look at responses for vigorous exercise
exer1517 %>% 
  group_by(PAQ650) %>% 
  summarise(n = n()) # 3,627 are 1, 9,190 are 2, and 2 are 9
# Remove respondents who didn't know or refused to answer q on vigorous exercise
exer1517 <- exer1517 %>% 
  subset(PAQ650 != 9 & PAQ650 != 7) # fell to 12,817 (dropped two)
# If participant wrote No to vigorous exercise, set vigorous days and minutes to zero
exer1517$PAQ655[exer1517$PAQ650 == 2] <- 0
exer1517$PAD660[exer1517$PAQ650 == 2] <- 0
summary(exer1517) # All NAs for PAQ655 disappear and only seven remain for PAD660
# Make a simple imputation of the rounded mean if days or minutes of exercise not specified
# First look at how many will be imputed
table(exer1517$PAQ650, useNA = "always") # 3,627 are 1, 9,190 are 2
table(exer1517$PAQ655, useNA = "always") # Only 1 is 99 (1 imputation), 9,190 are 0, 915 are 3...
table(exer1517$PAD660, useNA = "always") # 4 are 9999, 7 NAs (11 imputations), 9,190 are 0, 1,122 are 60..
# Days vigorous exercise simple imputation with mean - one imputation
# Change missing values to NA
exer1517 <- exer1517 %>% 
  mutate(PAQ655 = ifelse(PAQ655 > 10, NA, PAQ655))
summary(exer1517) # Now 1 NA, mean = 0.9856
# Keep track of imputations (to compare how many actually imputed after adding covariates)
imputed_exer <- c()
imp <- exer1517 %>% filter(is.na(PAQ655)) %>% select(SEQN)
for(i in imp){
  imputed_exer <- append(imputed_exer, i)
}
imputed_exer
# Impute NAs to rounded mean
exer1517 <- exer1517 %>% 
  mutate(PAQ655 = ifelse(is.na(PAQ655), round(mean(PAQ655, na.rm = TRUE)), PAQ655))
summary(exer1517)
table(exer1517$PAQ655, useNA = "always") # 1 imputation made to 1 day
# Previous mean = 0.99, smaller dataset mean = 1.09
hist(exer1517$PAQ655, breaks = -1:7)
# Minutes vigorous exercise simple imputation - 11 imputations
# Change missing values to NA
exer1517 <- exer1517 %>% 
  mutate(PAD660 = ifelse(PAD660 > 1000, NA, PAD660))
summary(exer1517) # Went from 7 to 11 NAs, mean = 21.78
# Keep track of imputations (to compare how many actually imputed after adding covariates)
imp <- exer1517 %>% filter(is.na(PAD660)) %>% select(SEQN)
for(i in imp){
  imputed_exer <- append(imputed_exer, i)
}
imputed_exer
# Impute NAs to rounded mean
exer1517 <- exer1517 %>% 
  # mutate(PAD660 = ifelse(is.na(PAD660), round(mean(PAD660, na.rm = TRUE)), PAD660))
  # Previously, 11 imputations made to 22 minutes (mean = 21.78)
  mutate(PAD660 = ifelse(is.na(PAD660), 28, PAD660))
# Corrected imputation to 28, after reviewing mean on smaller dataset
summary(exer1517)
table(exer1517$PAD660, useNA = "always") # 11 imputations made to 28 minutes (mean = 28.28)
hist(exer1517$PAD660)
# Create new column of weekly minutes of vigorous exercise
exer1517 <- exer1517 %>% 
  mutate(vigexminwk = (PAQ655*PAD660))
summary(exer1517)
hist(exer1517$vigexminwk)
# There are few extreme values, so I will log 2 transform them (excluding zeros)
exer1517 <- exer1517 %>% 
  mutate(lg2vigexminwk = ifelse(vigexminwk == 0, NA, log2(vigexminwk)))
hist(exer1517$lg2vigexminwk) # Now we have a normally distributed set of values
summary(exer1517)
### Moderate exercise
# Look at responses for moderate exercise
exer1517 %>% 
  group_by(PAQ665) %>% 
  summarise(n = n()) # 5,262 are 1, 7,551 are 2, and 4 are 9
# Remove respondents who didn't know or refused to answer q on moderate exercise
exer1517 <- exer1517 %>% 
  subset(PAQ665 != 9 & PAQ665 != 7) # fell to 12,813 (dropped four)
# If participant wrote No to moderate exercise, set moderate days and minutes to zero
exer1517$PAQ670[exer1517$PAQ665 == 2] <- 0
exer1517$PAD675[exer1517$PAQ665 == 2] <- 0
summary(exer1517) # All NAs for PAQ670 disappear and only 16 remain for PAD675
# Make a simple imputation of the mean if days or minutes of exercise not specified
# First look at how many will be imputed
table(exer1517$PAQ665, useNA = "always") # 5,262 are 7,551 are 2
table(exer1517$PAQ670, useNA = "always") # 6 are 99 (6 imputations), 7,551 are 0, 1,293 are 3...
table(exer1517$PAD675, useNA = "always") # 7 are 9999, 16 NAs (23 imputations), 7,551 are 0, 1,439 are 60..
# Days moderate exercise simple imputation with mean - 6 imputations
# Change missing values to NA
exer1517 <- exer1517 %>% 
  mutate(PAQ670 = ifelse(PAQ670 > 10, NA, PAQ670))
summary(exer1517) # Now 6 NAs, mean = 1.446
# Keep track of imputations (to compare how many actually imputed after adding covariates)
imp <- exer1517 %>% filter(is.na(PAQ670)) %>% select(SEQN)
for(i in imp){
  imputed_exer <- append(imputed_exer, i)
}
imputed_exer
# Impute NAs to rounded mean
exer1517 <- exer1517 %>% 
  mutate(PAQ670 = ifelse(is.na(PAQ670), round(mean(PAQ670, na.rm = TRUE)), PAQ670))
summary(exer1517)
table(exer1517$PAQ670, useNA = "always") # 6 imputations made to 1 day
# Previous mean = 1.45, smaller dataset mean = 1.43
hist(exer1517$PAQ670, breaks = -1:7)
# Minutes moderate exercise simple imputation - 23 imputations
# Change missing values to NA
exer1517 <- exer1517 %>% 
  mutate(PAD675 = ifelse(PAD675 > 1000, NA, PAD675))
summary(exer1517) # Went from 16 to 23 NAs, mean = 26
# Keep track of imputations (to compare how many actually imputed after adding covariates)
imp <- exer1517 %>% filter(is.na(PAD675)) %>% select(SEQN)
for(i in imp){
  imputed_exer <- append(imputed_exer, i)
}
imputed_exer
# Impute NAs to rounded mean
exer1517 <- exer1517 %>% 
  # mutate(PAD660 = ifelse(is.na(PAD660), round(mean(PAD660, na.rm = TRUE)), PAD660))
  # Previously, 11 imputations made to 22 minutes (mean = 21.78)
  mutate(PAD660 = ifelse(is.na(PAD660), 28, PAD660))
# Corrected imputation to 28, after reviewing mean on smaller dataset
summary(exer1517)
table(exer1517$PAD660, useNA = "always") # 11 imputations made to 28 minutes (mean = 28.28)
hist(exer1517$PAD660)
# Impute NAs to rounded mean
exer1517 <- exer1517 %>% 
  # mutate(PAD675 = ifelse(is.na(PAD675), round(mean(PAD675, na.rm = TRUE)), PAD675))
  # Previously, 23 imputations made to 26 minutes (mean = 26)
  mutate(PAD675 = ifelse(is.na(PAD675), 31, PAD675))
# Corrected imputation to 31, after reviewing mean on smaller dataset
summary(exer1517)
table(exer1517$PAD675, useNA = "always") # 23 imputations made to 31 minutes (mean = 30.86)
hist(exer1517$PAD675)
#Create new column of weekly minutes of moderate exercise
exer1517 <- exer1517 %>% 
  mutate(modexminwk = (PAQ670*PAD675))
summary(exer1517)
hist(exer1517$modexminwk)
# There are few extreme values, so I will log 2 transform them (excluding zero)
exer1517 <- exer1517 %>% 
  mutate(lg2modexminwk = ifelse(modexminwk == 0, NA, log2(modexminwk)))
hist(exer1517$lg2modexminwk) # Now we have a normally distributed set of values
summary(exer1517)

# Parenthesis - doubts about people who write > 300 minutes/day of exercise
highvigex <- exer1517 %>% subset(PAD660 > 300)
highvigex
highmodex <- exer1517 %>% subset(PAD675 > 300)
highmodex
# No real red flags after looking at participants in detail

# Create new column of sum of moderate and vigorous exercise (multiply vigorous exercise times two)
exer1517 <- exer1517 %>% 
  mutate(exminwk = ((vigexminwk*2) + modexminwk))
summary(exer1517) #12,813 obs of 12 variables
hist(exer1517$exminwk)
# There are few extreme values, so I will log 2 transform them (excluding zero)
exer1517 <- exer1517 %>% 
  mutate(lg2exminwk = ifelse(exminwk == 0, NA, log2(exminwk)))
hist(exer1517$lg2exminwk) # Now we have a normally distributed set of values
summary(exer1517) # 12,813 obs of 13 variables

# Create new binary variable indicating whether person did more than 150 min exercise/wk
exer1517 <- exer1517 %>% 
  mutate(targetex = ifelse(exminwk < 150, 0, 1))
summary(exer1517) # 12,813 obs of 14 variables
table(exer1517$targetex, useNA = "always") # 8,091 are 0, 4,722 are 1 (mean = 0.3685)
hist(exer1517$targetex, breaks = 2)
# Create smaller dataframe with fewer columns
smex <- exer1517 %>% 
  dplyr::select(SEQN, vigexminwk, modexminwk, exminwk, targetex)
summary(smex) # 12,813 obs of 5 variables
# Save dataset with exercise variables for analysis
write_csv(smex, "smex.csv")
# Save imputed vector as object for later use
save(imputed_exer, file = "imputed_exer.RData")

##### SLEEP
# Exclude years before 2007 because they didn't collect variables we needed
# Select and rename sleep variables for merging (slightly different names across years)
# Only use 2015 & 2017 because data collected differently prior to that
# Later changed target sleep to be 7 hours or more

# 2007
# sleep2007 <- nhanes('SLQ_E') %>% 
#   select(c('SEQN', 'SLD010H'))
# summary(sleep2007)
# sleep2007 <- sleep2007 %>% 
#   mutate(slphrs = SLD010H)
# sleep2007 <- sleep2007 %>% 
#   mutate(slphrs = ifelse(slphrs > 15, NA, slphrs)) %>% 
#   mutate(slphrs = ifelse(slphrs < 2, 2, slphrs)) %>% 
#   select(-SLD010H)
# summary(sleep2007)
# sleep2007 <- sleep2007 %>% 
#   drop_na('slphrs') %>% 
#   mutate(targetslp = ifelse(slphrs > 6, 1, 0))
# summary(sleep2007)
# hist(sleep2007$targetslp, breaks = 2)
# #2009
# sleep2009 <- nhanes('SLQ_F') %>% 
#   select(c('SEQN', 'SLD010H'))
# summary(sleep2009)
# sleep2009 <- sleep2009 %>% 
#   mutate(slphrs = SLD010H)
# sleep2009 <- sleep2009 %>% 
#   mutate(slphrs = ifelse(slphrs > 15, NA, slphrs)) %>% 
#   mutate(slphrs = ifelse(slphrs < 2, 2, slphrs)) %>% 
#   select(-SLD010H)
# summary(sleep2009)
# sleep2009 <- sleep2009 %>% 
#   drop_na('slphrs') %>% 
#   mutate(targetslp = ifelse(slphrs > 6, 1, 0))
# summary(sleep2009)
# hist(sleep2009$targetslp, breaks = 2)
# # 2011
# sleep2011 <- nhanes('SLQ_G') %>% 
#   select(c('SEQN', 'SLD010H'))
# summary(sleep2011)
# sleep2011 <- sleep2011 %>% 
#   mutate(slphrs = SLD010H)
# sleep2011 <- sleep2011 %>% 
#   mutate(slphrs = ifelse(slphrs > 15, NA, slphrs)) %>% 
#   mutate(slphrs = ifelse(slphrs < 2, 2, slphrs)) %>% 
#   select(-SLD010H)
# summary(sleep2011)
# sleep2011 <- sleep2011 %>% 
#   drop_na('slphrs') %>% 
#   mutate(targetslp = ifelse(slphrs > 6, 1, 0))
# summary(sleep2011)
# hist(sleep2011$targetslp, breaks = 2)
# # 2013
# sleep2013 <- nhanes('SLQ_H') %>% 
#   select(c('SEQN', 'SLD010H'))
# summary(sleep2013)
# sleep2013 <- sleep2013 %>% 
#   mutate(slphrs = SLD010H)
# sleep2013 <- sleep2013 %>% 
#   mutate(slphrs = ifelse(slphrs > 15, NA, slphrs)) %>% 
#   mutate(slphrs = ifelse(slphrs < 2, 2, slphrs)) %>% 
#   select(-SLD010H)
# summary(sleep2013)
# sleep2013 <- sleep2013 %>% 
#   drop_na('slphrs') %>% 
#   mutate(targetslp = ifelse(slphrs > 6, 1, 0))
# summary(sleep2013)
# hist(sleep2013$targetslp, breaks = 2)
### Note - there seems to be a change in methodology starting in 2015. Fewer people with little sleep.

# 2015
sleep2015 <- nhanes('SLQ_I') %>% 
  dplyr::select(c('SEQN', 'SLD012'))
summary(sleep2015) # 6,327 obs of 2 variables (range: 2-14.5, 33 NAs)
sleep2015 <- sleep2015 %>% 
  mutate(slphrs = SLD012)
table(sleep2015$slphrs)
hist(sleep2015$slphrs)
sleep2015 <- sleep2015 %>% 
  mutate(slphrs = ifelse(slphrs > 15, NA, slphrs)) %>% 
  dplyr::select(-SLD012)
summary(sleep2015) # No real change except now 2 columns - SEQN and slphrs
# Drop NAs and create targetslp variable
sleep2015 <- sleep2015 %>% 
  drop_na('slphrs') %>% 
  mutate(targetslp = ifelse(slphrs >= 7, 1, 0))
# Later implemented change in target sleep (from >6 to >=7, verify changes are correct)
table(sleep2015$slphrs)
# 393 people are in 6.5 hours category and should have shifted - changes worked!
summary(sleep2015) # 6,294 obs of 3 variables
table(sleep2015$targetslp) # 1,379 are 0 and 4,915 are 1
hist(sleep2015$targetslp, breaks = 2)

# 2017
sleep2017 <- nhanes('SLQ_J') %>% 
  dplyr::select(c('SEQN', 'SLD012'))
summary(sleep2017)  # 6,161 obs of 2 variables (range: 2-14, 48 NAs)
sleep2017 <- sleep2017 %>% 
  mutate(slphrs = SLD012)
table(sleep2017$slphrs) # 378 people in 6.5 hour sleep category
hist(sleep2017$slphrs)
sleep2017 <- sleep2017 %>% 
  mutate(slphrs = ifelse(slphrs > 15, NA, slphrs)) %>% 
  dplyr::select(-SLD012)
summary(sleep2017) # No real change except now 2 columns - SEQN and slphrs
# Drop NAs and create targetslp variable
sleep2017 <- sleep2017 %>% 
  drop_na('slphrs') %>% 
  mutate(targetslp = ifelse(slphrs >= 7, 1, 0))
# Later implemented change in target sleep (from >6 to >=7, verify changes are correct)
table(sleep2017$slphrs)
# 378 people are in 6.5 hours category and should have shifted - changes worked!
summary(sleep2017) # 6,113 obs of 3 variables
table(sleep2017$targetslp) # 1,496 are 0 and 4,617 are 1
hist(sleep2017$targetslp, breaks = 2)

# sleep1720 <- nhanes('P_SLQ')
# Combine all sleep tables
# sleep0717 <- bind_rows(sleep2007, sleep2009, sleep2011, sleep2013, sleep2015, sleep2017)
sleep1517 <- bind_rows(sleep2015, sleep2017) # 12,407 obs of 3 variables
summary(sleep1517)
table(sleep1517$targetslp) # 2,875 are 0 and 9,532 are 1
hist(sleep1517$targetslp, breaks = 2)
hist(sleep1517$slphrs, breaks = 10)
# Save dataset with sleep variables for analysis
write_csv(sleep1517, "smslp.csv")

##### Final Tables
# Merge sleep and exercise tables
slpex <- merge(smex, sleep1517, by = 'SEQN') # 12,098 obs of 7 variables
summary(slpex)
write_csv(slpex, "slpex.csv")
# Binary table
binslpex <- dplyr::select(slpex, SEQN, targetex, targetslp)
summary(binslpex)

# Contingency tables
binslpex %>% select(-SEQN) %>% table()
#          targetslp
# targetex    0    1
#        0 1859 5960
#        1  959 3320

binslpex %>% select(-SEQN) %>% table %>% prop.table(margin = 1)
#           targetslp
# targetex         0         1
#    0     0.2377542 0.7622458
#    1     0.2241178 0.7758822
binslpex %>% select(-SEQN) %>% table %>% prop.table(margin = 2)
#           targetslp
# targetex         0         1
#    0     0.6596877 0.6422414
#    1     0.3403123 0.3577586

binslpex %>% 
  dplyr::select(-SEQN) %>% 
  group_by(targetex, targetslp) %>% 
  summarise(n = n())
# # A tibble: 4 × 3
# # Groups:   targetex [2]
#   targetex targetslp     n
#       <dbl>     <dbl> <int>
# 1        0         0  1859
# 2        0         1  5960
# 3        1         0   959
# 4        1         1  3320

####### ANALYSIS
# library(gtsummary)
# library(pubh)
# library(MASS)
# library(epiR)
# tbl_summary(slpex)
# tbl_summary(binslpex, by = targetex)
# binslpex %>%
#   dplyr::select(targetex, targetslp) %>% 
#   cross_tbl(by = "targetex") %>%
#   theme_pubh(2)
# con1<-table(binslpex$targetex, binslpex$targetslp, dnn = c("Exercise", "Sleep"))
# con1
# mosaicplot(con1)

###################
