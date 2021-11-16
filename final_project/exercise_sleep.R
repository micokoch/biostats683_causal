## Exercise and sleep
#Libraries
library(tidyverse)
library(nhanesA)

setwd("final_project")
getwd()


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
exer1517 <- bind_rows(exercise2015, exercise2017)
exer1517 <- exer1517 %>% 
  drop_na('PAQ650', 'PAQ665')
summary(exer1517)

### Vigorous exercise
# Look at responses for vigorous exercise
exer1517 %>% 
  group_by(PAQ650) %>% 
  summarise(n = n())
# Remove respondents who didn't know or refused to answer q on vigorous exercise
exer1517 <- exer1517 %>% 
  subset(PAQ650 != 9 & PAQ650 != 7)
# If participant wrote No to vigorous exercise, set vigorous days and minutes to zero
exer1517$PAQ655[exer1517$PAQ650 == 2] <- 0
exer1517$PAD660[exer1517$PAQ650 == 2] <- 0
summary(exer1517)
# Make a simple imputation of the mean if days or minutes of exercise not specified
# Days vigorous exercise simple imputation
exer1517 <- exer1517 %>% 
  mutate(PAQ655 = ifelse(PAQ655 > 10, NA, PAQ655))
summary(exer1517)
exer1517 <- exer1517 %>% 
  mutate(PAQ655 = ifelse(is.na(PAQ655), mean(PAQ655, na.rm = TRUE), PAQ655))
summary(exer1517)
# Minutes vigorous exercise simple imputation
exer1517 <- exer1517 %>% 
  mutate(PAD660 = ifelse(PAD660 > 1000, NA, PAD660))
summary(exer1517)
exer1517 <- exer1517 %>% 
  mutate(PAD660 = ifelse(is.na(PAD660), mean(PAD660, na.rm = TRUE), PAD660))
summary(exer1517)
#Create new column of weekly minutes of vigorous exercise
exer1517 <- exer1517 %>% 
  mutate(vigexminwk = (PAQ655*PAD660))
summary(exer1517)
hist(exer1517$vigexminwk)
# There are few extreme values, so I will log 2 transform them (excluding zero)
exer1517 <- exer1517 %>% 
  mutate(lg2vigexminwk = ifelse(vigexminwk == 0, NA, log2(vigexminwk)))
hist(exer1517$lg2vigexminwk) # Now we have a normally distributed set of values
summary(exer1517)
### Moderate exercise
# Look at responses for moderate exercise
exer1517 %>% 
  group_by(PAQ665) %>% 
  summarise(n = n())
# Remove respondents who didn't know or refused to answer q on moderate exercise
exer1517 <- exer1517 %>% 
  subset(PAQ665 != 9 & PAQ665 != 7)
# If participant wrote No to moderate exercise, set moderate days and minutes to zero
exer1517$PAQ670[exer1517$PAQ665 == 2] <- 0
exer1517$PAD675[exer1517$PAQ665 == 2] <- 0
summary(exer1517)
# Make a simple imputation of the mean if days or minutes of exercise not specified
# Days moderate exercise simple imputation
exer1517 <- exer1517 %>% 
  mutate(PAQ670 = ifelse(PAQ670 > 10, NA, PAQ670))
summary(exer1517)
exer1517 <- exer1517 %>% 
  mutate(PAQ670 = ifelse(is.na(PAQ670), mean(PAQ670, na.rm = TRUE), PAQ670))
summary(exer1517)
# Minutes moderate exercise simple imputation
exer1517 <- exer1517 %>% 
  mutate(PAD675 = ifelse(PAD675 > 1000, NA, PAD675))
summary(exer1517)
exer1517 <- exer1517 %>% 
  mutate(PAD675 = ifelse(is.na(PAD675), mean(PAD675, na.rm = TRUE), PAD675))
summary(exer1517)
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
#Create new column of sum of moderate and vigorous exercise (multiply vigorous exercise times two)
exer1517 <- exer1517 %>% 
  mutate(exminwk = ((vigexminwk*2) + modexminwk))
summary(exer1517)
hist(exer1517$exminwk)
# There are few extreme values, so I will log 2 transform them (excluding zero)
exer1517 <- exer1517 %>% 
  mutate(lg2exminwk = ifelse(exminwk == 0, NA, log2(exminwk)))
hist(exer1517$lg2exminwk) # Now we have a normally distributed set of values
summary(exer1517)
#Create new binary variable indicating whether person did more than 150 min exercise/wk
exer1517 <- exer1517 %>% 
  mutate(targetex = ifelse(exminwk < 150, 0, 1))
summary(exer1517)
hist(exer1517$targetex, breaks = 2)
# Create smaller dataframe with fewer columns
smex <- exer1517 %>% 
  dplyr::select(SEQN, targetex, exminwk, modexminwk, vigexminwk)
summary(smex)

##### SLEEP
# Exclude years before 2007 because they didn't collect variables we needed
# Select and rename sleep variables for merging (slightly different names across years)
# Only use 2015 & 2017 because data collected differently prior to that

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
### Note - there seems to be a change in methodology starting in 2017. Fewer people with little sleep.
# 2015
sleep2015 <- nhanes('SLQ_I') %>% 
  dplyr::select(c('SEQN', 'SLD012'))
summary(sleep2015)
sleep2015 <- sleep2015 %>% 
  mutate(slphrs = SLD012)
sleep2015 <- sleep2015 %>% 
  mutate(slphrs = ifelse(slphrs > 15, NA, slphrs)) %>% 
  mutate(slphrs = ifelse(slphrs > 12, 12, slphrs)) %>% 
  dplyr::select(-SLD012)
summary(sleep2015)
sleep2015 <- sleep2015 %>% 
  drop_na('slphrs') %>% 
  mutate(targetslp = ifelse(slphrs > 6, 1, 0))
summary(sleep2015)
hist(sleep2015$targetslp, breaks = 2)
# 2017
sleep2017 <- nhanes('SLQ_J') %>% 
  dplyr::select(c('SEQN', 'SLD012'))
summary(sleep2017)
sleep2017 <- sleep2017 %>% 
  mutate(slphrs = SLD012)
sleep2017 <- sleep2017 %>% 
  mutate(slphrs = ifelse(slphrs > 15, NA, slphrs)) %>% 
  mutate(slphrs = ifelse(slphrs > 12, 12, slphrs)) %>% 
  dplyr::select(-SLD012)
summary(sleep2017)
sleep2017 <- sleep2017 %>% 
  drop_na('slphrs') %>% 
  mutate(targetslp = ifelse(slphrs > 6, 1, 0))
summary(sleep2017)
hist(sleep2017$targetslp, breaks = 2)
# sleep1720 <- nhanes('P_SLQ')
# Combine all sleep tables
# sleep0717 <- bind_rows(sleep2007, sleep2009, sleep2011, sleep2013, sleep2015, sleep2017)
sleep1517 <- bind_rows(sleep2015, sleep2017)
summary(sleep1517)
hist(sleep1517$targetslp, breaks = 2)
hist(sleep1517$slphrs, breaks = 10)

##### Final Tables
# Merge sleep and exercise tables
slpex <- merge(smex, sleep1517, by = 'SEQN')
summary(slpex)
write_csv(slpex, "slpex.csv")
# Binary table
binslpex <- dplyr::select(slpex, SEQN, targetex, targetslp)
summary(binslpex)

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

binslpex %>% 
  dplyr::select(-SEQN) %>% 
  group_by(targetex, targetslp) %>% 
  summarise(n = n())

###################