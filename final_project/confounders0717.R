## Confounders for exercise and sleep
# Libraries
library(tidyverse)
library(nhanesA)

setwd("final_project")
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
  dplyr::select(c('SEQN', 'RIAGENDR', 'RIDAGEYR', 'RIDRETH3', 'DMDBORN4', 'DMDYRSUS', 'DMDEDUC3',
                  'DMDEDUC2', 'DMDMARTL', 'RIDEXPRG', 'DMDHHSIZ', 'DMDFMSIZ', 'INDHHIN2', 'INDFMIN2',
                  'INDFMPIR'))
# 2017
demogconf2017 <- nhanes('DEMO_J') %>% 
  dplyr::select(c('SEQN', 'RIAGENDR', 'RIDAGEYR', 'RIDRETH3', 'DMDBORN4', 'DMDYRSUS', 'DMDEDUC3',
                  'DMDEDUC2', 'DMDMARTL', 'RIDEXPRG', 'DMDHHSIZ', 'DMDFMSIZ', 'INDHHIN2', 'INDFMIN2',
                  'INDFMPIR'))
# demogconf1720 <- nhanes('P_DEMO')

# Combine all confounder tables
# demogconf0709 <- dplyr::bind_rows(demogconf2007, demogconf2009)
# demogconf1113 <- dplyr::bind_rows(demogconf2011, demogconf2013)
demogconf1517 <- dplyr::bind_rows(demogconf2015, demogconf2017)
summary(demogconf1517)

# Merge slpex and confounders
slpexconf <- merge(slpex, demogconf1517, by = 'SEQN')
summary(slpexconf)
head(slpexconf)
tail(slpexconf)
write_csv(slpexconf, "slpexconf.csv")

#####