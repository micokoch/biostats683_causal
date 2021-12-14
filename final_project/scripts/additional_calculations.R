## Additional calculations
# Libraries
# library(tidyverse)
# library(nhanesA)
# 
# setwd("final_project")
getwd()
set.seed(252)

##### Confounders for exercise and sleep
# Unimputed exercise dataset:
exer1517_complete <- read.csv("exer1517_complete.csv")
# Final dataset for analysis
slpexcov1517 <- read_csv("slpexcov1517.csv")
# Limit unimputed dataset to participants used for analysis
exer1517_sm <- exer1517_complete %>% filter(SEQN %in% slpexcov1517$SEQN)
# Read in SEQN for participants with imputed exercise
load("imputed_exer.RData")
# Select only imputed participants and look at them
exer1517_imputed_part <- exer1517_sm %>% filter(SEQN %in% imputed_exer)
exer1517_imputed_part
#     SEQN PAQ650 PAQ655 PAD660 PAQ665 PAQ670 PAD675
# 1  83986      2     NA     NA      1      3     NA - only minutes for modex missing
# 2  88224      1      3     NA      2     NA     NA - only minutes for vigex missing
# 3  89043      1      7     NA      1      7     NA - minutes for mod and vig ex missing
# 4  89543      1      3     NA      1      2     NA - minutes for mod and vig ex missing
# 5  92417      1      7     NA      1      7     NA - minutes for mod and vig ex missing
# 6  92924      2     NA     NA      1      7     NA - only minutes for modex missing
# 7  97054      2     NA     NA      1      6     NA - only minutes for modex missing
# 8  97157      1      4     NA      1      4     NA - minutes for mod and vig ex missing
# 9  97445      1      6     NA      1      6     NA - minutes for mod and vig ex missing
# 10 98134      1      7   9999      2     NA     NA - only minutes for vigex missing

# No one has days missing - 3 modex, 2 vigex, 5 both, have missing data

# To do list:
# Check diet quality and pattern variables in NHANES (missing data)