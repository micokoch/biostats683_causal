## Tests
# Libraries
# library(tidyverse)
# library(nhanesA)
# 
# setwd("final_project")
getwd()

slpexcov1517 <- read_csv("slpexcov1517.csv")
summary(slpexcov1517)

# Check imputed values compared with smaller dataset
eximprev <- merge(slpexcov1517, exer1517, by = 'SEQN')
# PAQ655 - 1 imputation made to 1 day (mean = 0.9856)
mean(eximprev$PAQ655) # mean 1.093618 -> imputation would be to 1 day
# PAD660 - 11 imputations made to 22 minutes (mean = 21.78)
mean(eximprev$PAD660) # mean 28.27611 -> imputation would be to 28 minutes -> correct
# PAQ670 - 6 imputation made to 1 day (mean = 1.445)
mean(eximprev$PAQ670) # mean 1.430643 -> imputation would be to 1 day
# PAD675 - 23 imputations made to 26 minutes (mean = 26)
mean(eximprev$PAD675) # mean 30.86181 -> imputation would be to 31 minutes -> correct



