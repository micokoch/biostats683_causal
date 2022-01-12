##### Additional computations

### Libraries
library(tidyverse)

### Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

##### Read in variables
clean_exslp_1517 <- read_csv("clean_exslp_1517.csv")
# Index column was added -> remove
raw_exslp_1517 <- clean_exslp_1517 %>% select(-1, -usyears)
# 10,739 obs of 37 variables
summary(raw_exslp_1517)

