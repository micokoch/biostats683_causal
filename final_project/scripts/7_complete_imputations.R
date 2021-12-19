##### Imputing clean dataset

# Libraries
library(tidyverse)
library(mice)
library(VIM)
# library(nhanesA)
# library(survey)
# library(gtools)

# Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

##### Read in variables
clean_exslp_1517 <- read_csv("clean_exslp_1517.csv")
# Index column was added -> remove
impute_exslp_1517 <- clean_exslp_1517 %>% select(-1)
# 10,739 obs of 37 variables
summary(impute_exslp_1517)
# Use primary variables (not derived) with full cleaned set for imputations
names(impute_exslp_1517)
impute_full <- impute_exslp_1517 %>% select(-SEQN, -WTINT2YR, -WTMEC2YR, -SDMVPSU, -SDMVSTRA)
# 10,739 obs of 32 variables
# Check NAs in dataset
colSums(is.na(impute_full))
# There are numerous missing values - 1 vigex, 2 daysvigex, 10 minvigex, 4 modex, 7 daysmodex, 
# 21 minmodex, 71 slphrs, 818 snoring, 671 apnea, 4 usborn, 7381 usyears, 15 educ, 7 marital, 
# 868 income, 158 bmi, 686 waist, 10 smoke, 1397 alcohol, 991 phq01, 981 phq02, 982 phq03, 
# 983 phq04, 984 phq05, 987 phq06, 984 phq07, 987 phq08, 987 phq09
# 
# Too many missing values for `usyears` -> remove
impute_full <- impute_full %>% select(-usyears)
# 10,739 obs of 31 variables

# The following variables have more than 5% missing (>536 missing values):
# snoring, apnea, income, waist, alcohol, phqs -> consider removing
# Only alcohol has more than 10% (>1,073 missing values) missing

# After reviewing, it is preferable to impute outcome (van Ginkel 2020)
# Compare participants with missing sleep hours and not.
missing.slphrs <- impute_full %>% filter(!complete.cases(impute_full$slphrs))
complete.slphrs <- impute_full %>% filter(complete.cases(impute_full$slphrs))
summary(missing.slphrs[,1:6])
summary(complete.slphrs[,1:6])
# Participants with missing sleep hours do far less exercise
summary(missing.slphrs[,7:12])
summary(complete.slphrs[,7:12])
# Also, a bit more apnea (but less snoring), older, slightly different race breakdown
summary(missing.slphrs[,13:18])
summary(complete.slphrs[,13:18])
# Fewer usborn, less education, more live alone, more pregnancy, smaller house, less income
summary(missing.slphrs[,19:24])
summary(complete.slphrs[,19:24])
# Bigger BMI and waist (mean), more smoking, less alcohol

# Write dataset to avoid running this code each time
write.csv(impute_full, "impute_full.csv")

##### MICE
# Read in this dataset to avoid running MICE each time
impute_full <- read_csv("impute_full.csv")
# Index column was added -> remove
ForImputing <- impute_full %>% select(-1)
# 10,739 obs of 31 variables
summary(ForImputing)
# Based on: https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# Ensure variables are in correct format:
str(ForImputing)
# All are read as numbers -> format correctly
ForImputing <- ForImputing %>%
  mutate(
    vigex = as.factor(vigex),
    daysvigex = as.factor(daysvigex),
    minvigex = as.factor(minvigex),
    modex = as.factor(modex),
    daysmodex = as.factor(daysmodex),
    minmodex = as.factor(minmodex),
    slphrs = as.factor(slphrs),
    snoring = as.factor(snoring),
    apnea = as.factor(apnea),
    gender = as.factor(gender),
    age = as.integer(age),
    raceeth = as.factor(raceeth),
    usborn = as.factor(usborn),
    educ = as.factor(educ),
    marital = as.factor(marital),
    pregnancy = as.factor(pregnancy),
    household = as.factor(household),
    income = as.factor(income),
    smoke = as.factor(smoke),
    alcohol = as.factor(alcohol),
    phq01 = as.factor(phq01),
    phq02 = as.factor(phq02),
    phq03 = as.factor(phq03),
    phq04 = as.factor(phq04),
    phq05 = as.factor(phq05),
    phq06 = as.factor(phq06),
    phq07 = as.factor(phq07),
    phq08 = as.factor(phq08),
    phq09 = as.factor(phq09),
  )
str(ForImputing)

##### Call in the MICE
# Look at pattern of missing data
md.pattern(ForImputing)
# 7,352 are complete (68.46%)
aggr_plot <- aggr(ForImputing, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(ForImputing), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(ForImputing[c(7,8)])
marginplot(ForImputing[c(17, 18)])
marginplot(ForImputing[c(19, 20)])

### Do the MICE
imputed <- mice(ForImputing,m=12,maxit=50,meth='pmm',seed=252)
summary(imputed)
save(imputed, file = "imputed.Rdata")
# Check imputed variables
imputed$imp$vigex
imputed$imp$daysvigex
imputed$imp$minvigex
imputed$imp$modex
imputed$imp$daysmodex
imputed$imp$minmodex
imputed$imp$slphrs
imputed$imp$snoring
imputed$imp$apnea
imputed$imp$usborn
imputed$imp$educ
imputed$imp$marital
imputed$imp$income
imputed$imp$bmi
imputed$imp$waist
imputed$imp$smoke
imputed$imp$alcohol
imputed$imp$phq01
imputed$imp$phq09
# Methods
imputed$meth
# Completed data
all_imputed <- complete(imputed, "all")
imputedData.1 <- complete(imputed,1)
write_csv(imputedData.1, "imputedData.1.csv")
imputedData.2 <- complete(imputed,2)
write_csv(imputedData.2, "imputedData.2.csv")
imputedData.3 <- complete(imputed,3)
write_csv(imputedData.3, "imputedData.3.csv")
imputedData.4 <- complete(imputed,4)
write_csv(imputedData.4, "imputedData.4.csv")
imputedData.5 <- complete(imputed,5)
write_csv(imputedData.5, "imputedData.5.csv")
imputedData.6 <- complete(imputed,6)
write_csv(imputedData.6, "imputedData.6.csv")
imputedData.7 <- complete(imputed,7)
write_csv(imputedData.7, "imputedData.7.csv")
imputedData.8 <- complete(imputed,8)
write_csv(imputedData.8, "imputedData.8.csv")
imputedData.9 <- complete(imputed,9)
write_csv(imputedData.9, "imputedData.9.csv")
imputedData.10 <- complete(imputed,10)
write_csv(imputedData.10, "imputedData.10.csv")
imputedData.11 <- complete(imputed,11)
write_csv(imputedData.11, "imputedData.11.csv")
imputedData.12 <- complete(imputed,12)
write_csv(imputedData.12, "imputedData.12.csv")

## Inspecting distribution of original and imputed data
# xyplot(imputed,slphrs ~ vigex+daysvigex+minvigex+modex+daysmodex+minmodex+
#          snoring+apnea+gender+age+raceeth+usborn+educ+marital+pregnancy+
#          household+income+bmi+waist+smoke+alcohol+phq01+phq02+phq03+
#          phq04+phq05+phq06+phq07+phq08+phq09,pch=18,cex=1)

xyplot(imputed, slphrs ~ vigex + modex + gender,pch=18,cex=1)
xyplot(imputed, slphrs ~ daysvigex + minvigex + daysmodex + minmodex,pch=18,cex=1)
xyplot(imputed, slphrs ~ raceeth + educ + household + income,pch=18,cex=1)
xyplot(imputed, slphrs ~ age + bmi + waist,pch=18,cex=1)
# Check that waist and BMI imputations make sense
xyplot(imputed, bmi ~ waist,pch=18,cex=1)
# Excellent. Outliers almost entirely are in the observed (not imputed)

# Very long computation time. Try density plots
densityplot(imputed)

# Compare range of values imputed for BMI and waist
# BMI
summary(impute_full$bmi)
summary(c(imputedData.1$bmi, imputedData.2$bmi, imputedData.3$bmi, imputedData.4$bmi, 
          imputedData.5$bmi, imputedData.6$bmi, imputedData.7$bmi, imputedData.8$bmi, 
          imputedData.9$bmi, imputedData.10$bmi, imputedData.11$bmi, imputedData.12$bmi))
# Range is kept and distribution looks very similar
#Waist
summary(impute_full$waist)
summary(c(imputedData.1$waist, imputedData.2$waist, imputedData.3$waist, imputedData.4$waist, 
          imputedData.5$waist, imputedData.6$waist, imputedData.7$waist, imputedData.8$waist, 
          imputedData.9$waist, imputedData.10$waist, imputedData.11$waist, imputedData.12$waist))
# Range is kept and distribution looks very similar

# Try strip plots
stripplot(imputed, pch = 20, cex = 1.2)

# Pool results from imputed datasets
modelFit1 <- with(imputed, glm(modex ~ age + factor(raceeth) + factor(educ) + factor(marital) + 
                                 bmi + waist,
                               family = binomial(link = "logit")))
summary(pool(modelFit1))
# Compare with standard model
glm.unimputed = glm(modex ~ age + factor(raceeth) + factor(educ) + factor(marital) + bmi + waist,
                    family = binomial(link = "logit"), data = impute_full)
summary(glm.unimputed)
# Relatively similar results.

# Check for missing data in the imputed datasets
sapply(c(imputedData.1, imputedData.2, imputedData.3, imputedData.4, imputedData.5, imputedData.6, 
         imputedData.7, imputedData.8, imputedData.9, imputedData.10, imputedData.11, imputedData.12), 
       function(x) sum(is.na(x)))

##########

