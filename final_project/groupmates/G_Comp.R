library(dplyr)
set.seed(252)
ObsData <- read.csv("slpexcov1517.csv")

ObsData <- ObsData %>% select(-SEQN, -exminwk, -slphrs, -household, -income, -snoring,
                              -apnea, -bmicat, -smoke, -alcohol, -phq9)

ObsData <- ObsData %>% mutate(A = targetex) %>% mutate(Y = targetslp) %>% 
  select(-targetex, -targetslp)

ObsData <- na.omit(ObsData)

#ObsData$A <- as.factor(ObsData$A) 
#ObsData$raceeth <- as.factor(ObsData$raceeth) 
#ObsData$educ <- as.factor(ObsData$educ) 
#ObsData$marital <- as.factor(ObsData$marital)
#ObsData$depressed <- as.factor(ObsData$depressed)

reg.model <- glm(Y ~ age + factor(A) + factor(raceeth) + factor(educ) + 
                   factor(marital) + bmi + waist + factor(depressed),
                 family = "binomial", data = ObsData)
summary(reg.model)

txt <- control <- ObsData
txt$A <- 1
control$A <- 0

predictY.txt <- predict(reg.model, newdata = txt, type = "response")
predictY.control <- predict(reg.model, newdata = control, type = "response")

mean(predictY.txt - predictY.control)



