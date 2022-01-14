####### Figure

##### Libraries
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(ggthemes)

##### Preliminaries
# setwd("final_project")
getwd()
set.seed(252)

##### Boxplot
# create a data frame
slpex.unadj = data.frame(RiskDifference = c(rnorm(200, 0.042, 0.014), 
                                             rnorm(200, 0.039, 0.014), 
                                             rnorm(200, 0.023, 0.022)),
                         DataAnalysisMethod = rep(c("Unimputed", "Multiple Imputation (MI)", 
                                                    "MI + Complex Survey Analysis"), each=200), 
                         LogisticRegressionModel = c("Unadjusted"))

slpex.partial = data.frame(RiskDifference = c(rnorm(200, 0.038, 0.016), 
                                               rnorm(200, 0.027, 0.015), 
                                               rnorm(200, 0.007, 0.023)), 
                           DataAnalysisMethod = rep(c("Unimputed", "Multiple Imputation (MI)", 
                                                      "MI + Complex Survey Analysis"), each=200), 
                           LogisticRegressionModel = c("Partially Adjusted"))

slpex.full = data.frame(RiskDifference = c(rnorm(200, 0.036, 0.018), 
                                            rnorm(200, 0.026, 0.016), 
                                            rnorm(200, 0.004, 0.024)), 
                        DataAnalysisMethod = rep(c("Unimputed", "Multiple Imputation (MI)", 
                                                   "MI + Complex Survey Analysis"), each=200), 
                        LogisticRegressionModel = c("Fully Adjusted"))

slpex.nosnoap = data.frame(RiskDifference = c(rnorm(200, 0.037, 0.017), 
                                               rnorm(200, 0.028, 0.016), 
                                               rnorm(200, 0.005, 0.023)), 
                           DataAnalysisMethod = rep(c("Unimputed", "Multiple Imputation (MI)", 
                                                      "MI + Complex Survey Analysis"), each=200), 
                           LogisticRegressionModel = c("nosnoap"))

slpex.results <- bind_rows(slpex.unadj, slpex.partial, slpex.full)
slpex.results$LogisticRegressionModel <- factor(slpex.results$LogisticRegressionModel, 
                                                levels=c("Unadjusted", "Partially Adjusted", "Fully Adjusted"), 
                                                ordered = TRUE)
slpex.results$DataAnalysisMethod <- factor(slpex.results$DataAnalysisMethod, 
                                           levels = c("Unimputed", "Multiple Imputation (MI)", 
                                                                "MI + Complex Survey Analysis"), ordered = TRUE)

ggplot(slpex.results, aes(x=RiskDifference, 
                          y=LogisticRegressionModel, fill=DataAnalysisMethod)) + 
  geom_boxplot() +
  scale_y_discrete(limits=c("Fully Adjusted", "Partially Adjusted", "Unadjusted"))

p <- slpex.results %>% 
  mutate(DataAnalysisMethod = fct_reorder(DataAnalysisMethod, desc(DataAnalysisMethod))) %>% 
  ggplot(aes(x=RiskDifference, y=LogisticRegressionModel, fill=DataAnalysisMethod)) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "darkblue") +
  geom_violin(draw_quantiles = c(0.025, 0.05, 0.5, 0.95, 0.975)) + 
  scale_fill_brewer(palette="Accent") +
  scale_y_discrete(limits=c("Fully Adjusted", "Partially Adjusted", "Unadjusted")) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_light() + 
  theme(plot.title = element_text(color="black", size=16, face="bold.italic", hjust=0.5), 
        axis.title.x = element_text(color="black", size=13, face="bold"),
        axis.title.y = element_text(color="black", size=13, face="bold"), 
        legend.position="bottom", 
        legend.title = element_text(face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black")) + 
  ggtitle("Physical activity & sleep among men 20-64yrs: \nAn exercise in weighting ") + 
  xlab("Risk Difference (mean, 90% and 95% CI)") + 
  ylab("Logistic Regression Model") +
  labs(fill = "Data Analysis Method: ")
p
ggsave("slpex.jpg")

q <- slpex.results %>% 
  mutate(DataAnalysisMethod = fct_reorder(DataAnalysisMethod, desc(DataAnalysisMethod))) %>% 
  ggplot(aes(x=RiskDifference, y=LogisticRegressionModel, fill=DataAnalysisMethod)) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "darkblue") +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_brewer(palette="Accent") +
  scale_y_discrete(limits=c("Fully Adjusted", "Partially Adjusted", "Unadjusted")) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_light() + 
  theme(plot.title = element_text(color="black", size=16, face="bold.italic", hjust=0.5), 
        axis.title.x = element_text(color="black", size=13, face="bold"),
        axis.title.y = element_text(color="black", size=13, face="bold"), 
        legend.position="bottom", 
        legend.title = element_text(face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black")) + 
  ggtitle("Physical activity & sleep among men 20-64yrs: \nAn exercise in weighting ") + 
  xlab("Risk Difference") + 
  ylab("Logistic Regression Model") +
  labs(fill = "Data Analysis Method: ")
q

ggsave("physactslp.jpg")

