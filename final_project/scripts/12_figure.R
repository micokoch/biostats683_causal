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
load("unadj.complete.rd.boot.Rdata")
load("adj.complete.rd.boot.Rdata")
# create a data frame
slpex.unadj = data.frame(RiskDifference = c(unadj.complete.rd.boot, 
                                            rnorm(200, 0.042, 0.014), 
                                             rnorm(200, 0.039, 0.014), 
                                             rnorm(200, 0.023, 0.022)),
                         DataAnalysisMethod = rep(c("Real", "Complete", "After MICE", 
                                                    "After MICE with Weights"), each=200), 
                         LogisticRegressionModel = c("Unadjusted"))

slpex.partial = data.frame(RiskDifference = c(rnorm(200, 0.038, 0.016), 
                                               rnorm(200, 0.027, 0.015), 
                                               rnorm(200, 0.007, 0.023)), 
                           DataAnalysisMethod = rep(c("Complete", "After MICE", 
                                                      "After MICE with Weights"), each=200), 
                           LogisticRegressionModel = c("Partially Adjusted"))

slpex.full = data.frame(RiskDifference = c(rnorm(200, 0.036, 0.018), 
                                            rnorm(200, 0.026, 0.016), 
                                            rnorm(200, 0.004, 0.024)), 
                        DataAnalysisMethod = rep(c("Complete", "After MICE", 
                                                   "After MICE with Weights"), each=200), 
                        LogisticRegressionModel = c("Fully Adjusted"))

slpex.nosnoap = data.frame(RiskDifference = c(adj.complete.rd.boot, 
                                              rnorm(200, 0.037, 0.017), 
                                               rnorm(200, 0.028, 0.016), 
                                               rnorm(200, 0.005, 0.023)), 
                           DataAnalysisMethod = rep(c("Real", "Complete", "After MICE", 
                                                      "After MICE with Weights"), each=200), 
                           LogisticRegressionModel = c("Adjusted"))

slpex.results <- bind_rows(slpex.unadj, slpex.nosnoap)

slpex.results$LogisticRegressionModel <- factor(slpex.results$LogisticRegressionModel, 
                                                levels=c("Unadjusted", "Adjusted"), 
                                                ordered = TRUE)
slpex.results$DataAnalysisMethod <- factor(slpex.results$DataAnalysisMethod, 
                                           levels = c("Real", "Complete", "After MICE", 
                                                                "After MICE with Weights"), ordered = TRUE)

ggplot(slpex.results, aes(x=RiskDifference, 
                          y=LogisticRegressionModel, fill=DataAnalysisMethod)) + 
  geom_boxplot() +
  scale_y_discrete(limits=c("Adjusted", "Unadjusted"))

p <- slpex.results %>% 
  mutate(DataAnalysisMethod = fct_reorder(DataAnalysisMethod, desc(DataAnalysisMethod))) %>% 
  ggplot(aes(x=RiskDifference, y=LogisticRegressionModel, fill=DataAnalysisMethod)) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "darkblue") +
  geom_violin(draw_quantiles = c(0.025, 0.05, 0.5, 0.95, 0.975)) + 
  scale_fill_brewer(palette="Accent") +
  scale_y_discrete(limits=c("Adjusted", "Unadjusted")) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_light() + 
  theme(axis.title.x = element_text(color="black"), 
        axis.title.y = element_blank(), 
        legend.position="bottom", 
        legend.title = element_blank(), 
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black")) + 
  xlab("Estimated Risk Difference (mean, 90% and 95% CI)")
p

ggsave("slpex2.jpg")

q <- slpex.results %>% 
  mutate(DataAnalysisMethod = fct_reorder(DataAnalysisMethod, desc(DataAnalysisMethod))) %>% 
  ggplot(aes(x=RiskDifference, y=LogisticRegressionModel, fill=DataAnalysisMethod)) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "darkblue") +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_brewer(palette="Accent") +
  scale_y_discrete(limits=c("Adjusted", "Unadjusted")) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_light() + 
  theme(axis.title.x = element_text(color="black"),
        axis.title.y = element_blank(), 
        legend.position="bottom", 
        legend.title = element_blank(), 
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black")) + 
  xlab("Estimated Risk Difference")
q

ggsave("physactslp2.jpg")

