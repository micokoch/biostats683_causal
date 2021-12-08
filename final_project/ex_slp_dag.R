library(tidyverse)
library(ggdag)
library(ggplot2)
library(colorBlindness)
library(here)

exer_sleep <- dagify(y ~ w1 + w2 + w3 + a + U,
                     a ~ w1 + w2 + w3 + U,
                     w3 ~ w1 + w2 + U,
                     w2 ~ w1 + U,
                     w1 ~ U,
                     labels = c("y" = "Target Sleep",
                                "a" = "Target Exercise",
                                "w1" = "age, race/ethnicity, married, education",
                                "w2" = "baseline BMI, baseline waist",
                                "w3" = "depression",
                                "U" = "Unmeasured all"),
                     exposure = "a",
                     outcome = "y",
                     coords = list(x = c(y = 5, a = 3, w1 = 1, w2 = 2, w3 = 4, U = 4),
                                   y = c(y = 1, a = 1, w1 = 2, w2 = 2.5, w3 =2, U = 3))) %>% 
  tidy_dagitty() %>% 
  dplyr::mutate(Variable = case_when(
    name == "y" ~ "Sleep > 6 h/night",
    name == "a" ~ "Exercise ≥ 150 min/wk",
    name == "w1" ~ "Age, Race/Ethnicity, Marriage, Education",
    name == "w2" ~ "Baseline BMI, Baseline Waist Size",
    name == "w3" ~ "Depression",
    name == "U" ~ "Unmeasured (all)"))

exer_sleep_dag <- exer_sleep %>% 
  ggplot(aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_point(aes(color = Variable)) +
  geom_dag_edges() +
  geom_dag_text() +
  theme_dag() +
  scale_color_viridis_d()+
  ggtitle("Causal DAG \nEffect of Exercise on Sleep") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  guides(color = guide_legend(override.aes = list(size = 8)))

exer_sleep_dag

ggsave(here("final_project/exer_sleep_dag.jpg"), width = 8, height = 6, units = "in")

