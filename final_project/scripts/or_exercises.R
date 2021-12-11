# Odds ratio to risk difference exercises
# https://www.r-bloggers.com/2021/06/
#estimating-a-risk-difference-and-confidence-intervals-using-logistic-regression/


set.seed(287362)

library(simstudy)
library(data.table)
library(ggplot2)
library(ggthemes)
library(parallel)

def <- defDataAdd(varname = "x1", formula = "..mu_x", variance = 8, dist = "beta")
def <- defDataAdd(def, varname = "x", formula = "x1 - 0.5", dist = "nonrandom")
def <- defDataAdd(def, varname = "y", 
                  formula = "-2 + log(2.5) * rx + 1.5 * x",
                  dist = "binary", link="logit")

mu_x = 0.2

dd_2 <- genData(500)
dd_2 <- trtAssign(dd_2, grpName = "rx")
dd_2 <- addColumns(def, dd_2)
ggplot(data = dd_2, aes(x = x)) +
  geom_histogram(fill="#9ec785", binwidth = 0.05, boundary = 0) +
  scale_x_continuous(limits = c(-.55, .55), breaks = seq(-.5, .5, by = .25)) +
  theme(panel.grid = element_blank())

glmfit <- glm(y ~ rx + x, data = dd_2, family = "binomial")

newdata <- dd_2[, .(rx=1, x)]
p1 <- mean(predict(glmfit, newdata, type = "response"))

newdata <- dd_2[, .(rx=0, x)]
p0 <- mean(predict(glmfit, newdata, type = "response"))

c(p1, p0)
## [1] 0.152 0.068

newtest <- dd_2