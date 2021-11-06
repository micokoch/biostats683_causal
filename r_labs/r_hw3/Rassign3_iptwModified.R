# SECTION 4 of R Assign 3

set.seed(1)

# The true value of the conditional mean outcome E_0[Y|A,W]
true.meanY.AW <- function(A,W){
  1000 + plogis(W*A)
}
# The true value of propensity score Pr(A=1|W)
true.prob.AW <- function(W){
  0.2 + 0.6*W
}

# A function which returns a data frame with n i.i.d. observations from P_0
gen.data <- function(n){
  # note this is a shortcut way of coding that skips generating the Us
  # first and then generating the endogenous variables deterministically
	W <- rbinom(n, 1, 1/2)
	A <- rbinom(n, 1, true.prob.AW(W=W))
	Y <- 1000 + rbinom(n, 1, true.meanY.AW(A=A,W=W) - 1000)
	return(data.frame(W=W,A=A,Y=Y))
}

# samples size
n<- 1000
# Number of Monte Carlo draws
R <- 2000
# Matrix of estimates from IPTW, modified Horvitz-Thompson, and my.est
est <- matrix(NA,nrow=R,ncol=3)
colnames(est) <- c('IPTW','Modifed HT','my.est')
for(r in 1:R){
	# Generate data with sample size
	ObsData <- gen.data(n)
	W <- ObsData$W
	A <- ObsData$A
	Y <- ObsData$Y
	# True propensity score P_0(A=1|W)
	pscore <- true.prob.AW(W=W)
	# IPTW estimate
	IPTW.est <- mean(A/pscore*Y)
	# Modified Horvitz-Thompson estimate
	HT.est <- mean(A/pscore*Y)/mean(A/pscore)
	# You should replace the NA below with your own estimator
	my.est <- NA
	# Put the estimates into the est matrix
	est[r,] <- c(IPTW.est, HT.est, my.est)
}

# Calculate the true value of sum_w E[Y|A=1,W=w) P(W=w)
truth <- .5*true.meanY.AW(A=1, W=0) + .5*true.meanY.AW(A=1,W=1)
# note: we know P_0(W=1) = 0.5
truth

# Calculate the estimated bias, variance, and MSE
est.bias <- colMeans(est) - truth
est.var <- apply(est,2,var)
est.mse <- est.bias^2 + est.var

# The estimators have (estimated) bias:
est.bias
# The estimators have (estimated) variance:
est.var
# The estimators have (estimated) MSE:
est.mse
