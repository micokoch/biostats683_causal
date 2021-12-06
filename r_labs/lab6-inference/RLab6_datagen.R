#----------------------------------------
# generateData - function to generate the data
# input: number of draws, whether or not there is a treatment effect,
#   whether or not to return the counterfactuals or the observed data
# output: counterfactuals or the observed data
#----------------------------------------
generateData<- function(n, effect=T, get.psi.star=F){
  
  W1 <- rbinom(n, size=1, prob=0.5) 
  W2 <- rbinom(n, size=1, prob=0.5) 
  W3 <- runif(n, min=0, max=1) 
  W4 <- runif(n, min=0, max=5) 
  
  pscore <- plogis(1+2*W1*W2-W4)
  A<- rbinom(n, size=1, prob=pscore) 
  
  U.Y<- runif(n,0,1)
  # generate the counterfactual outcome with A=0
  Y.0<- generateY(W1=W1, W2=W2, W3=W3, W4=W4, A=0, U.Y=U.Y)
  
  if(!effect){ # if there is no effect, the counterfactual under txt = 
    # the counterfactual under the control
    Y.1<- Y.0
  }else{ # otherwise, generated the counterfactual outcome with A=1
    Y.1<- generateY(W1=W1, W2=W2, W3=W3, W4=W4, A=1, U.Y=U.Y)
  }
  
  # assign the observed outcome based on the observed exposure
  Y<- rep(NA, n)
  Y[A==1]<- Y.1[A==1]
  Y[A==0]<- Y.0[A==0]
  
  if(get.psi.star){
    # return the counterfactual outcomes
    data<- data.frame(Y.1, Y.0)
  } else {
    # return the observed data: O=(W,A,Y)
    data<- data.frame(W1,W2,W3,W4,A,Y) 
  }
  data
}

#---------
# generateY: function to generate the outcome given the
#   baseline covariates, exposure and background error U.Y
#------------------
generateY<- function(W1, W2, W3, W4, A, U.Y){
  prob <- plogis(-1.5+A-2*W3+0.5*W4+5*W1*W2*W4)
  as.numeric(U.Y < prob)
}