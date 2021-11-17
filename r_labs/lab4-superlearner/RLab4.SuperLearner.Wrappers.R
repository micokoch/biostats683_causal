
# ##############
# Estimator A
SL.glm.EstA<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {
    fit.glm<- glm(Y~ W1*W3 + W4sq, data=X, family=family)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  if(family$family=='gaussian'){}
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstA')
  return(out)
}
#############
# Estimator B
SL.glm.EstB<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {
    fit.glm<- glm(Y~ W1+ logW2 + W3*W4, data=X, family=family)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  if(family$family=='gaussian'){}
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstB')
  return(out)
}
#####################
# Estimator C
SL.glm.EstC<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {
    fit.glm<- glm(Y~ W1*W2*W4, data=X, family=family)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  if(family$family=='gaussian'){}
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstC')
  return(out)
}
###################
# Estimator D
SL.glm.EstD<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {
    fit.glm<- glm(Y~  W1*sinW2sq+ logW4, data=X, family=family)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  if(family$family=='gaussian'){}
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstD')
  return(out)
}