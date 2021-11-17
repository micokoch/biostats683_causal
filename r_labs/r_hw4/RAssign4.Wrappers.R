# ##############
SL.glm.EstA<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {}
  if(family$family=='gaussian'){
    fit.glm<- glm(Y~ 1, family=family)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstA')
  return(out)
}
# ##############
SL.glm.EstB<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {}
  if(family$family=='gaussian'){
  	 fit.glm<- glm(Y~ W1+ W2 + W3 + W4 + W5, data=X, family=family)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
   }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstB')
  return(out)
}
#############
# Estimator C
SL.glm.EstC <- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {}
  if(family$family=='gaussian'){
    fit.glm<- glm(Y~ W1+ W2 + W3 + W4 + W5 +W2*W5, data=X, family=family)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)

  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstC')
  return(out)
}
#####################
# Estimator D
SL.glm.EstD<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {
  }
  if(family$family=='gaussian'){
    fit.glm<- glm(Y~ W1+ W2 + W3 + W4 + W5 +W1*W3 + W2*W5, data=X, family=family)
    pred <- predict(fit.glm, newdata=newX, type='response')
    fit<- list(object=fit.glm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('SL.glm.EstD')
  return(out)
}
