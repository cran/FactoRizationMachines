HoFM.train <-
function(data, target, factors=c(1,10,5), intercept=T, iter=100, regular=0, stdev=0.1){
  
  object=list()
  object$vK=factors
  if(length(regular)==1) regular=rep(regular,length(factors))
  object$vLambda=regular
  length(object$vLambda)=length(factors)
  
  if(length(factors)>3) warning("HoFM.train only supports up to third-order factors -> parameter factors partly ignored")
  
  return(learn.FM.model(data=data, target=target, intercept=intercept, iter=iter, stdev=stdev, object=object))
  
}
