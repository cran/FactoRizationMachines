FM.train <-
function(data, target, factors=c(1,10), intercept=T, iter=100, regular=0, stdev=0.1){
  
  object=list()
  if(length(factors)>2) object$vK=factors[1:2] else object$vK=factors

  if(length(factors)>2) warning("FM.train only supports second-order factors -> parameter factors partly ignored\nsee command HoFM.train for higher-order support")
  
  return(learn.FM.model(data=data, target=target, object=object, intercept=intercept, iter=iter, regular=regular, stdev=stdev))
  
}
