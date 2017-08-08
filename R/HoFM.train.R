HoFM.train <-
function(data, target, factors=c(1,10,5), intercept=T, iter=100, regular=0, stdev=0.1){
  
  object=list()
  object$vK=factors
  
  if(length(factors)>10) warning("HoFM.train only supports up to tenth-order factors -> parameter factors partly ignored")
  
  return(learn.FM.model(data=data, target=target, object=object, intercept=intercept, iter=iter, regular=regular, stdev=stdev))
  
}
