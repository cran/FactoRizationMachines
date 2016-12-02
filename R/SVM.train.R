SVM.train <-
function(data, target, factors=1, intercept=T, iter=100, regular=0, stdev=0.1){

  object=list()
  object$vK=factors[1]
  if(object$vK==0) intercept=T
  object$vLambda=regular[1]
  
  if(factors[1]!=1&factors[1]!=0) warning("SVM.train does not allow factors -> parameter factors ignored")

  return(learn.FM.model(data=data, target=target, intercept=intercept, iter=iter, stdev=stdev, object=object))
  
}
