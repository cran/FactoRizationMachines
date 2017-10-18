SVM.train <-
function(data, target, factors=1, intercept=T, iter=100, regular=NULL, stdev=0.1){

  object=list()
  object$vK=factors[1]
  if(object$vK==0) intercept=T

  if(factors[1]!=1&factors[1]!=0) warning("SVM.train does not allow factors -> parameter factors ignored")

  return(learn.FM.model(data=data, target=target, object=object, intercept=intercept, iter=iter, regular=regular, stdev=stdev))
  
}
