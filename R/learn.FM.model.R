learn.FM.model <-
function(data, target, intercept, iter, stdev, silent, object){
  
  if(object$vK[1]>1 | object$vK[1]<0) warning("fist element of factors must either be 0 or 1")
  if(object$vK[1]>1) object$vK[1]=1
  if(object$vK[1]<1) object$vK[1]=0
  
  object=c(object,bIntercept=intercept,iIter=iter,dStdev=stdev)

  if(is.data.frame(data)) data=as.matrix(data)
  data=as(data,"dgTMatrix")
  object$mX=cbind(data@i,data@j,data@x)
  object$vY=target
  
  if(length(target)!=nrow(data)) stop("number of training cases does not match between feature matrix and target vector")
  
  if(!is.numeric(object$mX)) stop("feature matrix contains non-numeric elements")
  if(!is.numeric(object$vY)) stop("target vector contains non-numeric elements")
  
  if(any(is.na(object$vY)) | any(is.nan(object$vY)) | any(is.infinite(object$vY)) ) stop("target vector contains na, nan, or inf element")
  if(any(is.na(object$mX)) | any(is.nan(object$mX)) | any(is.infinite(object$mX)) ) warning("feature matrix contains na, nan, or inf element")
  
  object=trainFM(object)
  
  if(any(is.na(object$weights)) | any(is.nan(object$weights)) | any(is.infinite(object$weights)) ) warning("model parameter contain na, nan, or inf element")
  
  return(object)
  
}
