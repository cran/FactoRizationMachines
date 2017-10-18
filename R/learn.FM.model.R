learn.FM.model <-
function(data, target, object, intercept=T, iter=100, regular=NULL, stdev=0.1){
  
  if(object$vK[1]!=1 & object$vK[1]!=0) warning("fist element of factors must either be 0 or 1")
  if(object$vK[1]>1) object$vK[1]=1
  if(object$vK[1]<1) object$vK[1]=0
  
  if(is.null(object$lV3)) object$lV3=0
  
  if(!is.null(regular)) {
    if(length(regular)==1) regular=rep(regular,length(object$vK))
    object$vLambda=regular
    length(object$vLambda)=length(object$vK)
    object$bMCMC=FALSE
  } else {
    object$vLambda=rep(0,length(object$vK))
    object$bMCMC=TRUE
  }

  if(is.data.frame(data)) data=as.matrix(data)
  data=as(data,"dgTMatrix")
  object$variables=data@Dim[2]
  object$traincases=data@Dim[1]
  object$mX=cbind(data@i,data@j,data@x)
  object$vY=(target)
  # object$vY=scale(target)*0.1
  
  if(length(target)!=nrow(data)) stop("number of training cases does not match between feature matrix and target vector")
  
  if(!is.numeric(object$mX)) stop("feature matrix contains non-numeric elements")
  if(!is.numeric(object$vY)) stop("target vector contains non-numeric elements")
  
  if(any(is.na(object$vY)) | any(is.nan(object$vY)) | any(is.infinite(object$vY)) ) stop("target vector contains na, nan, or inf element")
  if(any(is.na(object$mX)) | any(is.nan(object$mX)) | any(is.infinite(object$mX)) ) warning("feature matrix contains na, nan, or inf element")
  
  if(sd(target)==0) warning("target vector contains only identical values")
  
  object=c(object,bIntercept=intercept,iIter=iter,dStdev=stdev)
  
  object=trainFM(object)
  # object$mean=mean(target)
  # object$sd=sd(target)
  
  if(any(is.na(object$weights)) | any(is.nan(object$weights)) | any(is.infinite(object$weights)) ) warning("model parameter contain na, nan, or inf element")
  
  return(object)
  
}
