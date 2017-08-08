predict.FMmodel <-
function(object, newdata, truncate=T, ...){
  
  if(object$variables!=ncol(newdata)) stop(paste0("number of features (p=",ncol(newdata),") does not match with model (p=",object$variables,")"))
  
  if(is.data.frame(newdata)) newdata=as.matrix(newdata)
  newdata=as(newdata,"dgTMatrix")
  object$traincases=newdata@Dim[1]
  object$mX=cbind(newdata@i,newdata@j,newdata@x)
  object$truncate=truncate
  
  prediction=predictFM(object)
  
  return(prediction)
  
}
