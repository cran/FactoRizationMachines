KnoFM.train <-
function(data, target, multicore=T, silent=F){
  
  if(!silent) cat("\nAutomatic determination started...")
  if(!silent) cat("\n\nInitializing methods...")
  
  evalFM=function(indices){
    
    i=indices[1]
    j=indices[2]
    
    mX.train1=mX.train[-which(vSub==j),]
    vY.train1=vY.train[-which(vSub==j)]
    mX.train2=mX.train[which(vSub==j),]
    vY.train2=vY.train[which(vSub==j)]
    
    factors=rep(0,i)
    factors[1]=1
    factors[i]=1
    
    model=NULL
    rm(model)
    model=HoFM.train(mX.train1,vY.train1,factors)
    RMSE=sqrt(mean((predict(model,mX.train2)-vY.train2)^2))
    
    return(RMSE)
    
  }
  
  evalFM2=function(factors){
    
    model=HoFM.train(mX.train1,vY.train1,c(1,factors))
    RMSE=sqrt(mean((predict(model,mX.train2)-vY.train2)^2))
    
    return(RMSE)
    
  }
  
  iFold=10
  
  mX.train=data
  vY.train=target
  
  vSub=(1:nrow(mX.train))%%iFold+1
  vSub=vSub[sample.int(length(vSub))]
  
  if(!silent) cat(" Done")
  if(!silent) cat("\nDetecting maximum order...")
  
  mRMSE=NULL
  for(i in 1:min(max(rowSums(mX.train)),10)){
    
    mFactors=expand.grid(i,1:iFold)
    
    if(multicore){
      cl=makeCluster(detectCores()-1)
      clusterExport(cl, c("HoFM.train","mX.train","vY.train","vSub"), envir=environment())
      
      vRMSE=parLapply(cl,as.list(data.frame(t(mFactors))),evalFM)
      
      stopCluster(cl)
    } else {
      vRMSE=lapply(as.list(data.frame(t(mFactors))),evalFM)
    }
    
    vRMSE=unlist(vRMSE)
    names(vRMSE)=NULL
    
    if(!is.null(mRMSE)) if(ncol(mRMSE)>2) if(median(vRMSE,na.rm=TRUE)>median(mRMSE[,ncol(mRMSE)],na.rm=TRUE)) break
    
    mRMSE=cbind(mRMSE,vRMSE)
    rownames(mRMSE)=NULL
    colnames(mRMSE)=NULL
    
  }
  
  if(!silent) cat(paste(" Detected maximum order: ",ncol(mRMSE)))
  if(!silent) cat("\nDetecting Factorization Machine...")
  
  vRMSE=apply(mRMSE,2,median,TRUE)
  vRMSE=vRMSE[1]-vRMSE[-1]
  vRMSE[vRMSE<0]=0
  vRMSE=vRMSE^2
  vRatio=vRMSE/sum(vRMSE)
  if(any(is.nan(vRatio))|any(is.na(vRatio))|any(is.infinite(vRatio))) vRatio=rep(1,length(vRMSE))
  
  vSub=which(sample(0:1,nrow(mX.train),replace=T,prob=c(2,8))==1)
  mX.train1=mX.train[vSub,]
  vY.train1=vY.train[vSub]
  mX.train2=mX.train[-vSub,]
  vY.train2=vY.train[-vSub]
  
  
  for(intercept in c(20,40,60,80)){
    
    
    mFactors=round((seq(intercept,intercept-18,-2))%*%t(vRatio),0)
    
    if(multicore){
      cl=makeCluster(detectCores()-1)
      clusterExport(cl, c("HoFM.train","mX.train1","vY.train1","mX.train2","vY.train2"), envir=environment())
      
      vResult=parLapply(cl,as.list(data.frame(t(mFactors))),evalFM2)
      
      stopCluster(cl)
    } else {
      vResult=lapply(as.list(data.frame(t(mFactors))),evalFM2)
    }
    
    vResult=unlist(vResult)
    
    if(rank(vResult)[1]!=1) break
    
  }
  
  factors=mFactors[which(rank(vResult,ties.method="random")==1),]
  factors[factors<1]=1
  
  if(!silent) cat(paste0(" Detected Factorization Machine:  FM(",paste(c(1,factors),collapse="|"),")"))
  if(!silent) cat("\nLearning Factorization Machine model...")
  
  model=HoFM.train(mX.train,vY.train,c(1,factors),iter=500)
  
  if(!silent) cat(" Done")
  if(!silent) cat("\n\nAutomatic determination completed...\n\n")
  
  return(model)
  
}
