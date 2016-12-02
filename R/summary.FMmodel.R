summary.FMmodel <-
function(object, ...){
  
  cat(c(paste("\nfactorization machine model"),
               paste("\nnumber of training examples: ",object$traincases),
               paste("\nnumber of variables:         ",object$variables),
               paste("\nminimum of target vector:    ",object$min.target),
               paste("\nmaximum of target vector:    ",object$max.target),
               paste("\nnumber of factors:           ",paste(object$factors,collapse=" "),
               ""
     )))
  
}
