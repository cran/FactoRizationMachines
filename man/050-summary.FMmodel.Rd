\name{summary.FMmodel}
\alias{summary.FMmodel}
\alias{print.FMmodel}

\title{
   Summary and Print Method for FMmodel Objects
}

\description{
   Function generating the summary of a FMmodel object. 
}

\details{
  The summary contains for instance:
  
   - the number of training examples the model was build with,
   
   - the number of variables (features) the model considers,
   
   - the minimum value of the target vector elements (to truncate the prediction),
   
   - the maximum value of the target vector elements (to truncate the prediction),
   
   - the number of factors for each considered order:
     the first element specifies whether linear weights are used (\code{1}) or not (\code{0}),
     the second element specifies the number of parameters factorizing the second-order,
     the third element specifies the number of parameters factorizing the third-order.
}

\usage{
\method{summary}{FMmodel}(object, ...)

\method{print}{FMmodel}(x, ...)
}

\arguments{

  \item{object}{
    a FMmodel object (output of \code{\link{SVM.train}}, \code{\link{FM.train}}, or \code{\link{HoFM.train}})
  }

  \item{x}{
    a FMmodel object (output of \code{\link{SVM.train}}, \code{\link{FM.train}}, or \code{\link{HoFM.train}})
  }

  \item{\dots}{
    additional arguments
  }
}

\seealso{
  \code{\link{SVM.train}}, 
  \code{\link{FM.train}}, 
  \code{\link{HoFM.train}}
}