\name{SVM.train}
\alias{SVM.train}

\title{
   Method training a Support Vector Machine
}

\description{
  \code{SVM.train} is a method training a Support Vector Machine with a linear kernel.
  
  \code{factors} specifies whether linear weights are used (\code{1}) or not (\code{0}).
  If linear weights are not used \code{intercept} is set to \code{TRUE}.
   
  To date the learning method alternating least squares (\code{"als"}) and the task regression (\code{"r"}) is supported. 
  Consequently, regularization is suggested in most of the cases.
  Next steps are to implement the Monte Carlo Markov Chain method (\code{"mcmc"}) to simplify regularization.
  Furthermore, the task classifiction (\code{"c"}) will be supported in the future.
}

\usage{
  SVM.train(data, target, factors = 1, intercept = T, 
  iter = 100, regular = 0, stdev = 0.1)
}


\arguments{
  \item{data}{
    an object of class \code{dgTMatrix}, \code{matrix} or \code{data.frame} (or an object coercible to \code{dgTMatrix}): 
      a matrix containing training data, each row representing a training example and each column representing a feature. 
  }
  \item{target}{
    \code{numeric}: vector specifying the target value of each training example (length must match rows of object data).
  }
  \item{factors}{
    either \code{0} or \code{1}: specifying whether linear weights are used (\code{1}) or not (\code{0}).
    If linear weights are not used \code{intercept} is set to \code{TRUE}.
  }
  \item{intercept}{
    \code{logical}: specifying whether a global intercept is used (\code{TRUE}) or not (\code{FALSE}).
  }
  \item{iter}{
    \code{integer}: the number of iterations the learning method is applied.
  }
  \item{regular}{
    \code{numeric}: regularization value for the linear weights.
  }
  \item{stdev}{
    \code{numeric}: the standard deviation used to initialize the model parameters.
  }
}


\seealso{
  \code{\link{FactoRizationMachines}}
}

\examples{

### Example to illustrate the usage of the method
### Data set very small and not sparse, results not representative
### Please study major example in general help 'FactoRizationMachines'

# Load data set
library(FactoRizationMachines)
library(MASS)
data("Boston")

# Subset data to training and test data
set.seed(123)
subset=sample.int(nrow(Boston),nrow(trees)*.8)
data.train=Boston[subset,-ncol(Boston)]
target.train=Boston[subset,ncol(Boston)]
data.test=Boston[-subset,-ncol(Boston)]
target.test=Boston[-subset,ncol(Boston)]


# Predict with linear weights and intercept
model=SVM.train(data.train,target.train)

# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))


# Predict with linear weights but without intercept
model=SVM.train(data.train,target.train,intercept=FALSE)

# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))


# Predict with linear weights and regularization
model=SVM.train(data.train,target.train,regular=0.1)

# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))

}
