\name{HoFM.train}
\alias{HoFM.train}

\title{
   Method training a higher-order Factorization Machine
}

\description{
  \code{HoFM.train} is a method training a higher-order Factorization Machine. 
  
  \code{factors} specifies the model parameters of the Factorization Machine:
  the first element specifies whether linear weights are used (\code{1}) or not (\code{0}),
  the second element specifies the number of parameters factorizing the second-order,
  the third element specifies the number of parameters factorizing the third-order,
  ...,
  up to the tenth element specifying the number of parameters factorizing the tenth-order.

}

\usage{
  HoFM.train(data, target, factors = c(1, 10, 5), intercept = T,
  iter = 100, regular = NULL, stdev = 0.1)
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
    \code{numeric}: vector specifying the number of factors for each considered order:
     the first element specifies whether linear weights are used (\code{1}) or not (\code{0}),
     the second element specifies the number of parameters factorizing the second-order,
     the third element specifies the number of parameters factorizing the third-order,
     ...,
     up to the tenth element specifying the number of parameters factorizing the tenth-order.
  }
  \item{intercept}{
    \code{logical}: specifying whether a global intercept is used (\code{TRUE}) or not (\code{FALSE}).
  }
  \item{iter}{
    \code{integer}: the number of iterations the learning method is applied.
  }
  \item{regular}{
    \code{numeric}: regularization value for each order corresponding to factors. If one value, each order is regularized with this value, 
    otherwise each element of the vector specifies the regularization value of the corresponding order.
    If regular is NULL, automatic regularization using Markov Chain Monte Carlo (MCMC) method is applied.
  }
  \item{stdev}{
    \code{numeric}: the standard deviation used to initialize the model parameters.
  }
}

\references{
  [1] J. Knoll, Recommending with Higer-Order Factorization Machines, Research and Development in Intelligent Systems XXXIII, 2016.
  
  [2] S. Rendle, Factorization Machines with libFM, ACM Transactions onIntelligent Systems and Technology (TIST), 3, 2012.
}

\seealso{
  \code{\link{FactoRizationMachines}}
}

\examples{
\dontrun{

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


# Predict with 7 second-order and 2 third-order factors
#  with MCMC regularization
model=HoFM.train(data.train,target.train,c(1,7,2))

# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))


# Predict with 10 second-order and 5 third-order factor
#  with MCMC regularization
model=HoFM.train(data.train,target.train)

# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))


# Predict with 10 second-order and 5 third-order factor
#  with manual regularization
model=HoFM.train(data.train,target.train,regular=0.1)

# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))

}
}
