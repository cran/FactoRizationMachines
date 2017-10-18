\name{KnoFM.train}
\alias{KnoFM.train}

\title{
   Knowledge-extracting or adaptive-order Factorization Machine
}

\description{
   \code{KnoFM.train} is a method training a knowledge-extracting Factorization Machine. 
}
\usage{
   KnoFM.train(data, target, multicore = T, silent = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    an object of class \code{dgTMatrix}, \code{matrix} or \code{data.frame} (or an object coercible to \code{dgTMatrix}): 
      a matrix containing training data, each row representing a training example and each column representing a feature. 
  }
  \item{target}{
    \code{numeric}: vector specifying the target value of each training example (length must match rows of object data).
  }
  \item{multicore}{
     \code{logical}: specifying whether multiple cores should be used.
  }
  \item{silent}{
     \code{logical}: specifying whether progess should be printed.
  }
}

\references{
  [1] J. Knoll , J. Stuebinger, and M. Grottke, Exploiting social media with higher-order Factorization Machines: Statistical arbitrage on high-frequency data of the S&P 500. FAU Discussion Papers in Economics, University of Erlangen-Nuernberg, 2017.

  [2] J. Knoll, Recommending with Higer-Order Factorization Machines, Research and Development in Intelligent Systems XXXIII, 2016.
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

# Predict with an adaptive-order Factorization Machine
# using one CPU core  and printing progress
model=KnoFM.train(data.train,target.train,FALSE,FALSE)

# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))


}
}

