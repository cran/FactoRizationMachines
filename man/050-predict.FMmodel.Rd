\name{predict.FMmodel}
\alias{predict.FMmodel}

\title{
   Predict Method for FMmodel Objects
}

\description{
   Function for predicting new data based on a FMmodel object
}

\usage{
   \method{predict}{FMmodel}(object, newdata, truncate = T, ...)
}

\arguments{
  \item{object}{
    a FMmodel object (output of \code{\link{SVM.train}}, \code{\link{FM.train}}, or \code{\link{HoFM.train}})
  }
  \item{newdata}{
    new data for prediction based on the FMmodel object (number of features must match the features of the training data)
  }
  \item{truncate}{
    bool indicating whether the output should be trunceted (\code{T}) order not (\code{F})
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


# Predict with 10 second-order and 5 third-order factor
model=HoFM.train(data.train,target.train)

# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))

}
