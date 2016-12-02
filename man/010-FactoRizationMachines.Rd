\name{FactoRizationMachines}
\alias{FactoRizationMachines}
\docType{package}

\title{
  \packageTitle{FactoRizationMachines}
}

\description{
  Implementation of three factorization-based machine learning approaches: 
  
  - Support Vector Machines (\code{\link{SVM.train}}) with a linear kernel, 
  
  - second-order Factorization Machines [2] (\code{\link{FM.train}}), 
  
  - and higher-order Factorization Machines [1] (\code{\link{HoFM.train}}).
  
  Further informations about Factorization Machines are provided by the papers in the references.
}

\details{
  This package includes the following methods:
  
  - \code{\link{SVM.train}}: Method training a Support Vector Machine,
  
  - \code{\link{FM.train}}: Method training a second-order Factorization Machine,

  - \code{\link{HoFM.train}}: Method training a higher-order Factorization Machine,
  
  - \code{\link{predict.FMmodel}}: Predict Method for FMmodel Objects,
  
  - \code{\link{summary.FMmodel}} and \code{\link{print.FMmodel}}: Summary and Print Method for FMmodel Objects.
   
  To date the learning method alternating least squares (\code{"als"}) and the task regression (\code{"r"}) is supported. 
  Consequently, regularization is suggested in most of the cases.
  Next steps are to implement the Monte Carlo Markov Chain method (\code{"mcmc"}) to simplify regularization.
  Furthermore, the task classifiction (\code{"c"}) will be supported in the future.
}

\author{
  Maintainer: Julian Knoll <julian.knoll@th-nuernberg.de>
}

\references{
  [1] J. Knoll, Recommending with Higer-Order Factorization Machines, Research and Development in Intelligent Systems XXXIII, 2016.
  
  [2] S. Rendle, Factorization Machines with libFM, ACM Transactions onIntelligent Systems and Technology, 3, 2012.
}

\keyword{ package }
\keyword{ Factorization Machine }
\keyword{ Matrix Factorization }
\keyword{ Machine Learning }
\keyword{ Recommender }

\seealso{
  \code{\link{SVM.train}}, 
  \code{\link{FM.train}}, 
  \code{\link{HoFM.train}}, 
  \code{\link{predict.FMmodel}}
}

\examples{
\dontrun{

# Load libraries
library(FactoRizationMachines)
library(Matrix)

# Load MovieLens 100k data set
ml100k=as.matrix(read.table("http://files.grouplens.org/datasets/movielens/ml-100k/u.data"))
user=ml100k[,1]
items=ml100k[,2]+max(user)
wdays=(as.POSIXlt(ml100k[,4],origin="1970-01-01")$wday+1)+max(items)

# Transform MovieLens 100k to feature form
data=sparseMatrix(i=rep(1:nrow(ml100k),3),j=c(user,items,wdays),giveCsparse=F)
target=ml100k[,3]

# Subset data to training and test data
set.seed(123)
subset=sample.int(nrow(data),nrow(data)*.8)
data.train=data[subset,]
data.test=data[-subset,]
target.train=target[subset]
target.test=target[-subset]

# Predict ratings with Support Vector Machine with linear kernel
model=SVM.train(data.train,target.train)
# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))

# Predict ratings with second-order Factorization Machine 
# with second-order 10 factors (default) and regularization
model=FM.train(data.train,target.train,regular=0.1)
# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))

# Predict ratings with higher-order Factorization Machine 
# with 3 second-order and 1 third-order factor and regularization
model=HoFM.train(data.train,target.train,c(1,3,1),regular=0.1)
# RMSE resulting from test data prediction
sqrt(mean((predict(model,data.test)-target.test)^2))

}
}
