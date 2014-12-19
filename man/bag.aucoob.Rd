\name{bag.aucoob}
\alias{bag.aucoob}
\title{
AUC on the Out Of Bag samples
}
\description{
Compute the AUC on the OOB samples of the bagging procedure for the binomial family. The true and false positive rates are also returned and could be helpfull for plotting the ROC curves.  
}
\usage{
bag.aucoob(bag_pltr, xdata, Y.name)
}
\arguments{
  \item{bag_pltr}{ The output of the function \code{\link{bagging.pltr}}}
  \item{xdata}{The learning dataset containing the dependent variable, the confounding variables and the predictors variables}
  \item{Y.name}{The name of the binary dependent variable}
}
\details{
The thresshold values used for computing the AUC are defined when building the bagging predictor. see \code{\link{bagging.pltr}} for the convenient parameterization.  
}
\value{ A list of 4 elements
\item{AUCOOB}{the AUC computed on OOB samples of the Bagging procedure}
\item{TPR}{the true positive rate for several thresshold values}
\item{FPR}{the false positive rate for several thresshold values}
\item{OOB}{the Out Of Bag error for each thresshold value}
}
\references{
Mbogning, C., Perdry, H., Broet, P.: A Bagged partially linear tree-based regression procedure for prediction and variable selection (submitted 2014)

}
\author{
Cyprien Mbogning
}
\note{
The plot of the ROC curve is straighforward using the \code{TPR} and \code{FPR} obtained with the function \code{bag.aucoob}
}

\examples{
##
}

\keyword{ documentation }