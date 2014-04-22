\name{predict_bagg.pltr}
\alias{predict_bagg.pltr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
prediction on new features 
}
\description{
Predict new features using a set of bagging pltr models
}
\usage{
predict_bagg.pltr(bag_pltr, Y.name, newdata, type = "response",
                  thresshold = 0.5)
}
\arguments{
  \item{bag_pltr}{
a list containing the set of bagging pltr model obtained with the function \code{\link{bagging.pltr}}
}
  \item{Y.name}{
the name  of the binary dependent variable
}
\item{newdata}{
a data frame in which to look for variables with which to predict.
}
\item{type}{
the type of prediction required. \code{type = "response"} is the default; It gives the predicted probabilities. At this stage of the package, only this type is take into account. Other types like \code{"link"} and \code{"terms"} aren't supported yet.
}
\item{thresshold}{
the cutoff value for binary prediction
}
}

\value{
A list with three elements
\item{PRED_IND }{
A vector with the final prediction of each individual of the testing data by the bagging procedure (the modal prediction).
}
\item{PRED_IND}{
a matrix with the prediction of the testing data individuals with each bagging PLTR model (column by column)
}
\item{PRED_ERROR}{the estimated error of the Bagging procedure on the test sample.
}
}

\author{
Cyprien Mbogning
}

\seealso{
\code{\link{bagging.pltr}}, \code{\link[stats]{predict.glm}}
}
\examples{
##
}

\keyword{documentation}
