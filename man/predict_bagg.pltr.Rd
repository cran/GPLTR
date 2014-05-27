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
A list with 8 elements
\item{FINAL_PRED_IND1 }{
A vector with the final prediction of each individual of the testing data by the bagging procedure (the modal prediction).
}
\item{FINAL_PRED_IND2 }{
A vector with the final prediction of each individual of the testing data by the bagging procedure using the mean estimated probability.
}
\item{PRED_IND_MAT }{
A matrix containing the predicted values of the test sample (predictor by predictor in the bagging sequence.) column by column.
}
\item{PRED_ERROR1}{the estimated error of the Bagging procedure on the test sample using \code{FINAL_PRED_IND1}.
}
\item{PRED_ERROR2}{the estimated error of the Bagging procedure on the test sample using \code{FINAL_PRED_IND2}.
}
\item{PROB_MAT}{A matrix of estimated probabilities on testing sample individuals by each predictor of the bagging sequence.
}
\item{CONF1}{The confusion matrix using \code{FINAL_PRED_IND1}
}
\item{CONF2}{The confusion matrix using \code{FINAL_PRED_IND2}
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
