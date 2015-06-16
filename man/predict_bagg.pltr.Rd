\name{predict_bagg.pltr}
\alias{predict_bagg.pltr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
prediction on new features 
}
\description{
Prediction on  new features using a set of bagging pltr models
}
\usage{
predict_bagg.pltr(bag_pltr, Y.name, newdata, type = "response",
                  thresshold = seq(0, 1, by = 0.1))
}
\arguments{
  \item{bag_pltr}{
the bagging result obtained with the function \code{\link{bagging.pltr}}
}
  \item{Y.name}{
the name  of the binary dependent variable
}
\item{newdata}{
a data frame in which to look for predictors and the dependant variable.
}
\item{type}{
the type of prediction required. \code{type = "response"} is the default; It gives the predicted probabilities. At this stage of the package, only this type is take into account. Other types such as \code{"link"} and \code{"terms"} are useless.
}
\item{thresshold}{
a vector of cutoff values for binary prediction. Could be helpfull for computing the AUC on the test sample.
}
}

\value{
A list with 8 elements
\item{FINAL_PRED_IND1 }{
A list of size the length of the thresshold vector, containing the final prediction of each individual of the testing data by the bagging procedure using the majority rule (the modal prediction).
}
\item{FINAL_PRED_IND2 }{
A list of size the length of the thresshold vector, containing the final prediction of each individual of the testing data by the bagging procedure using the mean estimated probability.
}
\item{PRED_ERROR1}{A vector of  estimated errors of the Bagging procedure on the test sample for each thresshold value using \code{FINAL_PRED_IND1}.
}
\item{PRED_ERROR2}{A vector of estimated errors of the Bagging procedure on the test sample for each thresshold value using \code{FINAL_PRED_IND2}.
}
\item{CONF1}{A list of confusion matrix using \code{FINAL_PRED_IND1}
}
\item{CONF2}{A list of confusion matrix using \code{FINAL_PRED_IND2}
}
\item{PRED_ERRORS_PBP}{A list of size the length of the thresshold vector. Each element representing the prediction error obtained via each predictor in the bagging sequence for each thresshold value
}
\item{PRED_ERROR_PBP}{A vector containing the mean of \code{PRED_ERRORS_PBP} for each thresshold value}
}
\references{
Mbogning, C., Perdry, H., Broet, P.: A Bagged partially linear tree-based regression procedure for prediction and variable selection. Human Heredity (To appear), (2015)
}

\author{
Cyprien Mbogning
}

\seealso{
\code{\link{bagging.pltr}}, \code{\link[stats]{predict.glm}}
}
\examples{
\dontrun{
## load the data set

 data(burn)

## set the parameters 

 args.rpart <- list(minbucket = 10, maxdepth = 4, cp = 0, maxsurrogate = 0)
 family <- "binomial"
 Y.name <- "D2"
 X.names <- "Z2"
 G.names <- c('Z1','Z3','Z4','Z5','Z6','Z7','Z8','Z9','Z10','Z11')
 args.parallel = list(numWorkers = 1)
                     
## Bagging a set of basic unprunned pltr predictors

 Bag.burn <-  bagging.pltr(burn, Y.name, X.names, G.names, family, 
             args.rpart,epsi = 0.01, iterMax = 4, iterMin = 3, 
             Bag = 20, verbose = FALSE, doprune = FALSE)

## Use the bagging procedure to predict new features

# ?predict_bagg.pltr

 Pred_Bag.burn <- predict_bagg.pltr(Bag.burn, Y.name, newdata = burn, 
                type = "response", thresshold = seq(0, 1, by = 0.1))

## The confusion matrix for each thresshold value using the majority vote

Pred_Bag.burn$CONF1
}
}

\keyword{ documentation }
\keyword{ Machine Learning }
