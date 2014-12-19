\name{bagging.pltr}
\alias{bagging.pltr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bagging pltr models
}
\description{
bagging procedure to agregate several PLTR models for accurate prediction and variable selection
}
\usage{
bagging.pltr(xdata, Y.name, X.names, G.names, family = "binomial", 
args.rpart,epsi = 0.001, iterMax = 5, iterMin = 3, LB = FALSE, 
args.parallel = list(numWorkers = 1),
Bag = 20, Pred_Data = data.frame(), verbose = TRUE, doprune = FALSE
, thresshold = seq(0, 1, by = 0.1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdata}{
the learning data frame 
}
  \item{Y.name}{
the name  of the binary dependent variable
}
  \item{X.names}{
the names of independent variables to consider in the linear part of the glm and as offset in the tree part
}
  \item{G.names}{
the names of independent variables to consider in the tree part of the hybrid glm.
}
\item{family}{
the glm family considered depending on the type of the dependent variable (only the binomial family works in this function for the moment) .
}
  
 \item{args.rpart}{
a list of options that control details of the rpart algorithm. \code{minbucket}: the minimum number of observations in any terminal <leaf> node; \code{cp}: complexity parameter (Any split that does not decrease the overall lack of fit by a factor of cp is not attempted); \code{maxdepth}:  the maximum depth of any node of the final tree, with the root node counted as depth 0. ...
See \code{\link[rpart]{rpart.control}} for further details
}
  \item{epsi}{
a treshold value  to check the convergence of the algorithm
}
  \item{iterMax}{
the maximal number of iteration to consider
}
  \item{iterMin}{
the minimum number of iteration to consider
}
  \item{LB}{
a binary indicator with values TRUE or FALSE indicating weither the loading is balanced or not in the parallel computing. It is nevertheless useless on a windows platform. See \code{\link[parallel]{mclapply}}
}
  \item{args.parallel}{
a list of two elements containing the number of workers and the type of parallelization to achieve see \code{\link[parallel]{mclapply}}.
}
 \item{Bag}{
The number of Bagging samples to consider
}
 \item{Pred_Data}{
An optional data frame to validate the bagging procedure (the test dataset)
}
 \item{verbose}{Logical; TRUE for printing progress during the computation (helpful for debugging)}
 \item{doprune}{
a binary indicator with values TRUE or FALSE indicating weither the set of trees in the bagging procedure are pruned (by a \code{BIC} procedure) or not}
\item{thresshold}{a vector of numerical values between 0 and 1 used as thresshold values for the computation of the OOB error rate}
}
\details{ For the Bagging procedure, it is mendatory to set \code{maxcompete = 0} and \code{maxsurrogate = 0} within the rpart arguments. This will ensured the correct calculation of the  importance of variables.
}
\value{
A list with eleven elements
\item{IND_OOB }{A list of length \code{Bag} containing the Out Of Bag (OOB) individuals for each PLTR model.}
\item{EOOB }{The vector of OOB errors of the bagging procedure for each thresshold value. }
\item{OOB_ERRORS_PBP }{A matrix with \code{Bag} columns and \code{thresshold} rows containing  OOB error of each PLTR model in the bagging sequence for each thresshold value.}
\item{OOB_ERROR_PBP }{A vector containing the mean of \code{OOB_ERRORS_PBP} for each thresshold value.}
\item{Tree_BAG }{A list of length \code{Bag} containing the bagging trees}
\item{Glm_BAG }{A list of length \code{Bag} containing the bagging pltr model; could be helpfull for prediction on new features.}
\item{LOST}{The 0, 1  lost matrix for OOB observations at each thresshold value}
\item{TEST }{A value of \code{NULL} if \code{Pred_Data} is not available. A list of three elements otherwise: \code{PRED_ERROR}: the estimated error of the Bagging procedure on the test sample for each thresshold value; \code{PRED_IND}: A list of length the length of the thresshold vector, each element containing a matrix with the prediction of the testing data individuals using each  PLTR model of the bagging sequence (column by column); \code{FINAL_PRED_IND}: A list  containing the final prediction of each individual of the testing data by the bagging procedure (the modal prediction) for each thresshold value.}
\item{Var_IMP}{A numeric vector containing the relative variable importance of the bagging procedure}
\item{Timediff}{The execution time of the bagging  procedure}
\item{CUT}{The thresshold value used inside the bagging procedure}
}
\references{
Mbogning, C., Perdry, H., Broet, P.: A Bagged partially linear tree-based regression procedure for prediction and variable selection (submitted 2014)

 Leo Breiman:  Bagging Predictors. Machine Learning, 24, 123-140 (1996)
}
\author{
Cyprien Mbogning
}

\seealso{
\code{\link{predict_bagg.pltr}}
}
\examples{
\dontrun{
##load the data set

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
}
}

\keyword{ documentation }
\keyword{ Machine Learning }
