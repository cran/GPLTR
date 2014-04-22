\name{bagging.pltr}
\alias{bagging.pltr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bagging pltr models
}
\description{
bagging procedure to agregate several PLTR models for accurate prediction 
}
\usage{
bagging.pltr(xdata, Y.name, X.names, G.names, family = "binomial", 
args.rpart,epsi = 0.001, iterMax = 15, iterMin = 8, LB = FALSE, 
args.parallel = list(numWorkers = 10, type = "PSOCK"),
Bag = 20, Pred_Data = data.frame(), verbose = TRUE)
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
a binary indicator with values TRUE or FALSE indicating weither the loading is balanced or not in the parallel computing. It is nevertheless useless on a windows platform. See \code{\link[parallel]{parallel}}
}
  \item{args.parallel}{
a list of two elements containing the number of workers and the type of parallelization to achieve see \code{\link[parallel]{parallel}}.
}
 \item{Bag}{
The number of Bagging samples to consider
}
 \item{Pred_Data}{
An optional data frame to validate the bagging procedure (the test dataset)
}
 \item{verbose}{Logical; TRUE for printing progress during the computation (helpful for debugging)}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with six elements
\item{IND_OOB }{A list of length \code{Bag} containing the Out Of Bag (OOB) individual for each PLTR model}
\item{OOB_ERRORS }{A vector of length \code{Bag} containing  OOB error of each PLTR model }
\item{OOB_ERROR }{The OOB error of the Bagging procedure}
\item{Tree_BAG }{A list of length \code{Bag} containing the bagging trees}
\item{Glm_BAG }{A list of length \code{Bag} containing the bagging pltr model; could be helpfull for prediction of new features.}
\item{TEST }{A value of "000" if \code{Pred_Data} is not available. A list of three elements otherwise: \code{PRED_ERROR}: the estimated error of the Bagging procedure on the test sample; \code{PRED_IND}: a matrix with the prediction of the testing data individuals with each bagging PLTR model (column by column); \code{FINAL_PRED_IND}: A vector with the final prediction of each individual of the testing data by the bagging procedure (the modal prediction).}
\item{Timediff}{The execution time of the bagging  procedure}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Cyprien Mbogning
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{

%}
\examples{
##load the data set
#data(data_pltr)
#args.rpart <- list(minbucket = 40, maxdepth = 10, cp = 0)
#family <- "binomial"
#Y.name <- "Y"
#X.names <- "G1"
#G.names <- paste("G", 2:15, sep="")

                     
## bagging a set of PLTR predictors
#args.parallel = list(numWorkers = 1, type = "PSOCK")

#bagging_pred <- bagging.pltr(data_pltr, Y.name, X.names, G.names, family, 
#             args.rpart,epsi = 0.001, iterMax = 15, iterMin = 8, LB = FALSE, 
#             args.parallel = args.parallel, Bag = 20, Pred_Data = data.frame())
}

\keyword{ documentation }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
