\name{VIMPBAG}
\alias{VIMPBAG}
\title{
score of importance for variables
}
\description{
Several variable importance scores are computed: the deviance importance score (DIS), the permutation importance score (PIS), the depth deviance importance score (DDIS), the minimal depth importance score (MinDepth) and the occurence score (OCCUR).
}
\usage{
VIMPBAG(BAGGRES, data, Y.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BAGGRES}{ The output of the bagging procedure (\code{bagging.pltr})
}
  \item{data}{The learning dataframe used within the bagging procedure
}
\item{Y.name}{The name of the binary dependant variable used in the bagging procedure
}
}
\details{
 several choices for variable selection using the bagging procedure are proposed. A discussion about the scores of importance PIS, DIS, and DDIS is available in \code{Mbogning et al. 2014}
}
\value{ A list with 9 elements
\item{PIS}{A list of length the length of the thresshold value used in the bagging procedure, containing the permutation importance score displayed in decreasing order for each thresshold value}
\item{StdPIS}{The standard error of the PIS}
\item{OCCUR}{The occurence number for each variable in the bagging sequence displayed in decreasing order}
\item{DIS}{The deviance importance score displayed in decreasing order}
\item{DDIS}{The depth deviance importance score displayed in decreasing order}
\item{MinDepth}{The minimal depth score for each variable, displayed in increasing order }
\item{dimtrees}{A vector containing the dimensions of trees within the baging sequence}
\item{EOOB}{A vector containing the OOB error of the bagging procedure for each thresshold value}
\item{Bagfinal}{The number of Bagging iterations used}
}
\references{
Mbogning, C., Perdry, H., Broet, P.: A Bagged partially linear tree-based regression procedure for prediction and variable selection (submitted 2014)
}
\author{
Cyprien Mbogning
}

\seealso{
\code{\link{bagging.pltr}}
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

## Several importance scores for variables, using the bagging procedure

 Var_Imp_BAG.burn <- VIMPBAG(Bag.burn, burn, Y.name)

## Importance score using the permutaion method for each thresshold value

 Var_Imp_BAG.burn$PIS
 
## Importance score using the deviance criterion

 Var_Imp_BAG.burn$DIS
 }

}

\keyword{ documentation }
\keyword{ Machine Learning}
\keyword{variable selection}
