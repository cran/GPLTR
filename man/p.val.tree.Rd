\name{p.val.tree}
\alias{p.val.tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the p-value
}
\description{
Test weither the selected tree by either \code{BIC}, \code{AIC} or \code{CV} procedure is significantly associated to the dependent variable or not, while adjusting for a confounding effect.
}
\usage{
p.val.tree(xtree, xdata, Y.name, X.names, G.names, B = 10, args.rpart = 
list(minbucket = 40, maxdepth = 10, cp = 0), epsi = 0.001, iterMax = 5,
iterMin = 3, family = "binomial", LB = FALSE, 
args.parallel = list(numWorkers = 1), index = 4, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtree}{
the maximal tree obtained by the function pltr.glm
}
  \item{xdata}{
the data frame used to build xtree
}
  \item{Y.name}{
the name  of the dependent variable
}
  \item{X.names}{
the names of independent confounding variables to consider in the linear part of the \code{glm}
}
  \item{G.names}{
the names of independent variables to consider in the tree part of the hybrid \code{glm}.
}
  \item{B}{
the resampling size of the deviance difference
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
  \item{family}{
the glm family considered depending on the type of the dependent variable.
}
  \item{LB}{
a binary indicator with values TRUE or FALSE indicating weither the loading are balanced or not in the parallel computing
}
  \item{args.parallel}{
parameters of the parallelization. See \code{\link[parallel]{mclapply}} for more details. 
}
  \item{index}{
the size of the selected tree (by the functions \code{\link{best.tree.BIC.AIC}} or \code{\link{best.tree.CV}}) using one of the proposed criteria
}
 \item{verbose}{Logical; TRUE for printing progress during the computation (helpful for debugging)}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{ A list of three elements: 
\item{p.value}{The \code{P-value} of the selected tree}
\item{Timediff}{The execution time of the \code{test} procedure}
\item{Badj}{The number of samples used inside the  the procedure}
}
\references{
Mbogning, C., Perdry, H., Toussile, W., Broet, P.: A novel tree-based procedure for deciphering the genomic spectrum of clinical disease entities. Journal of Clinical Bioinformatics 4:6, (2014)

Fan, J., Zhang, C., Zhang, J.: Generalized likelihood ratio statistics and WILKS phenomenon. Annals of Statistics
29(1), 153-193 (2001)
}
\author{
Cyprien Mbogning
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{best.tree.bootstrap}}, \code{\link{best.tree.permute}}
}
\examples{
\dontrun{
## load the data set

data(data_pltr)

## set the parameters 

args.rpart <- list(minbucket = 40, maxdepth = 10, cp = 0)
family <- "binomial"
Y.name <- "Y"
X.names <- "G1"
G.names <- paste("G", 2:15, sep="")

## build a maximal tree

fit_pltr <- pltr.glm(data_pltr, Y.name, X.names, G.names, args.rpart = args.rpart, 
                    family = family,iterMax = 5, iterMin = 3)
                     
##prunned back the maximal tree by BIC or AIC criterion

tree_select <- best.tree.BIC.AIC(xtree = fit_pltr$tree,data_pltr,Y.name, 
                                 X.names, family = family)
                     
## Compute the p-value of the selected tree by BIC

args.parallel = list(numWorkers = 10, type = "PSOCK")
index = tree_select$best_index[[1]]
p_value <- p.val.tree(xtree = fit_pltr$tree, data_pltr, Y.name, X.names, G.names,
            B = 100, args.rpart = args.rpart, epsi = 1e-3, 
            iterMax = 5, iterMin = 3, family = family, LB = FALSE, 
            args.parallel = args.parallel, index = index)
}
}

\keyword{ documentation }
\keyword{ test }
