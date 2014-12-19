\name{best.tree.permute}
\alias{best.tree.permute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
permutation test on a pltr model
}
\description{
a unified permutation test procedure to select and test at the same time the selected tree
}
\usage{
best.tree.permute(xtree, xdata, Y.name, X.names, G.names, B = 10,
 args.rpart = list(cp = 0, minbucket = 20, maxdepth = 10), epsi = 0.001,
 iterMax = 5, iterMin = 3, family = "binomial", LEVEL = 0.05, 
 LB = FALSE, args.parallel = list(numWorkers = 1, type = "PSOCK"), verbose = TRUE)
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
the names of independent variables to consider in the linear part of the glm. For this function, only a binary variable is supported.
}
  \item{G.names}{
the names of independent variables to consider in the tree part of the hybrid glm.
}
  \item{B}{
the size of the bootstrap sample
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
the binomial family.
}
  \item{LEVEL}{
the level of the test
}
  \item{LB}{
a binary indicator with values TRUE or FALSE indicating weither the loading is balanced or not in the parallel computing. It is useless on a windows platform.
}
  \item{args.parallel}{
parameters of the parallelization. See \code{\link[parallel]{mclapply}} for more details.
}
 \item{verbose}{Logical; TRUE for printing progress during the computation (helpful for debugging)}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a list with six elements:
\item{p.val_selected }{the adjusted p-value of the selected tree}
\item{selected_model }{a list with the fit of the selected pltr model \code{fit_glm}, the selected tree \code{tree} and the p-value of the selected tree without adjusting for multiple comparisons \code{p.value}}
 \item{fit_glm }{the fitted pltr model under the null hypothesis if the test is not significant}
 \item{Timediff}{The execution time of the \code{permutation test} procedure}
 \item{comp_p_values}{The P-values of the competing trees}
\item{Badj}{The number of samples used inside the procedure}
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

\seealso{
\code{\link{p.val.tree}}, \code{\link{best.tree.bootstrap}}
}
\examples{
\dontrun{
##load the data set

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
                     
## select an test the selected tree by a permutation test procedure

args.parallel = list(numWorkers = 1, type = "PSOCK")

best_permute <- best.tree.permute(fit_pltr$tree, data_pltr, Y.name, X.names,
  G.names, B = 10, args.rpart = args.rpart, epsi = 0.001, iterMax = 5,              
             iterMin = 3, family = family, LEVEL = 0.05,LB = FALSE, 
}                                   args.parallel = args.parallel)
}

\keyword{ documentation }
\keyword{ test }
