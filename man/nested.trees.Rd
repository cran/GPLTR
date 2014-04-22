\name{nested.trees}
\alias{nested.trees}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute the nested trees
}
\description{
Compute a set of nested competing trees for the prunning phase
}
\usage{
nested.trees(xtree, xdata, Y.name, X.names, G.names, MaxTreeSize = NULL,
family = "gaussian", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtree}{
a tree inheriting to the rpart method
}
  \item{xdata}{
the dataset used to build the tree
}
  \item{Y.name}{
the name  of the dependent variable in the tree model
}
  \item{X.names}{
the names of independent variables considered as offset in the tree model
}
  \item{G.names}{
the names of independent variables used to build the tree.
}
  \item{MaxTreeSize}{
The maximal size of the competing trees
}
  \item{family}{
the glm family considered depending on the type of the dependent variable.
}
\item{verbose}{Logical; TRUE for printing progress during the computation (helpful for debugging)}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a list with 4 elements:
\item{leaves }{a list of leaves of the competing trees to consider for the optimal tree}
\item{null_deviance}{the deviance of the null model (linear part of the glm)}
\item{deviances}{a vector of deviances of the competing PLTR models}
\item{diff_deviances}{a vector of the deviance differencies between the competing PLTR models and the null model}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##load the data set

#data(data_pltr)
#args.rpart <- list(minbucket = 40, maxdepth = 10, cp = 0)
#family <- "binomial"
#Y.name <- "Y"
#X.names <- "G1"
#G.names <- paste("G", 2:15, sep="")

## build a maximal tree

#fit_pltr <- pltr.glm(data_pltr, Y.name, X.names, G.names, args.rpart = args.rpart, 
#                     family = family,iterMax =15, iterMin = 8)
                     
## compute the competing trees
#nested_trees <- nested.trees(fit_pltr$tree, data_pltr, Y.name, X.names, G.names,
#                            MaxTreeSize = 10, family = family)
}

\keyword{ documentation }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
