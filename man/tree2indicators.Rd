\name{tree2indicators}
\alias{tree2indicators}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
From a tree to indicators
}
\description{
Coerces a given tree structure to binary covariates.
}
\usage{
tree2indicators(fit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
a tree structure inheriting to the rpart method
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a list of indicators
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
#args.rpart <- list(minbucket = 40, xval = 10, cp = 0)
#family <- "binomial"
#Y.name <- "Y"
#X.names <- "G1"
#G.names <- paste("G", 2:15, sep="")

## build a maximal tree

#fit_pltr <- pltr.glm(data_pltr, Y.name, X.names, G.names, args.rpart = args.rpart,
#                     family = family,iterMax = 15, iterMin = 8)

##Compute a list of indicator from the leaves of the tree fitted tree

#tree2indicators(fit_pltr$tree)
}

\keyword{ documentation}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line