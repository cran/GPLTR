\name{tree2indicators}
\alias{tree2indicators}
\title{
From a tree to indicators (or dummy variables)
}
\description{
Coerces a given tree structure to binary covariates.
}
\usage{
tree2indicators(fit)
}
\arguments{
  \item{fit}{
a tree structure inheriting to the rpart method
}
}

\value{
a list of indicators
}

\author{
Cyprien Mbogning and Wilson Toussile
}

\examples{
\dontrun{
## load the data set

data(data_pltr)

## set the parameters

args.rpart <- list(minbucket = 40, xval = 10, cp = 0)
family <- "binomial"
Y.name <- "Y"
X.names <- "G1"
G.names <- paste("G", 2:15, sep="")

## build a maximal tree

fit_pltr <- pltr.glm(data_pltr, Y.name, X.names, G.names, args.rpart = args.rpart,
                     family = family,iterMax = 5, iterMin = 3)

## Compute a list of indicator from the leaves of the tree fitted tree

tree2indicators(fit_pltr$tree)
}
}

\keyword{ documentation}
