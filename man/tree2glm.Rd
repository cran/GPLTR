\name{tree2glm}
\alias{tree2glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tree to GLM
}
\description{
fit the PLTR model for a given tree. The tree is coerced into dummy covariates.
}
\usage{
tree2glm(xtree, xdata, Y.name, X.names, family = "binomial")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtree}{
a tree inherits from the rpart method
}
  \item{xdata}{
a data frame containing the variables in the model
}
  \item{Y.name}{
the name  of the dependent variable 
}
  \item{X.names}{
the names of independent variables to consider in the linear part of the glm
}

  \item{family}{
the glm family considered depending on the type of the dependent variable.
}
}

\value{
the pltr fitted model (fit)
}


\author{
Cyprien Mbogning and Wilson Toussile
}

\examples{
\dontrun{
##load the data set

data(data_pltr)

## set the parameters

args.rpart <- list(minbucket = 40, cp = 0)
family <- "binomial"
Y.name <- "Y"
X.names <- "G1"
G.names <- paste("G", 2:15, sep="")

## build a maximal tree

fit_pltr <- pltr.glm(data_pltr, Y.name, X.names, G.names, args.rpart = args.rpart,
                     family = family,iterMax = 5, iterMin = 3)
                     
## Coerce a tree into a glm model using the confonding factor                     

fit_glm <- tree2glm(fit_pltr$tree, data_pltr, Y.name, X.names,
                    family = family)
                    
summary(fit_glm)
}      
}

\keyword{ documentation }
