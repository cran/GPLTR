\name{tree2glm}
\alias{tree2glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tree to GLM
}
\description{
fit the PLTR model for a given tree. The tree is tranform in indicators covariates.
}
\usage{
tree2glm(xtree, xdata, Y.name, X.names, G.names, family = "gaussian")
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
  \item{G.names}{
the names of independent variables  considered in the tree part.
}
  \item{family}{
the glm family considered depending on the type of the dependent variable.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
the fitted model (fit)
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
#args.rpart <- list(minbucket = 40, cp = 0)
#family <- "binomial"
#Y.name <- "Y"
#X.names <- "G1"
#G.names <- paste("G", 2:15, sep="")

## build a maximal tree

#fit_pltr <- pltr.glm(data_pltr, Y.name, X.names, G.names, args.rpart = args.rpart,
#                     family = family,iterMax = 15, iterMin = 8)

#fit_glm <- tree2glm(fit_pltr$tree, data_pltr, Y.name, X.names, G.names,
#                    family = family)
                    
#summary(fit_glm)
      
}

\keyword{ documentation }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
