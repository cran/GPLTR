\name{best.tree.CV}
\alias{best.tree.CV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prunning the Maximal tree
}
\description{
this function  is set to prune back the maximal tree by using a K-fold \code{cross-validation} procedure.
}
\usage{
best.tree.CV(xtree, xdata, Y.name, X.names, G.names, family = "binomial", 
args.rpart = list(cp = 0, minbucket = 20, maxdepth = 10), epsi = 0.001, 
iterMax = 15, iterMin = 8, ncv = 10, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtree}{
a tree to prune
}
  \item{xdata}{
the dataset used to build the tree
}
  \item{Y.name}{
the name  of the dependent variable 
}
  \item{X.names}{
the names of independent variables to consider in the linear part of the \code{glm}
}
  \item{G.names}{
the names of independent variables to consider in the tree part of the hybrid \code{glm}.
}
  \item{family}{
the \code{glm} family considered depending on the type of the dependent variable.
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
  \item{ncv}{
The number of folds to consider for the \code{cross-validation}
}
  \item{verbose}{Logical; TRUE for printing progress during the computation (helpful for debugging)}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a list of five elements:
 \item{best_index }{The size of the selected tree by the cross-validation procedure}
 \item{tree }{The selected tree by \code{CV}}
 \item{fit_glm}{The fitted gpltr models selected with  \code{CV} }
 \item{CV_ERRORS}{A list of two elements containing the cross-validation error of the selected tree by the \code{CV} procedure and a
       vector of cross-validation errors of all the competing models}
 \item{Timediff}{The execution time of the \code{CV} procedure}
}
\references{
Mbogning, C., Perdry, H., Toussile, W., Broet, P.: A novel tree-based procedure for deciphering the genomic spectrum of clinical disease entities. Journal of Clinical Bioinformatics 4:6, (2014)
}
\author{
Cyprien Mbogning
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{best.tree.BIC.AIC}}, \code{\link{pltr.glm}}
}
\examples{
##load the data set
#data(data_pltr)
#args.rpart <- list(minbucket = 40, maxdepth = 10, cp = 0)
#family <- "binomial"
#Y.name <- "Y"
#X.names <- "G1"
#G.names <- paste("G", 2:15, sep="")

# build a maximal tree

#fit_pltr <- pltr.glm(data_pltr, Y.name, X.names, G.names, args.rpart = args.rpart, 
#                     family = family,iterMax =15, iterMin = 8)
                     
##prunned back the maximal tree by a cross-validation procedure

#tree_selected <- best.tree.CV(fit_pltr$tree, data_pltr, Y.name, X.names, G.names, 
#     family = family, args.rpart = args.rpart, epsi = 0.001, iterMax = 15, 
#     iterMin = 8, ncv = 10)
     
#plot(tree_selected$tree, main = 'CV TREE')
#text(tree_selected$tree, minlength = 0L, xpd = TRUE)
}

\keyword{ documentation }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
