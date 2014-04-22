\name{best.tree.BIC.AIC}
\alias{best.tree.BIC.AIC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prunning the Maximal tree
}
\description{
this function  is set to prune back the maximal tree by using  the \code{BIC} or the \code{AIC} criterion.
}
\usage{
best.tree.BIC.AIC(xtree, xdata, Y.name, X.names,
   G.names, family = "binomial", verbose = TRUE)
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
 \item{verbose}{Logical; TRUE for printing progress during the computation (helpful for debugging)}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a list of four elements:
 \item{best_index }{The size of the selected trees by \code{BIC} and \code{AIC} }
 \item{tree }{The selected trees by \code{BIC} and \code{AIC} }
 \item{fit_glm}{The fitted pltr models selected with \code{BIC}, and \code{AIC} }
 \item{Timediff}{The execution time of the selection procedure}
}
\references{
Mbogning, C., Perdry, H., Toussile, W., Broet, P.: A novel tree-based procedure for deciphering the genomic spectrum of clinical disease entities. Journal of Clinical Bioinformatics 4:6, (2014)

Akaike, H.: A new look at the statistical model identification. \code{IEEE Trans. Automat. Control AC-19}, 716-723 (1974)

Schwarz, G.: Estimating the dimension of a model. \code{The Annals of Statistics} 6, 461-464 (1978)
}
\author{
Cyprien Mbogning
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{best.tree.CV}}, \code{\link{pltr.glm}}
}
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
                     
##prunned back the maximal tree by BIC or AIC criterion

#tree_select <- best.tree.BIC.AIC(xtree = fit_pltr$tree,data_pltr,Y.name, 
#                            X.names, G.names, family = family)
                            
#plot(tree_select$tree$BIC, main = 'BIC TREE')
#text(tree_select$tree$BIC, minlength = 0L, xpd = TRUE)
}

\keyword{ documentation }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
