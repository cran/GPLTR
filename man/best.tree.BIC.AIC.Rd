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
                  family = "binomial", verbose = TRUE)
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
the names of independent confounding variables to consider in the linear part of the \code{glm}
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
Cyprien Mbogning and Wilson Toussile
}


\seealso{
 \code{\link{best.tree.CV}}, \code{\link{pltr.glm}}
}
\examples{
data(burn)

args.rpart <- list(minbucket = 10, maxdepth = 4, cp = 0, maxcompete = 0, 
                    maxsurrogate = 0)
 family <- "binomial"
 X.names = "Z2"
 Y.name = "D2"
 G.names = c('Z1','Z3','Z4','Z5','Z6','Z7','Z8','Z9','Z10','Z11')
 
pltr.burn <- pltr.glm(burn, Y.name, X.names, G.names, args.rpart = args.rpart,
                   family = family, iterMax = 4, iterMin = 3, verbose = FALSE)

## Prunned back the maximal tree using either the BIC or the AIC criterion

pltr.burn_prun <- best.tree.BIC.AIC(xtree = pltr.burn$tree, burn, Y.name, 
                                    X.names, family = family)

## plot the BIC selected tree

plot(pltr.burn_prun$tree$BIC, main = 'BIC selected tree')
text(pltr.burn_prun$tree$BIC, xpd = TRUE, cex = .6, col = 'blue')

\dontrun{
##load the data set

data(data_pltr)

## Set the parameters

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
                            
plot(tree_select$tree$BIC, main = 'BIC TREE')
text(tree_select$tree$BIC, minlength = 0L, xpd = TRUE, cex = .6)

}
}

\keyword{ documentation }
\keyword{ tree }
