\name{pltr.glm}
\alias{pltr.glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partially tree-based regression model function
}
\description{
The \code{pltr.glm} function is designed to fit an hybrid glm model with  an additive tree part on a glm scale.
}
\usage{
pltr.glm(data, Y.name, X.names, G.names, family = "binomial", 
    args.rpart = list(cp = 0, minbucket = 20, maxdepth = 10), 
    epsi = 0.001, iterMax = 5, iterMin = 3, verbose = TRUE)
}

\arguments{
  \item{data}{
a data frame containing the variables in the model
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
 \item{verbose}{Logical; TRUE for printing progress during the computation (helpful for debugging)}
}
\details{
 The \code{pltr.glm} function use an itterative procedure to fit the linear part of the \code{glm} and the tree part. The tree obtained at the convergence of the procedure is a maximal tree which overfits the data. It's then mandatory to prunned back this tree by using one of the proposed criteria (\code{BIC}, \code{AIC} and \code{CV}).
}
\value{A list with four elements:
 \item{fit }{the glm fitted on the confounding factors at the end of the iterative algorithm}
 \item{tree }{the maximal tree obtained at the end of the algorithm}
 \item{nber_iter}{the number of iterations used by the algorithm}
 \item{Timediff}{The execution time of the iterative procedure}
}
\references{
Mbogning, C., Perdry, H., Toussile, W., Broet, P.: A novel tree-based procedure for deciphering the genomic spectrum of clinical disease entities. Journal of Clinical Bioinformatics 4:6, (2014)

Terry M. Therneau, Elizabeth J. Atkinson (2013) An Introduction to Recursive Partitioning Using the \code{RPART} Routines. Mayo Foundation.

Chen, J., Yu, K., Hsing, A., Therneau, T.M.: A partially linear tree-based regression model for assessing complex joint gene-gene and gene-environment effects. Genetic Epidemiology
 31, 238-251 (2007)
}
\author{
Cyprien Mbogning and Wilson Toussile
}
\note{ The tree obtained at the end of these itterative procedure usually overfits the data. It's therefore mendatory to use either \code{\link{best.tree.BIC.AIC}} or \code{\link{best.tree.CV}} to prunne back the tree.
}

\seealso{
\code{\link[rpart]{rpart}}
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
                    
plot(fit_pltr$tree, main = 'MAXIMAL TREE')
text(fit_pltr$tree, minlength = 0L, xpd = TRUE, cex = .6)
}
}

\keyword{ documentation }
\keyword{ tree }
\keyword{ Machine Learning }
