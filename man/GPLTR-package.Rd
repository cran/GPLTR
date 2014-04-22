\name{GPLTR-package}
\alias{GPLTR-package}
\alias{GPLTR}
\docType{package}
\title{
Fit a generalized partially linear tree-based regression model
}
\description{
Package for generalize partially linear tree-based regression model, combining a
generalized linear model with an additional tree part on the same scale.
}
\details{
\tabular{ll}{
Package: \tab GPLTR\cr
Type: \tab Package\cr
Version: \tab 0.85\cr
Date: \tab 2014-04-22\cr
License: \tab GPL(>=2.0)\cr
}

}
\author{
Cyprien Mbogning  and Wilson Toussile

Maintainer: Cyprien Mbogning <cyprien.mbogning@gmail.com>
}
\references{
Mbogning, C., Perdry, H., Toussile, W., Broet, P.: A novel tree-based procedure for deciphering the genomic spectrum of clinical disease entities. Journal of Clinical Bioinformatics 4:6, (2014) 

Terry M. Therneau, Elizabeth J. Atkinson (2013) An Introduction to Recursive Partitioning Using the \code{RPART} Routines. Mayo Foundation.

Chen, J., Yu, K., Hsing, A., Therneau, T.M.: A partially linear tree-based regression model for assessing complex joint gene-gene and gene-environment effects. Genetic Epidemiology
 31, 238-251 (2007)
}
\keyword{ package }
%\seealso{

%}
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
#                     family = family,iterMax = 4, iterMin = 3)

#plot(fit_pltr$tree, main = 'MAXIMAL TREE')
#text(fit_pltr$tree, minlength = 0L, xpd = TRUE)

## prunned back the maximal tree by BIC or AIC criterion

#tree_select <- best.tree.BIC.AIC(xtree = fit_pltr$tree,data_pltr,Y.name, 
#                            X.names, G.names, family = family)
                            
#plot(tree_select$tree$BIC, main = 'BIC TREE')
#text(tree_select$tree$BIC, minlength = 0L,xpd = TRUE)

## prunned back the maximal tree by a cross-validation procedure

#tree_selected <- best.tree.CV(fit_pltr$tree, data_pltr, Y.name, X.names, G.names, 
# family = family, args.rpart = args.rpart, epsi = 0.001, iterMax = 15, 
#     iterMin = 8, ncv = 10)
     
#plot(tree_selected$tree, main = 'CV TREE')
#text(tree_selected$tree, minlength = 0L, xpd = TRUE)

## Compute the p-value of the selected tree by BIC

#args.parallel = list(numWorkers = 1, type = "PSOCK")
#index = tree_select$best_index[[1]]
#p_value <- p.val.tree(xtree = fit_pltr$tree, data_pltr, Y.name, X.names, G.names,
#            B = 10, args.rpart = args.rpart, epsi = 1e-3, 
#            iterMax = 15, iterMin = 8, family = family, LB = FALSE, 
#            args.parallel = args.parallel, index = index)

## select an test the selected tree by a pametric bootstrap  procedure

#best_bootstrap <- best.tree.bootstrap(fit_pltr$tree, data_pltr, Y.name, X.names,
#   G.names, B = 10, BB = 10, args.rpart = args.rpart, epsi = 0.001, 
#   iterMax = 15, iterMin = 8, family = family, LEVEL = 0.05,LB = FALSE, 
#                  args.parallel = args.parallel)
                  

## bagging a set of PLTR predictors

#bagging_pred <- bagging.pltr(data_pltr, Y.name, X.names, G.names, family, 
#   args.rpart,epsi = 0.001, iterMax = 15, iterMin = 8, LB = FALSE, 
#  args.parallel = args.parallel, Bag = 20, Pred_Data = data.frame())

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Example on a public dataset: the burn data 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## The burn data are also displayed in the KMsurv package
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 data(burn)

## Build the rpart tree with all the variables

 cfit = rpart(D2 ~ Z1  + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 + Z10 + Z11, 
              data = burn, method = "class")
              
 plot(cfit, main = 'rpart tree')
 text(cfit, xpd = TRUE)

## fit the PLTR model after adjusting on gender (Z2) using the proposed method

 args.rpart <- list(minbucket = 10, maxdepth = 4, cp = 0)
 family <- "binomial"
 X.names = "Z2"
 Y.name = "D2"
 G.names = c('Z1','Z3','Z4','Z5','Z6','Z7','Z8','Z9','Z10','Z11')
fit_pltr <- pltr.glm(burn, Y.name, X.names, G.names, args.rpart = args.rpart, 
           family = family,iterMax = 4, iterMin = 3, verbose = FALSE)

tree_select <- best.tree.BIC.AIC(xtree = fit_pltr$tree, burn ,Y.name, 
                             X.names, G.names, family = family)

plot(tree_select$tree$BIC, main = 'new PLTR tree')
text(tree_select$tree$BIC, xpd = TRUE)
summary(tree_select$fit_glm$BIC)

## fit the PLTR model after adjusting on gender (Z2) using the original method

## uncomment the following code and set numWorkers = 1 on a windows plateform

# args.parallel = list(numWorkers = 10, type = "PSOCK")
#best_bootstrap <- best.tree.bootstrap(fit_pltr$tree, burn, Y.name, X.names,
#           G.names, B = 2000, BB = 2000, args.rpart = args.rpart, epsi = 0.008,
#              iterMax = 6,iterMin = 5, family = family, LEVEL = 0.05,LB = FALSE, 
#              args.parallel = args.parallel, verbose = FALSE)
# plot(best_bootstrap$selected_model$tree, main = 'original method')
# text(best_bootstrap$selected_model$tree, xpd = TRUE)


}
