\name{GPLTR-package}
\alias{GPLTR-package}
\alias{GPLTR}
\docType{package}
\title{
Fit a generalized partially linear tree-based regression model
}
\description{
  Combining a generalized linear model with an additional tree part on the 
  same scale. A four-step procedure is proposed to fit the model and test the
  joint effect of the selected tree part while adjusting on confounding
  factors. 
  We also proposed an ensemble procedure based on the bagging to improve  
  prediction accuracy and computed several scores of importance for variable 
  selection. See 'Cyprien Mbogning et al.'(2014)<doi:10.1186/2043-9113-4-6>,
  'Cyprien Mbogning et al.'(2015)<doi:10.1159/000380850> 
  for an overview of all the methods implemented in this package. 
}
\details{
\tabular{ll}{
Package: \tab GPLTR\cr
Type: \tab Package\cr
Version: \tab 1.4\cr
Date: \tab 2023-08-26\cr
License: \tab GPL(>=2.0)\cr
}

}
\author{
Cyprien Mbogning  and Wilson Toussile

Maintainer: Cyprien Mbogning <cyprien.mbogning@gmail.com>
}
\references{
Mbogning, C., Perdry, H., Broet, P.: A Bagged partially linear tree-based regression procedure for prediction and variable selection. Human Heredity, 79(3-4):1 82-93 (2015)

Mbogning, C., Perdry, H., Toussile, W., Broet, P.: A novel tree-based procedure for deciphering the genomic spectrum of clinical disease entities. Journal of Clinical Bioinformatics 4:6, (2014) 

Terry M. Therneau, Elizabeth J. Atkinson (2013) An Introduction to Recursive Partitioning Using the \code{RPART} Routines. Mayo Foundation.

Chen, J., Yu, K., Hsing, A., Therneau, T.M.: A partially linear tree-based regression model for assessing complex joint gene-gene and gene-environment effects. Genetic Epidemiology
 31, 238-251 (2007)
}
\keyword{ package }

\examples{

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Example on a public dataset: the burn data 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## The burn data are also displayed in the KMsurv package
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 \dontrun{
 data(burn)

## Build the rpart tree with all the variables

 rpart.burn <-  rpart(D2 ~ Z1  + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 
                          + Z10  + Z11, data = burn, method = "class")
              
 plot(rpart.burn, main = 'rpart tree')
 text(rpart.burn, xpd = TRUE, cex = .6, use.n = TRUE)

## fit the PLTR model after adjusting on gender (Z2) using the proposed method

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

## Summary of the selected tree by a BIC criterion

summary(pltr.burn_prun$tree$BIC)

## Summary of the final selected pltr model

summary(pltr.burn_prun$fit_glm$BIC)

## fit the PLTR model after adjusting on gender (Z2) using the parametric 
## bootstrap method

##  set numWorkers = 1 on a windows plateform

 args.parallel = list(numWorkers = 10)
 
best_bootstrap <- best.tree.bootstrap(pltr.burn$tree, burn, Y.name, X.names,
           G.names, B = 2000, BB = 2000, args.rpart = args.rpart, epsi = 0.008,
           iterMax = 6, iterMin = 5, family = family, LEVEL = 0.05, LB = FALSE, 
              args.parallel = args.parallel, verbose = FALSE)
              
 plot(best_bootstrap$selected_model$tree, main = 'original method')
 text(best_bootstrap$selected_model$tree, xpd = TRUE)

## Bagging a set of basic unprunned pltr predictors
# ?bagging.pltr

Bag.burn <-  bagging.pltr(burn, Y.name, X.names, G.names, family, 
              args.rpart,epsi = 0.01, iterMax = 4, iterMin = 3, 
              Bag = 10, verbose = FALSE, doprune = FALSE)

## The thresshold values used

Bag.burn$CUT

## The set of PLTR models in the bagging procedure

PLTR_BAG.burn <- Bag.burn$Glm_BAG

## The set of trees in the bagging procedure

TREE_BAG.burn <- Bag.burn$Tree_BAG

## Use the bagging procedure to predict new features
# ?predict_bagg.pltr

Pred_Bag.burn <- predict_bagg.pltr(Bag.burn, Y.name, newdata = burn, 
                 type = "response", thresshold = seq(0, 1, by = 0.1))

## The confusion matrix for each thresshold value using the majority vote

Pred_Bag.burn$CONF1

## The prediction error for each thresshold value

 Pred_Bag.burn$PRED_ERROR1
 
## Compute the variable importances using the bagging procedure
 
 Var_Imp_BAG.burn <- VIMPBAG(Bag.burn, burn, Y.name)

## Importance score using the permutaion method for each thresshold value

Var_Imp_BAG.burn$PIS

## Shadow plot of three proposed scores 

par(mfrow=c(1,3))
barplot(Var_Imp_BAG.burn$PIS$CUT5, main = 'PIS', horiz = TRUE, las = 1,
        cex.names = .8, col = 'lightblue')
barplot(Var_Imp_BAG.burn$DIS, main = 'DIS', horiz = TRUE, las = 1,
        cex.names = .8, col = 'grey') 
barplot(Var_Imp_BAG.burn$DDIS, main = 'DDIS', horiz = TRUE, las = 1,
        cex.names = .8, col = 'purple')
}
}
