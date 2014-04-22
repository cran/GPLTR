### R code from vignette source 'intro.Rnw'

###################################################
### code chunk number 1: intro.Rnw:37-42
###################################################
options(continue = "  ", width = 60)
options(SweaveHooks=list(fig=function() par(mar = c(4.1, 4.1, 0.1, 1.1))))
pdf.options(pointsize = 10)
par(xpd = NA)  #stop clipping
library(GPLTR)


###################################################
### code chunk number 2: cart
###################################################
getOption("SweaveHooks")[["fig"]]()
data(burn)
cfit <- rpart(D2 ~ Z1  + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 
                  + Z10 + Z11, data = burn, method = "class")
par(mar = rep(0.1, 4))
plot(cfit, uniform = TRUE)
 text(cfit, xpd = TRUE, use.n = TRUE)


###################################################
### code chunk number 3: gpltr
###################################################
getOption("SweaveHooks")[["fig"]]()
#setting the parameters
args.rpart <- list(minbucket = 10, maxdepth = 4, cp = 0)
family <- "binomial"
X.names = "Z2"
Y.name = "D2"
G.names = c('Z1','Z3','Z4','Z5','Z6','Z7','Z8','Z9','Z10','Z11')
# Build the tree with an adjustment  on gender (Z2)
fit_pltr <- pltr.glm(burn, Y.name, X.names, G.names, args.rpart = 
                      args.rpart, family = family,iterMax =8, iterMin = 6,
                        verbose = TRUE)
tree_select <- best.tree.BIC.AIC(xtree = fit_pltr$tree,burn,Y.name, 
                        X.names, G.names, family = family, verbose = FALSE)
summary(tree_select$tree$BIC)

summary(tree_select$fit_glm$BIC)

par(mfrow = c(1,2), mar = rep(0.1, 4))
plot(fit_pltr$tree, uniform = TRUE, margin = 0.05)
plot(tree_select$tree$BIC, uniform = TRUE, margin = 0.05)
text(tree_select$tree$BIC, xpd = TRUE)


###################################################
### code chunk number 4: intro.Rnw:196-209
###################################################
tree_selected <- best.tree.CV(fit_pltr$tree, burn, Y.name, X.names, 
                  G.names, family = family, args.rpart = args.rpart,
                  epsi = 0.001, iterMax = 15, iterMin = 8, ncv = 10,
                  verbose = FALSE) 

tree_selected$CV_ERROR

Bic_size <- sum(tree_select$tree$BIC$frame$var == '<leaf>')
## Bic_size <- tree_select$best_index[[1]]

CV_ERROR_BIC <- tree_selected$CV_ERROR[[2]][Bic_size]

CV_ERROR_BIC


###################################################
### code chunk number 5: intro.Rnw:214-222
###################################################
args.parallel = list(numWorkers = 1, type = "PSOCK")
index = Bic_size
# p_value <- p.val.tree(xtree = fit_pltr$tree, data_pltr, Y.name, X.names,
#             G.names, B = 1000, args.rpart = args.rpart, epsi = 1e-3, 
#             iterMax = 15, iterMin = 8, family = family, LB = FALSE, 
#             args.parallel = args.parallel, index = index, verbose =
#             FALSE)
# p_value$P.value


