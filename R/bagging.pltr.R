bagging.pltr <- function(xdata, Y.name, X.names, G.names, family = "binomial", args.rpart,epsi = 0.001, iterMax = 15, iterMin = 8, LB = FALSE, args.parallel = list(numWorkers = 10, type = "PSOCK"), Bag = 20, Pred_Data = data.frame(), verbose = TRUE, doprune = TRUE)
{
  time1 <- Sys.time()
  n = nrow(xdata)
  Ind_data <- 1:n
  IND_SAMP <- lapply(1:Bag, function(u) sample.int(n, size = n, replace = TRUE))
  IND_OOB  <- lapply(IND_SAMP, function(v) Ind_data[!(Ind_data %in% v)])
  
  wrapper <- function(xdata_bag)
  {
    pltr_lm_b = pltr.glm(xdata_bag, Y.name = Y.name, X.names = X.names, G.names = G.names, family = family, args.rpart = args.rpart, epsi = epsi, iterMax = iterMax, iterMin = iterMin, verbose = verbose)
    
    return(pltr_lm_b$tree)
  }
  numWorkers = args.parallel$numWorkers
  cat("\n ncores = ", numWorkers, " for bagging trees !\n")
  List_xdatas <- lapply(IND_SAMP, function(w) return(xdata[w,]))
  
  MaxTreeList <- mclapply(List_xdatas, wrapper, mc.cores = getOption("mc.cores", numWorkers), mc.preschedule = LB, mc.silent = TRUE)
  
  List_xTrees_xDatas <-  lapply(1:Bag, function(j)
  {
    return(list(MaxTreeList[[j]], List_xdatas[[j]]))
  })
  if(doprune ){
  cat("\n ncores = ", numWorkers, " for bagging selected trees !\n")
  wrapper2 <- function(list_xtree_xdata)
  {
    resultBICAIC = best.tree.BIC.AIC(xtree = list_xtree_xdata[[1]], xdata = list_xtree_xdata[[2]], Y.name, X.names, G.names, family, verbose = verbose)
    return(list(resultBICAIC$tree$BIC, resultBICAIC$fit_glm$BIC))
  }
  tree_model_BIC <- mclapply(List_xTrees_xDatas, wrapper2, mc.cores = getOption("mc.cores", numWorkers), mc.preschedule = LB, mc.silent = TRUE)
  LIST_tree_BIC_Bag <- lapply(tree_model_BIC,function(mod) return(mod[[1]]))
  LIST_glm_Bag <- lapply(tree_model_BIC,function(modd) return(modd[[2]]))
  ImpVarlist = lapply(LIST_tree_BIC_Bag, function(ww){
    ww$variable.importance
  })
  Impvarmat <- unlist(ImpVarlist)
  TREES <- LIST_tree_BIC_Bag
  }
  else{
    LIST_glm_Bag <- lapply( List_xTrees_xDatas, function(u){
      tree2glm(xtree = u[[1]], xdata = u[[2]], Y.name, X.names, G.names, family)
    })
    ImpVarlist = lapply(MaxTreeList, function(ww){
      ww$variable.importance
    })
    Impvarmat <- unlist(ImpVarlist)
    TREES <- MaxTreeList
  }
  ImpVar <- tapply(Impvarmat, names(Impvarmat), sum)
  ImpVar <- ImpVar/sum(ImpVar)*100
  ImpVar <- sort(ImpVar, decreasing = TRUE)
  ImpVar <- ImpVar[ImpVar > 1]
  ImpVar <- round(ImpVar/sum(ImpVar)*100, 2)
  
  List_xdatas_OOB <- lapply(IND_OOB, function(w) return(xdata[w,]))
  List_xdatas_Glm_OBB <- lapply(1:Bag, function(j)
  {
    return(list(List_xdatas_OOB[[j]], LIST_glm_Bag[[j]]))
  })
  predict_glm_OOB_PBP <- lapply(List_xdatas_Glm_OBB,function(uw)
  {
   pred = predict.glm(uw[[2]], newdata = uw[[1]], type = "response")
   return(as.numeric(pred >= 0.5))
  })
  OOB_ERRORS_PBP <- sapply(1:Bag,function(uuu)
  { 
   return(mean(List_xdatas_OOB[[uuu]][Y.name] != predict_glm_OOB_PBP[[uuu]]))
  })
  OOB_ERROR_PBP <- mean(OOB_ERRORS_PBP)
  
  predict_glm_OOB <- lapply(LIST_glm_Bag,function(uw)
  {
    pred = predict.glm(uw, newdata = xdata, type = "response")
    return(as.numeric(pred >= 0.5))
  })
  PRED_OOB <- matrix(unlist(predict_glm_OOB), ncol = Bag, byrow = FALSE)
  FINAL_PRED_OOB <- apply(PRED_OOB,1,function(zz){return(names(which.max(table(zz))))})
  OOB_ERROR <- mean(FINAL_PRED_OOB != xdata[Y.name])
  
  ## Compute the real OOB error.
  
  UNIQUE_IND_OOB <- sort(unique(unlist(IND_OOB)))
  EOOB <- rep(0,length(UNIQUE_IND_OOB))
  j <- 0
  for(i in UNIQUE_IND_OOB){
    j <- j+1
    poslist <- sapply(IND_OOB, function(uu) is.element(i,uu))
    IND_OOBi <- IND_OOB[poslist]
    PRED_OOBi <- predict_glm_OOB_PBP[poslist]
    posi <- sapply(IND_OOBi, function(w) which(w == i))
    vec_PREDi <- sapply(PRED_OOBi, function(ww) ww[posi])
    PREDi <- names(which.max(table(vec_PREDi)))
    EOOB[j] <- PREDi != i 
  }
  EOOB <- mean(EOOB)
 
  if(nrow(Pred_Data) != 0)
 {
   predict_glm2 <- lapply(List_xdatas_Glm_OBB, function(uw)
   {
     pred = predict.glm(uw[[2]], newdata = Pred_Data, type = "response")
     return(as.numeric(pred>0.5))
   })
   
   PRED_IND <- matrix(unlist(predict_glm2), ncol = Bag, byrow = F)
   FINAL_PRED_IND <- apply(PRED_IND,1,function(zz){return(names(which.max(table(zz))))})
   PRED_ERROR <- mean(FINAL_PRED_IND != Pred_Data[Y.name] )
   
 }
  time2 <- Sys.time()
  Timediff <- difftime(time2, time1)
 return(list(IND_OOB = IND_OOB, EOOB = EOOB, OOB_ERROR = OOB_ERROR, OOB_ERRORS_PBP = OOB_ERRORS_PBP, OOB_ERROR_PBP = OOB_ERROR_PBP,
        Tree_BAG = TREES, Glm_BAG = LIST_glm_Bag, TEST = ifelse(length(Pred_Data) != 0,
        list(PRED_ERROR = PRED_ERROR, PRED_IND = PRED_IND, FINAL_PRED_IND = FINAL_PRED_IND),"000"), 
             Var_IMP = ImpVar, Timediff = Timediff))  
}