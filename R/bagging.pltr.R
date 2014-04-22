bagging.pltr <- function(xdata, Y.name, X.names, G.names, family = "binomial", args.rpart,epsi = 0.001, iterMax = 15, iterMin = 8, LB = FALSE, args.parallel = list(numWorkers = 10, type = "PSOCK"), Bag = 20, Pred_Data = data.frame(), verbose = TRUE)
{
  time1 <- Sys.time()
  n = nrow(xdata)
  Ind_data <- 1:n
  IND_SAMP <- lapply(1:Bag,function(u) sample.int(n,size = n, replace = TRUE))
  IND_OOB  <- lapply(IND_SAMP,function(v) Ind_data[!(Ind_data %in% v)])
  
  wrapper <- function(xdata_bag)
  {
    pltr_lm_b = pltr.glm(xdata_bag, Y.name = Y.name, X.names = X.names, G.names = G.names, family = family, args.rpart = args.rpart, epsi = epsi, iterMax = iterMax, iterMin = iterMin, verbose = verbose)
    
    return(pltr_lm_b$tree)
  }
  numWorkers = args.parallel$numWorkers
  cat("\n ncores = ", numWorkers, " for bagging trees !\n")
  List_xdatas <- lapply(IND_SAMP, function(w) return(xdata[w,]))
  
  MaxTreeList <- mclapply(List_xdatas, wrapper, mc.cores = getOption("mc.cores", numWorkers), mc.preschedule = LB, mc.silent = TRUE)
  
  wrapper2 <- function(list_xtree_xdata)
  {
   resultBICAIC = best.tree.BIC.AIC(xtree = list_xtree_xdata[[1]], xdata = list_xtree_xdata[[2]], Y.name, X.names, G.names, family, verbose = verbose)
   return(list(resultBICAIC$tree$BIC, resultBICAIC$fit_glm$BIC))
  }
  List_xTrees_xDatas <-  lapply(1:Bag, function(j)
  {
    return(list(MaxTreeList[[j]], List_xdatas[[j]]))
  })
  cat("\n ncores = ", numWorkers, " for bagging selected trees !\n")

  tree_model_BIC <- mclapply(List_xTrees_xDatas, wrapper2, mc.cores = getOption("mc.cores", numWorkers), mc.preschedule = LB, mc.silent = TRUE)
  LIST_tree_BIC_Bag <- lapply(tree_model_BIC,function(mod) return(mod[[1]]))
  LIST_glm_Bag <- lapply(tree_model_BIC,function(modd) return(modd[[2]]))
  
  List_xdatas_OOB <- lapply(IND_OOB, function(w) return(xdata[w,]))
  List_xdatas_Glm_OBB <- lapply(1:Bag, function(j)
  {
    return(list(List_xdatas_OOB[[j]], LIST_glm_Bag[[j]]))
  })
  predict_glm <- lapply(List_xdatas_Glm_OBB,function(uw)
  {
   pred = predict.glm(uw[[2]], newdata = uw[[1]], type = "response")
   return(as.numeric(pred>0.5))
  })
  OOB_ERRORS <- sapply(1:Bag,function(uuu)
  { 
   return(mean(List_xdatas_OOB[[uuu]][Y.name] != predict_glm[[uuu]]))
  })
  OOB_ERROR <- mean(OOB_ERRORS)
 
  if(nrow(Pred_Data) != 0)
 {
   predict_glm2 <- lapply(List_xdatas_Glm_OBB, function(uw)
   {
     pred = predict.glm(uw[[2]], newdata = Pred_Data, type = "response")
     return(as.numeric(pred>0.5))
   })
   PRED_ERRORS <- sapply(1:Bag,function(vvv)
   { 
     return(mean(Pred_Data[Y.name] != predict_glm2[[vvv]]))
   })
   PRED_ERROR <- mean(PRED_ERRORS)
   PRED_IND <- matrix(unlist(predict_glm2), ncol = Bag, byrow = F)
   FINAL_PRED_IND <- apply(PRED_IND,1,function(zz){return(names(which.max(table(zz))))})
   
 }
  time2 <- Sys.time()
  Timediff <- difftime(time2, time1)
 return(list(IND_OOB = IND_OOB, OOB_ERRORS = OOB_ERRORS, OOB_ERROR = OOB_ERROR, 
        Tree_BAG = LIST_tree_BIC_Bag, Glm_BAG = LIST_glm_Bag, TEST = ifelse(length(Pred_Data) != 0,list(PRED_ERROR = PRED_ERROR, PRED_IND = PRED_IND, FINAL_PRED_IND = FINAL_PRED_IND),"000"), Timediff = Timediff))  
}