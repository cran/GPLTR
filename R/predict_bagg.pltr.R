predict_bagg.pltr <- function(bag_pltr, Y.name, newdata, type = "response", thresshold = 0.5)
{
  predict_glm <- lapply(bag_pltr, function(uw)
  {
    pred = predict.glm(uw, newdata = newdata, type = type)
    return(as.numeric(pred >= thresshold))
  })
  PRED_ERRORS_PBP <- sapply(1:length(bag_pltr),function(vvv)
  { 
    return(mean(newdata[Y.name] != predict_glm[[vvv]]))
  })
  PRED_ERROR_PBP <- mean(PRED_ERRORS_PBP)
  PROB_LIST <- lapply(bag_pltr, function(uu){
  pred = predict.glm(uu, newdata = newdata, type = type)
  return(pred)  
  })
  PROB_MAT <- matrix(unlist(PROB_LIST), ncol = length(bag_pltr), byrow = FALSE)
  PROB_VECT <- apply(PROB_MAT, 1, mean)
  FINAL_PRED_IND2 <- as.numeric(PROB_VECT >= thresshold)
  confusion2 <- table(FINAL_PRED_IND2, t(newdata[Y.name]), dnn = c("Predicted Class", "Observed Class"))
  PRED_ERROR2 <- mean(FINAL_PRED_IND2 != newdata[Y.name])
  PRED_IND_MAT <- matrix(unlist(predict_glm), ncol = length(bag_pltr), byrow = FALSE)
  FINAL_PRED_IND1 <- apply(PRED_IND_MAT,1,function(zz){return(names(which.max(table(zz))))})
  confusion1 <- table(FINAL_PRED_IND1, t(newdata[Y.name]), dnn = c("Predicted Class", "Observed Class"))
  PRED_ERROR1 <- mean(FINAL_PRED_IND1 != newdata[Y.name])
  return(list(FINAL_PRED_IND1 = FINAL_PRED_IND1, FINAL_PRED_IND2 = FINAL_PRED_IND2, PRED_IND_MAT = PRED_IND_MAT, PRED_ERROR1 = PRED_ERROR1, 
              PRED_ERROR2 = PRED_ERROR2, PROB_MAT = PROB_MAT, CONF1 = confusion1, CONF2 = confusion2))
  
}