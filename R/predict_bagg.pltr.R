predict_bagg.pltr <- function(bag_pltr, Y.name, newdata, type = "response", thresshold = 0.5)
{
  predict_glm <- lapply(bag_pltr, function(uw)
  {
    pred = predict.glm(uw, newdata = newdata, type = type)
    return(as.numeric(pred > thresshold))
  })
  PRED_ERRORS <- sapply(1:length(bag_pltr),function(vvv)
  { 
    return(mean(newdata[Y.name] != predict_glm[[vvv]]))
  })
  PRED_ERROR <- mean(PRED_ERRORS)
  PRED_IND <- matrix(unlist(predict_glm), ncol = length(bag_pltr), byrow = F)
  FINAL_PRED_IND <- apply(PRED_IND,1,function(zz){return(names(which.max(table(zz))))})
  
  return(list(FINAL_PRED_IND = FINAL_PRED_IND, PRED_IND = PRED_IND, PRED_ERROR = PRED_ERROR))
  
}