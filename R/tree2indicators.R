tree2indicators <- function(fit)
{
  #	Retrieve splits for leaves
  leaves <- path.rpart(fit, nodes= row.names(fit$frame[ fit$frame$var == "<leaf>", ] ), print.it=FALSE )
  
  #	Number of leaves
  n_leaves = length(leaves)
  
  #	List of splits associated to each leaf
  list_leaves = lapply(leaves, function(u) return(u[-1]))
  
  
  #  Replace "<" by "<c("
  list_leaves_1 = lapply(list_leaves, function(u)
  {
    r = sapply(u, function(uu) return(sub("<","<c(", uu)))
    return(r)
  })
  #  Replace ">" by ">c(" 
  list_leaves_2 = lapply(list_leaves_1, function(u)
  {
    r = sapply(u, function(uu) return(sub(">",">c(", uu)))
    return(r)
  })
  #  Replace "=" by "%in%c("
  list_leaves_3 = lapply(list_leaves_2, function(u)
  {
    r = sapply(u, function(uu) return(sub("=","%in%c(", uu)))
    return(r)
  })
  #  Replace "<c(%in%" by "<="
  list_leaves_4 = lapply(list_leaves_3, function(u)
  {
    r = sapply(u, function(uu) return(sub("<c(%in%","<=", uu, fixed=T)))
    return(r)
  })
  #  Replace ">c(%in%" by ">="
  list_leaves_5 = lapply(list_leaves_4, function(u)
  {
    r = sapply(u, function(uu) return(sub(">c(%in%",">=", uu, fixed=T)))
    return(r)
  })
  #  Replace the closure with ")"
  list_leaves_6 = lapply(list_leaves_5, function(u)
  { 
    r = sapply(u, function(uu) return(paste(uu, "", sep = ")")))
    return(r)
  })
  
  #	List of indicators
  indicators = lapply(list_leaves_6, function(u)
  {
    return(paste(u, collapse=" & "))
  })
  
  return(indicators)
}
