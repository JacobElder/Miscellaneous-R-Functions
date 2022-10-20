named.contr.sum<-function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else if (is.numeric(x) & length(x)==1L) {
    stop("cannot create names with integer value. Pass factor levels")
  }
  x<-contr.sum(x, ...)
  colnames(x) <- apply(x,2,function(x) 
    paste(names(x[x>0]), names(x[x<0]), sep="-")
  )
  x
}

named.effects.ref <- function(fact, ref){
  
  fact <- as.factor(fact) # ensure it's a factor
  Ls <- levels(fact) # levels
  others <- Ls[which(Ls!=ref)] # which is not a reference group
  fact <- factor(fact, levels = c(others, ref)) # shift the order
  contrasts(fact) <- named.contr.sum(levels(fact)) # retain meaningful level labels
  return(fact)
  
}
