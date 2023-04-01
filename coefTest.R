# Clogg, C. C., Petkova, E., & Haritou, A. (1995). Statistical methods for comparing regression coefficients between models. American Journal of Sociology, 100(5), 1261-1293. 

pTest <- function(Z){
  p <- 2*pnorm(-abs(Z))
  return(p)
}

zTest <- function(b1,b2,se1,se2){
  Z<-(b1 - b2)/sqrt( ((se1)^2) + ((se2)^2) )
  p <- pTest(Z)
  return(list("Z"=Z,"p"=p))
}


