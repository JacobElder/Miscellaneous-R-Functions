# increase wave at every sign flip

set.seed(50)
nums<-round(runif(50,-100,100))
wave <- 0
waves <- matrix(nrow=length(nums),ncol=1)
for(i in 1:(length(nums)-1) ){
  if(sign(nums[i+1])==sign(nums[i])){
    wave <- wave
  }else{
    wave <- wave + 1
  }
  waves[i+1] <- wave
}
