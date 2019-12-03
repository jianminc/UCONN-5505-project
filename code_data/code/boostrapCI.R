### conduct bootstrap method to get confidence interval for non-normal sample with difference dispersion
bootci <- function(x,y){
  n1 = length(x)
  n2 = length(y)
  dif = NULL
  for(i in 1:1000){
    x1 = base::sample(x,n1,replace=TRUE)
    x2 = base::sample(y,n2,replace=TRUE)
    dif = c(dif, mean(x1)-mean(x2))
  }
  return(quantile(dif,prob=c(0.025,0.975)))
}
