estabilidade.var <- function(x, h = 12){
  
  #x = data
  M <- matrix(NA, ncol = 1, nrow = h)
  for(i in h:1){
    var<-VAR(x[1:(nrow(data)-i),], lag.max = 2)
    var_for <- predict(var, n.ahead=1, ci=0.95)
    M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
  }
  
  ts(M, end = end(x), freq = 12)
}
  
  