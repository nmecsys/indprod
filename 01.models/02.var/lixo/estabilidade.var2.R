estabilidade.var <- function(x = NULL, h = 12, exo = NULL){
  
  #x = data
  # x = data[,c(1:6)] 
  # h = 12
  # exo = data[,c(7,8)]
  
  M <- matrix(NA, ncol = 1, nrow = h)
  for(i in h:1){
    var <- vars::VAR(x[1:(nrow(x)-i),], p = 2, exogen = exo[1:(nrow(x)-i),])
    var_for <- predict(var, n.ahead=1, ci=0.95, dumvar = t(exo[(nrow(x)-i)+1,]))
    M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
  }
  
  ts(M, end = end(x), freq = 12)
}
  
  