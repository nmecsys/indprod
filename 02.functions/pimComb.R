# combine forecasts
pimComb <- function(y = NULL, fcst = NULL){
  # y: data observed (ts or vector)
  # fcst: forecasts (ts or matrix)

  # mean
  weights_mean <- rep(1/ncol(fcst), ncol(fcst))
  comb_mean <- fcst %*% weights_mean

  # median
  comb_median <- apply(fcst, MARGIN = 1, FUN = median)
  
  # RMSE
  rmse <- eval(parse(text = paste0("c(", paste0("RMSE(y,fcst[,",1:ncol(fcst),"])", collapse = ", "),")")))
  weights_rmse <- (1/rmse)/sum(1/rmse)
  comb_rmse <- fcst %*% weights_rmse
  
  # ols
  data <- cbind(y,fcst)
  weights_ols <- lm(y ~ 0 + ., data = data)$coefficients
  comb_ols <- fcst %*% weights_ols
  
  combs <- cbind(comb_mean, comb_median, comb_rmse, comb_ols)
  colnames(combs) <- c("mean","median","rmse","ols")
 
  if(is.ts(y) & is.ts(fcst)){combs <- ts(combs, start = start(fcst), freq = 12)}
  
  # output
  list(combinations = combs, weights = list(mean = weights_mean, rmse = weights_rmse, ols = weights_ols))

}