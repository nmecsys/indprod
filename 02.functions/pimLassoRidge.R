pimLassoRidge <- function(data, log = T, carnaval = NULL, diff = T, outDFM = NULL, pim_cajuste = NULL, alpha = NULL){
  
  #data <- database$dados2
  
  message("Variaveis previstas: ",paste0(colnames(data)[which(is.na(tail(data,1)) & colnames(data) != "PIM")], collapse = ", "))
  out <- outDFM[,which(colnames(outDFM) %in% colnames(data))]
  data[(nrow(data)-3):nrow(data),-which(colnames(data) == "PIM")] <- out
  
  if(log){data <- log(data)}
  pim <- na.omit(data[,"PIM"])
  
  # padronizar
  mean_data <- apply(data, MARGIN = 2, FUN = mean, na.rm = T)
  sd_data <- apply(data, MARGIN = 2, FUN = sd, na.rm = T)
  data_pad <- ts(apply(data, MARGIN = 2, FUN = function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)),
                 start = start(data), freq = 12)
  #data_pad[nrow(data_pad),"PIM"] <- NA
  
  data_pad_ok <- data_pad
  if(diff){
    last_pim <- tail(data_pad,2)[1]
    data_pad_ok <- ts(rbind(NA,diff(data_pad)), start = start(data_pad), freq = 12)
  }
  
  data_in <- na.omit(as.matrix(data_pad_ok[-nrow(data_pad_ok),]))
  data_out <- t(data_pad_ok[nrow(data_pad_ok),-which(colnames(data_pad_ok) == "PIM")])
  
  cv.out <- cv.glmnet(x = data_in[,-which(colnames(data_pad_ok) == "PIM")], y = data_in[,"PIM"],
                      alpha = alpha)
  m <- glmnet(x = data_in[,-which(colnames(data_pad_ok) == "PIM")], y = data_in[,"PIM"],
              alpha = alpha, lambda = cv.out$lambda.min)
  
  fcst <- predict(m, s = cv.out$lambda.min, newx = data_out)
  fit <- ts(predict(m,  s = cv.out$lambda.min, newx = data_in[,-which(colnames(data_pad_ok) == "PIM")]), end = end(na.omit(data[,"PIM"])), freq = 12)
  
  if(diff){
    fcst <- fcst + last_pim
    fit <- ts(fit + lag(data_pad[,'PIM'],-1), end = end(fit), freq = 12)
  }
  
  # despadronizar
  fcst <- fcst*sd_data["PIM"] + mean_data["PIM"]
  fit <-  fit*sd_data["PIM"] + mean_data["PIM"]
  
  if(log){
    pim <- exp(pim)
    fcst <- exp(fcst)
    fit <- exp(fit)
  }
  
  prev_nivel <- ts(fcst, end = end(data), freq = 12)
  pim_ajustada <- pimSeas(pim = ts(c(pim, prev_nivel), start = start(pim), freq = 12), carnaval)
  fit_ajustado <- pimSeas(pim = fit, carnaval)
  
  prev_margem <- (tail(pim_ajustada,1)/c(tail(pim_cajuste,1))-1)*100
  prev_inter <- (prev_nivel/tail(pim,12)[1]-1)*100
  
  resid <- pim-fit
  list(fit = fit, fit_ajustado = fit_ajustado, resid = resid, prev_nivel = prev_nivel, prev_nivel_cajuste = tail(pim_ajustada,1), prev_margem = prev_margem, prev_inter = prev_inter)
  
}