pimLM <- function(data, lagsy = NULL, log = F, carnaval = NULL, diff = F, outDFM = NULL){
  
  message("Variaveis previstas: ",paste0(colnames(data)[which(is.na(tail(data,1)) & colnames(data) != "PIM")], collapse = ", "))
  out <- outDFM[,which(colnames(outDFM) %in% colnames(data))]
  data[(nrow(data)-3):nrow(data),-which(colnames(data) == "PIM")] <- out
  
  pim <- na.omit(data[,"PIM"])
  if(log){data[,"PIM"] <- log(data[,"PIM"])}
  
  if(is.null(lagsy)){
    ys <- NULL
    nomes_ys <- NULL
  }else{
    ys <- eval(parse(text = paste0("cbind(",paste0("lag(data[,'PIM'], -",lagsy,")", collapse = ","),")")))
    nomes_ys <- paste0("PIM",lagsy)
  }
  
  data_reg <- window(cbind(data, ys), end = as.numeric(c(tail(substr(as.Date(pim) + months(1), 1,4),1), tail(substr(as.Date(pim) + months(1), 6,7),1))), freq = 12)
  colnames(data_reg) <- c(colnames(data),nomes_ys)
  
  if(diff){
    data_reg <- diff(data_reg)
  }
  
  data_in <- na.omit(data.frame(data_reg))
  data_out <- data.frame(t(data_reg[nrow(data_reg), -which(colnames(data_reg) == "PIM")]))
  m <- lm(PIM~., data = data_in)
  m_fcst <- predict(m, n.ahead = 1, newdata = data_out)
  #print(summary(m))
  
  fit <- ts(c(fitted(m)), end = end(na.omit(data_reg)), freq = 12)
  prev_nivel <- ts(m_fcst, end = as.numeric(c(tail(substr(as.Date(pim) + months(1), 1,4),1), tail(substr(as.Date(pim) + months(1), 6,7),1))), freq = 12)
  
  if(diff){
    fit <- ts(fit + lag(pim,-1), end = end(fit), freq = 12)
    prev_nivel <- ts(m_fcst + tail(pim,1), end = end(prev_nivel), freq = 12)
  }
  fit_ajustado <- pimSeas(pim = fit, carnaval)
  pim_ajustada <- pimSeas(pim = ts(c(pim, prev_nivel), start = start(pim), freq = 12), carnaval)
  
  prev_margem <- (tail(pim_ajustada,1)/c(tail(pim_cajuste,1))-1)*100
  prev_inter <- (prev_nivel/tail(pim,12)[1]-1)*100
  
  if(log){
    fit <- exp(fit)
    data[,"PIM"] <- exp(data[,"PIM"])
  }
  
  resid <- ts(resid(m), end = end(data), freq = 12)
  list(fit = fit, fit_ajustado = fit_ajustado, resid = resid, prev_nivel = prev_nivel, prev_nivel_cajuste = tail(pim_ajustada,1), prev_margem = prev_margem, prev_inter = prev_inter)
  
}