pimLMFcst <- function(data, outDFM = NULL, lagsy = NULL, log = F, carnaval = NULL, pim_cajuste = NULL, diff = F, h = 24){
  
  out <- outDFM[,which(colnames(outDFM) %in% colnames(data))]
  data[(nrow(data)-3):nrow(data),-which(colnames(data) == "PIM")] <- out
  data <- window(data, start = start(na.omit(data[,"PIM"])), end = end(na.omit(data[,"PIM"])), freq = 12)
  
  
  #h = 24
  M <- matrix(NA, ncol = 1, nrow = h)
  
  if(log){data[,"PIM"] <- log(data[,"PIM"])}
  
  pim <- data[,"PIM"]
  pim_inter <- 100*(pim/lag(pim,-12)-1)
  
  data0 <- data.frame(data = as.Date(data), data)
  data0$prever <- c(rep(NA,nrow(data0)-h),h:1)
  
  first_fcst <- max(which(is.na(data0$prever)))
  
  if(is.null(lagsy)){
    ys <- NULL
    nomes_ys <- NULL
  }else{
    ys <- eval(parse(text = paste0("cbind(",paste0("lag(data[,'PIM'], -",lagsy,")", collapse = ","),")")))
    nomes_ys <- paste0("PIM",lagsy)
  }
  
  data_reg <- window(cbind(data, ys), end = end(pim), freq = 12)
  colnames(data_reg) <- c(colnames(data),nomes_ys)
  
  if(diff){
    data_reg <- rbind(NA,diff(data_reg))
  }
  
  
  for(i in first_fcst:(nrow(data0)-1)){
    data_in <- data.frame(data_reg[1:i,])
    data_out <- data.frame(t(data_reg[i+1,-which(colnames(data_reg) == "PIM")]))
    m <- lm(PIM~., data = data_in)
    m_fcst <- predict(m, n.ahead = 1, newdata = data_out)
    M[nrow(data0)-i,] <- m_fcst
  }
  
  data_prev <-  data0$data[max(which(is.na(data0$prever)))+1]
  previsao <- ts(M[h:1], start = as.numeric(c(substr(data_prev,1,4),substr(data_prev,6,7))), freq = 12)
  
  if(diff){
    previsao <- ts(previsao + lag(pim,-1), end = end(previsao), freq = 12)
  }
  
  if(log){ 
    previsao <- ts(exp(previsao), start = start(previsao), freq = 12)
    data[,"PIM"] <- exp(data[,"PIM"])
    pim <- data[,"PIM"]
  }
  
  prev_cajuste <- pimSeasFcst(pim, pim_cajuste, previsao, carnaval)
  previsao_margem <- (prev_cajuste/lag(pim_cajuste,-1)-1)*100
  previsao_inter <- 100*(previsao/lag(pim,-12)-1)
  
  
  list(nivel = list(RMSE = RMSE(pim, previsao), MAPE = MAPE(pim, previsao)),
       margem = list(RMSE = RMSE(pim_margem, previsao_margem)),
       inter = list(RMSE = RMSE(pim_inter, previsao_inter)),
       sinal_margem = mean(sign(previsao_margem) == sign(pim_margem))*100,
       sinal_inter = mean(sign(previsao_inter) == sign(pim_inter))*100,
       out = cbind(previsao, 
                   prev_cajuste,
                   previsao_margem, 
                   previsao_inter))
}