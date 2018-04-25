# modelo de fatores - exercício de previsão dentro da amostra
pimFactorsFcst <- function(data, n.fatores = 1, p = 1, lagsy = c(1,12), lagsf = c(12), log = F, carnaval = NULL, pim_cajuste = NULL, diff = F, h = 24){
  # data=database$dados2
  # lagsy = NULL
  # lagsf = NULL
  # n.fatores = 1
  # p = 1,
  # log = F
  # diff = T
  #h = 24
  M <- matrix(NA, ncol = 1, nrow = h)

  if(log){data[,"PIM"] <- log(data[,"PIM"])}
  
  data <- window(data, start = start(na.omit(data[,"PIM"])), end = end(na.omit(data[,"PIM"])), freq = 12)
  pim <- na.omit(data[,"PIM"])
  pim_inter <- 100*(pim/lag(pim,-12)-1)
  
  data0 <- data.frame(data = as.Date(data), data)
  data0$prever <- c(rep(NA,nrow(data0)-h),h:1)
  
  first_fcst <- max(which(is.na(data0$prever)))
  pos <- which(colnames(data) == "PIM")
  
  for(i in first_fcst:(nrow(data0)-1)){
    X <- ts(data0[1:i,-c(1,ncol(data0))], start = start(data), freq = 12)
    # y <- month2qtr(X[,"PIM"])
    # base <- Bpanel(base = X[,-pos], trans = rep(2, ncol(X) - 1), aggregate = F)
    y <- month2qtr(diff(diff(X[,"PIM"],12)))
    y <- ts(c(y,NA), start = start(y), freq = 4)
    pos <- which(colnames(data) == "PIM")
    base <- diff(diff(X[,-pos],12))
    now <- nowcast(y = y, x = base, q = n.fatores, r = n.fatores, p = p, method = "EM")
    fator <- now$factors$dynamic_factors
    if(p == 1){
      fator <- fator[,1]
    }else{
      fator <- fator[,1:n.fatores]
    }
    if(is.null(lagsy)){
      ys <- NULL
      nomes_ys <- NULL
    }else{
      ys <- eval(parse(text = paste0("cbind(",paste0("lag(X[,'PIM'], -",lagsy,")", collapse = ","),")")))
      nomes_ys <- paste0("PIM",lagsy)
    }
    if(is.null(lagsf)){
      fs <- NULL
      nomes_fs <- NULL
    }else{
      fs <- eval(parse(text = paste0("cbind(",paste0("lag(fator, -",lagsf,")", collapse = ","),")")))
      nomes_fs <- paste0("fator",1:n.fatores,".",rep(lagsf,each = n.fatores))
    }
    
    data_reg <- window(cbind(X[,"PIM"], ys, fator, fs), start = start(fator), end = end(fator), freq = 12)
    colnames(data_reg) <- c("PIM", nomes_ys, paste0("fator",1:n.fatores), nomes_fs)
    
    if(diff){
      if(length(c("PIM",nomes_ys)) == 1){
        data_reg[,c("PIM",nomes_ys)] <- c(NA,diff(data_reg[,c("PIM",nomes_ys)]))
      }else{
        data_reg[,c("PIM",nomes_ys)] <- rbind(NA,diff(data_reg[,c("PIM",nomes_ys)]))
      }
    }
    
    data_in <- data.frame(data_reg[1:i,])
    data_out <- data.frame(t(data_reg[i+1,-1]))
    m <- lm(PIM~., data = data_in)
    print(Box.test(m$residuals), type = "Ljung-Box", lag = 12)
    print(summary(m))
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
