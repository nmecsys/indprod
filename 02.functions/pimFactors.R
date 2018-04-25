# modelo de fatores para PIM

pimFactors <- function(data, lagsy = NULL, lagsf = NULL, n.fatores = 1, p = 1, log = F, carnaval = NULL, pim_cajuste = NULL, diff = F){
  
if(log){data[,"PIM"] <- log(data[,"PIM"])}

  data <- window(data, start = start(na.omit(data[,"PIM"])), freq = 12)
  pim <- na.omit(data[,"PIM"])
  y <- month2qtr(diff(diff(pim,12)))
  y <- ts(c(y,NA), start = start(y), freq = 4)
  pos <- which(colnames(data) == "PIM")
  #base <- Bpanel(base = data[,-pos], trans = rep(2, ncol(data) - 1), aggregate = F)
  base0 <- data[,-pos]
  base <- diff(diff(data[,-pos],12))
  now <- nowcast(y = y, x = base, q = n.fatores, r = n.fatores, p = p, method = "EM")
  
  fator <- now$factors$dynamic_factors[,1]
  fcst_x <- now$xfcst + lag(base0 ,-1) + lag(base0 ,-12) - lag(base0 ,-13)
  colnames(fcst_x) <- colnames(now$xfcst)
  fcst_x <- window(fcst_x, end = tail(as.yearmon(as.Date(na.omit(data[,"PIM"])) + months(1)),1), freq = 12)
  base1 <- fcst_x
  nomes_na <- apply(fcst_x, MARGIN = 2, FUN = function(x) sum(is.na(x)) > 0)
  if(sum(nomes_na) > 0){
    for(i in names(nomes_na)[nomes_na]){
      prevs <- na.omit(now$xfcst[,i]) + tail(lag(na.omit(base1[,i]) ,-1),1) + lag(na.omit(base1[,i]) ,-12) - lag(na.omit(base1[,i]) ,-13)
      window(fcst_x[,i], start = start(prevs), end = end(prevs), freq = 12) <- prevs
    }
  }
  fcst_x <- tail(fcst_x,4)
  # #colnames(fcst_x) <- colnames(data[,-which(colnames(data) == "PIM")])
  # if(p != 1){
  #   fator <- fator[,1]
  # }else{
  #   fator <- fator[,1:n.fatores]
  # }
  
  if(is.null(lagsy)){
    ys <- NULL
    nomes_ys <- NULL
  }else{
    ys <- eval(parse(text = paste0("cbind(",paste0("lag(data[,'PIM'], -",lagsy,")", collapse = ","),")")))
    nomes_ys <- paste0("PIM",lagsy)
  }
  if(is.null(lagsf)){
    fs <- NULL
    nomes_fs <- NULL
  }else{
    fs <- eval(parse(text = paste0("cbind(",paste0("lag(fator, -",lagsf,")", collapse = ","),")")))
    nomes_fs <- paste0("fator",1:n.fatores,".",rep(lagsf,each = n.fatores))
  }
  
  data_reg <- window(cbind(data[,"PIM"], ys, fator, fs), start = start(fator), end = end(fator), freq = 12)
  colnames(data_reg) <- c("PIM",nomes_ys,paste0("fator",1:n.fatores), nomes_fs)
  
  if(diff){
    if(is.null(nomes_ys)){
      data_reg[,"PIM"] <- c(NA,diff(data_reg[,"PIM"]))
    }else{
      data_reg[,c("PIM",nomes_ys)] <- rbind(NA,diff(data_reg[,c("PIM",nomes_ys)]))  
    }
    
  }
  
  data_in <- na.omit(data.frame(data_reg))
  data_out <- data.frame(t(data_reg[max(as.numeric(rownames(data_in)))+1,-1]))
  
  m <- lm(PIM~., data = data_in)
  print(summary(m))
  
  fit <- ts(fitted(m), end = end(na.omit(data_reg)), freq = 12)
  if(diff){
    fit <- ts(fit + lag(pim,-1), end = end(fit), freq = 12)
  }
  
  if(log){
    fit <- exp(fit)
    data[,"PIM"] <- exp(data[,"PIM"])
    pim <- na.omit(data[,"PIM"])
  }
  
  resid <- ts(resid(m), end = end(data), freq = 12)
  data_prev <- tail(as.Date(pim),1) + months(1)
  m_fcst <- ts(predict(m, n.ahead = 1, newdata = data_out), start = as.numeric(c(substr(data_prev,1,4),substr(data_prev,6,7))), freq = 12)
  
  if(diff){
    m_fcst <- c(tail(pim,1)) + m_fcst  
  }
  pim_ajustada <- pimSeas(pim = ts(c(pim, m_fcst), start = start(pim), freq = 12), carnaval)
  prev_margem <- ts((tail(pim_ajustada,1)/c(tail(pim_cajuste,1))-1)*100, start = start(m_fcst), freq = 12)
  prev_inter <- (m_fcst/tail(data[,"PIM"],12)[1]-1)*100
  
  list(fit = fit, fit_ajustado = pim_ajustada, prev_nivel = m_fcst, prev_nivel_cajuste = tail(pim_ajustada,1), prev_margem = prev_margem , prev_inter = prev_inter, resid = resid, fator = fator, fcst_x = fcst_x)
  
}
