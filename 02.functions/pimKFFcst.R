# modelo de fatores para PIM

pimKFFcst <- function(pim = NULL, data = NULL, type = "univariate", carnaval = NULL, pim_cajuste = NULL, h = 24){
  
  n.fcst <- h
  
  if(type == "univariate"){
    
    M <- matrix(NA, ncol = 1, nrow = n.fcst)
    
    for(i in n.fcst:1){
      y <- na.omit(pim)
      y[which(as.Date(y) %in% tail(as.Date(y),i))] <- NA
      y <- ts(c(na.omit(y), NA), start = start(y), freq = 12)
      m <- StructTS(y, type = "BSM")
      fit <- ts(rowSums(fitted(m)), start = start(fitted(m)), freq = 12)
      M[i,] <- tail(fit,1)
    }
    
    previsao <- ts(M[n.fcst:1], end = end(fit), freq = 12)
    previsao_cajuste <- pimSeasFcst(pim, pim_cajuste, previsao, carnaval)
    previsao_margem <- (previsao_cajuste/lag(pim_cajuste,-1)-1)*100
    previsao_inter <- 100*(previsao/lag(pim,-12)-1)
    
    # output
    list(nivel = list(RMSE = RMSE(pim, previsao), MAPE = MAPE(pim, previsao)),
         margem = list(RMSE = RMSE(pim_margem, previsao_margem)),
         inter = list(RMSE = RMSE(pim_inter, previsao_inter)),
         sinal_margem = mean(sign(previsao_margem) == sign(pim_margem))*100,
         sinal_inter = mean(sign(previsao_inter) == sign(pim_inter))*100,
         out = cbind(previsao, 
                     previsao_cajuste,
                     previsao_margem, 
                     previsao_inter))
    
  }else if(type == "multivariate"){
    pim <- na.omit(data[,"PIM"])
    M <- matrix(NA, ncol = 4, nrow = n.fcst)
    colnames(M) <- c("previsao","prev_cajuste","previsao_margem",'previsao_inter')
    data0 <- window(data, start = start(na.omit(data[,"PIM"])), end = end(na.omit(data[,"PIM"])), freq = 12)
    n <- nrow(data0)
    for(i in 1:n.fcst){
      x <- head(data0, n - n.fcst + i -1)
      m <- pimKF(data = x, type = "multivariate", carnaval = carnaval, pim_cajuste = pim_cajuste)
      M[i,"previsao"] <- m$prev_nivel
      M[i,"prev_cajuste"] <- m$prev_nivel_cajuste
      M[i,"previsao_margem"] <- m$prev_margem
      M[i,"previsao_inter"] <- m$prev_inter
     # print(i)
    }
    
    M <- ts(M, end = end(data0), freq = 12)
    ts.plot(pim_cajuste, M[,"prev_cajuste"], col = 1:2)
    ts.plot(pim, M[,"previsao"], col = 1:2)
    
    list(nivel = list(RMSE = RMSE(pim, M[,"previsao"]), MAPE = MAPE(pim, M[,"previsao"])),
         margem = list(RMSE = RMSE(pim_margem, M[,"previsao_margem"])),
         inter = list(RMSE = RMSE(pim_inter, M[,"previsao_inter"])),
         sinal_margem = mean(sign(M[,"previsao_margem"]) == sign(pim_margem))*100,
         sinal_inter = mean(sign(M[,"previsao_inter"]) == sign(pim_inter))*100,
         out = M)
    
  }
}

# for(i in n.fcst:1){
#   
#   all0 <- head(all, nrow(all) - i)
#   pim <- na.omit(all0[,"PIM"])
#   data0 <- all0[,-which(colnames(all0) == "PIM")]
#   
#   # variáveis estacionárias e padronizadas
#   y_padFULLts <- ts(diff(apply(data0, 2, FUN = function(x) (x - mean(x,na.rm = T))/sd(x,na.rm = T))), end = end(data), freq = 12)
#   y_padFULL <- t(diff(apply(data0, 2, FUN = function(x) (x - mean(x,na.rm = T))/sd(x,na.rm = T))))
#   y_pad <- t(na.omit(diff(apply(data0, 2, FUN = function(x) (x - mean(x,na.rm = T))/sd(x,na.rm = T)))))
#   
#   n <- nrow(y_pad) # número de variáveis
#   t <- ncol(y_pad) # tamanho da amostra
#   
#   S <- (y_pad %*% t(y_pad))/t
#   D <- diag(eigen(S)$values[1:r],r)
#   V <- matrix(eigen(S)$vectors[,1:r], ncol = r)
#   F_hat <- t(V) %*% y_pad
#   
#   lambda <- cbind(V, matrix(0, ncol = r*p - ncol(V), nrow = nrow(V)))  #y_pad %*% t(F_hat) %*% solve(F_hat %*% t(F_hat))
#   psi <- diag(diag(S - V %*% D %*% t(V)))
#   Fhat <- ts(t(F_hat))
#   X <- t(na.omit(cbind(Fhat, eval(parse(text = paste0("cbind(",paste0("lag(Fhat,-",1:p,")", collapse = ","),")"))))))#[1:(r*p),]
#   rownames(X) <- 1:nrow(X)
#   
#   A_hat <- t(matrix(X[1:(r*p),], nrow = r*p) %*% t(matrix(X[-(1:(r*p)),], nrow = nrow(X) - r*p)) %*% solve(matrix(X[-(1:(r*p)),], nrow = nrow(X) - r*p) %*% t(matrix(X[-(1:(r*p)),], nrow = nrow(X) - r*p))))
#   A_hat <- rbind(A_hat, cbind(diag(1,r*p-r), matrix(0, ncol = r, nrow = r*p-r))) 
#   
#   if(r == q){
#     Q <- matrix(0, nrow = r*p, ncol = r*p)
#     e <- X[1:(r*p),] - A_hat %*%  X[-(1:r),]#, nrow = r*p) 
#     H <- cov(t(e))[1:r,1:r]
#     Q[1:r,1:r] <- H
#   }else{
#     SIGMA_hat <- matrix(X[1:r,], nrow = r) %*% t(matrix(X[1:r,], nrow = r)) - matrix(A_hat[1:r,], nrow = r) %*% (matrix(X[-(1:r),], nrow = nrow(X) - r) %*% t(matrix(X[-(1:r),], nrow = nrow(X) - r))) %*% t(matrix(A_hat[1:r,], nrow = r))
#     P_hat <- diag(eigen(SIGMA_hat)$values[1:q],q)
#     P_hat <- P_hat %*% sign(P_hat)
#     M_hat <- matrix(eigen(SIGMA_hat)$vectors[,1:q], ncol = q)
#     B_hat <- M_hat %*% sqrt(P_hat)
#     Q <- matrix(0, nrow = r*p, ncol = r*p)
#     Q[1:r,1:r] <- M_hat %*% P_hat %*% t(M_hat)
#   }
#   
#   yt <- y_padFULL
#   m <- nrow(A_hat)
#   d <- nrow(yt)
#   
#   ## Set constant parameters:
#   ct <- matrix(0, nrow = d, ncol = 1)
#   dt <- matrix(0, nrow = m, ncol = 1)
#   
#   a0 <- rep(0,m) # Estimation of the first year flow
#   P0 <- diag(100,m) # Variance of 'a0'
#   
#   model <- fkf(a0, P0, dt, ct,  yt = yt, 
#                HHt = Q,
#                GGt = psi, 
#                Zt = lambda,
#                Tt = A_hat,
#                check.input = T)
#   
#   if(r == 1){
#     fator <- ts(model$att[1:r,], end = end(y_padFULLts), freq = 12)
#   }else{
#     fator <- ts(t(model$att[1:r,]), end = end(y_padFULLts), freq = 12)
#   }
#   Zt <- lambda
#   resid <- ts(t(model$vt),  start = start(y_pad), freq = 12)[,1]
#   
#   # modelo linear para estimar PIM
#   pim_pad <- (pim - mean(pim))/sd(pim)
#   pim_estac <- diff(pim_pad)
#   
#   if(is.null(lagsy)){
#     ys <- pim_estac
#     nomes_ys <- "PIM"
#   }else{
#     ys <- eval(parse(text = paste0("cbind(pim_estac,",paste0("lag(pim_estac, -",lagsy,")", collapse = ","),")")))
#     nomes_ys <- c("PIM",paste0("PIM",lagsy))
#   }
#   
#   variaveis0 <- cbind(ys,fator)
#   variaveis0 <- window(variaveis0, start = start(na.omit(variaveis0)), end = as.numeric(c(tail(substr(as.Date(pim) + months(1), 1,4),1), tail(substr(as.Date(pim) + months(1), 6,7),1))), freq = 12)
#   colnames(variaveis0) <- c(nomes_ys, paste0("fator",1:r))
#   
#   m <- lm(PIM ~ ., data = na.omit(variaveis0))
#   fit <- ts(fitted(m), end = end(na.omit(variaveis0)), freq = 12)
#   
#   # previsão
#   if(ncol(variaveis0) == 2){
#     prev <- ts(predict(m, data.frame(fator1 = tail(variaveis0[,-1],sum(is.na(variaveis0))))), end = end(variaveis0 <- cbind(pim_estac,fator)), freq = 12)
#   }else{
#     prev <- ts(predict(m, tail(variaveis0[,-1],sum(sum(is.na(variaveis0))))), end = end(variaveis0 <- cbind(pim_estac,fator)), freq = 12)
#   }
#   prev_nivel <- (prev + c(tail(pim_pad,1)))*sd(pim) + mean(pim)
#   
#   M[n.fcst-i+1,] <- prev_nivel
# }
# 
# previsao <- ts(M, end = end(all), freq = 12)
# prev_cajuste <- pimSeasFcst(pim, pim_cajuste, previsao, carnaval)
# previsao_margem <- (prev_cajuste/lag(pim_cajuste,-1)-1)*100
# previsao_inter <- 100*(previsao/lag(pim,-12)-1)
# 
# pim <- data[,"PIM"]