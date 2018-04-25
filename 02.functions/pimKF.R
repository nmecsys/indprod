# modelo de fatores para PIM

pimKF <- function(pim = NULL, data = NULL, type = "univariate", carnaval = NULL, pim_cajuste = NULL){
  
  if(type == "univariate"){
    
    y <- ts(c(na.omit(pim),NA), start = start(na.omit(pim)), freq = 12)
    
    m <- StructTS(y, type = "BSM")
    fit <- ts(rowSums(fitted(m)), start = start(fitted(m)), freq = 12)
    
    prev_nivel <- tail(fit,1)
    fit_ajustado <- pimSeas(pim = fit, carnaval)
    pim_ajustada <- pimSeas(pim = ts(c(pim, prev_nivel), start = start(y), freq = 12), carnaval)
    
    prev_margem <- ts((tail(pim_ajustada,1)/c(tail(pim_cajuste,1))-1)*100, start = start(prev_nivel), freq = 12)
    prev_inter <- (prev_nivel/tail(pim,12)[1]-1)*100
    
    # output
    list(fit = fit, 
         fit_ajustado = fit_ajustado, 
         prev_nivel = prev_nivel, 
         prev_nivel_cajuste = tail(pim_ajustada,1), 
         prev_margem = prev_margem , 
         prev_inter = prev_inter)
    
    
  }else if(type == "multivariate"){
    
    r <- 1
    data <- window(data, start = start(na.omit(data[,"PIM"])), freq = 12)
    pim <- na.omit(data[,"PIM"])
    y <- month2qtr(pim)
    pim_position <- which(colnames(data) == "PIM")
    base <- Bpanel(base = data[,-pim_position],
                   trans = rep(4, ncol(data) - 1),
                   aggregate = T)
    now <- nowcast(y = y, x = base, r = 1, p = 1, q = 1, method = '2sq')
    
    fator <- now$factors$dynamic_factors
    fcst_x <- now$xfcst + lag(data[,-pim_position] ,-1) + lag(data[,-pim_position] ,-12) - lag(data[,-pim_position] ,-13)
    colnames(fcst_x) <- colnames(now$xfcst)
    fcst_x <- tail(window(fcst_x, end = end(data), freq = 12),4)
    
    pim_estac <- diff(diff(pim),12)
    lagsy = c(1,12)
    ys <- eval(parse(text = paste0("cbind(pim_estac,",paste0("lag(pim_estac, -",lagsy,")", collapse = ","),")")))
    nomes_ys <- c("PIM",paste0("PIM",lagsy))
    
    variaveis0 <- cbind(ys,fator)
    variaveis0 <- window(variaveis0, start = start(na.omit(variaveis0)), end = as.yearmon(as.Date(tail(na.omit(variaveis0),1)) + months(1)), freq = 12)
    colnames(variaveis0) <- c(nomes_ys, paste0("fator",1:r))

    m <- lm(PIM ~ ., data = na.omit(variaveis0))
    print(summary(m))
    fit <- ts(fitted(m), end = end(na.omit(variaveis0)), freq = 12)
    
    # previsão
    if(ncol(variaveis0) == 2){
      prev <- ts(predict(m, data.frame(fator1 = tail(variaveis0[,-1],sum(is.na(variaveis0))))), end = end(variaveis0 <- cbind(pim_estac,fator)), freq = 12)
    }else{
      prev <- ts(predict(m, tail(variaveis0[,-1],sum(sum(is.na(variaveis0))))), end = end(variaveis0), freq = 12)
    }
    # retornar pro nível e despadronizar
    fit_nivel <- fit + lag(pim,-1) + lag(pim ,-12) - lag(pim ,-13)
    prev_nivel <- prev + c(tail(lag(pim,-1) + lag(pim ,-12) - lag(pim ,-13),1))
    
    fit_ajustado <- pimSeas(pim = fit_nivel, carnaval)
    pim_ajustada <- pimSeas(pim = ts(c(pim, prev_nivel), start = start(pim), freq = 12), carnaval)
    prev_margem <- ts((tail(pim_ajustada,1)/c(tail(pim_cajuste,1))-1)*100, start = start(prev_nivel), freq = 12)
    prev_inter <- (prev_nivel/tail(pim,12)[1]-1)*100
    
    pim_ajustada <- pimSeas(pim = ts(c(pim, prev_nivel), start = start(pim), freq = 12), carnaval)
    prev_margem <- ts((tail(pim_ajustada,1)/c(tail(pim_cajuste,1))-1)*100, start = start(prev_nivel), freq = 12)
    prev_inter <- (prev_nivel/tail(data[,"PIM"],12)[1]-1)*100
    
    list(fit = fit, fit_ajustado = pim_ajustada, prev_nivel = prev_nivel, prev_nivel_cajuste = tail(pim_ajustada,1), prev_margem = prev_margem , prev_inter = prev_inter, fator = fator, fcst_x = fcst_x)
    
  }
}

# list(A = A_hat, B = B_hat, SIGMA = SIGMA_hat, V = V, 
#      P = P_hat, M = M_hat, D = D)
# 
# library(dynlm)
# k <- ts(t(F_hat))
# a <- dynlm(k ~ 0 + L(k,-1))
# summary(a)
# A_hat
# library(nowcasting)
# 
# pib<-BRGDP[,8]
# y<-month2qtr(diff(diff(pib,3),12))
# x<-Bpanel(BRGDP[,-8],rep(2,dim(BRGDP)[2]),aggregate = F)
# q<-1
# r<-1
# p<-1
# 
# now_2sq<-nowcast(y,x,q,r,p,method = '2sm')
# now_2sq$factors$A
# now_2sq$factors$C
# now_2sq$factors$Q
# now_2sq$factors$eigen$values
# ts.plot(t(F_hat    ), ylim = c(-4,3))
# plot((now_2sq$factors$dynamic_factors))
# fim PCA
# initial <- c(rep(0, m^2 + 1), loadings, rep(0,m^2)) #rep(0, m^2 + d*m + m^2 + 1 )
# 
# objective <- function(par, ...){
#   HHt <- matrix(par[1:m^2], nrow = m, ncol = m)
#   diag(HHt) <- exp(diag(HHt))
#   
#   -fkf(HHt = HHt,
#        GGt = diag(exp(par[(m^2+1)]),d), 
#        Zt = matrix(par[(m^2+1+1):(m^2+1+d*m)], nrow = d, ncol = m, byrow = T),
#        Tt = matrix(par[(m^2+1+d*m+1):(m^2+1+d*m+m^2)], nrow = m, ncol = m, byrow = T), ...)$logLik
# }
# 
# otimo_fkf <- nlminb(start = initial, objective = objective,
#                     yt = yt, a0 = a0, P0 = P0, dt = dt, ct = ct,
#                     lower = rep(-Inf,m^2+1+d*m+m^2), 
#                     upper = rep(Inf,m^2+1+d*m+m^2), control = list(eval.max = 10000, iter.max = 10000))
# 
# # estimar com parametros ótimos
# 
# HHt <- matrix(otimo_fkf$par[1:m^2], nrow = m, ncol = m)
# diag(HHt) <- exp(diag(HHt))
# 
# model <- fkf(a0, P0, dt, ct,  yt = yt, 
#              HHt = HHt,
#              GGt = diag(exp(otimo_fkf$par[(m^2+1)]),d), 
#              Zt = matrix(otimo_fkf$par[(m^2+1+1):(m^2+1+d*m)], nrow = d, ncol = m, byrow = T),
#              Tt = matrix(otimo_fkf$par[(m^2+1+d*m+1):(m^2+1+d*m+m^2)], nrow = m, ncol = m, byrow = T),
#              check.input = T)


### Method EM
#   # selecting and transforming y  
#   pimQ <- month2qtr(x = pim)
#   pimQ <- ts(c(pimQ,NA,NA,NA,NA), start = start(pimQ), frequency = 4)
#   pim_stationary <- pimQ - lag(pimQ, k = -1) - lag(pimQ, k = -12) +  lag(pimQ, k = -13)
#   
#   # selecting and transforming x 
#   stationaryBase <- data0 - lag(data0, k = -1) - lag(data0, k = -12) +  lag(data0, k = -13)
#   colnames(stationaryBase) <- colnames(data0)
#   
#   nowEM <- nowcast(y = pim_stationary, x = stationaryBase, r = 1, p = 1, q = 1, method = 'EM')
#   fcst_x <- nowEM$xfcst + lag(data0 ,-1) + lag(data0 ,-12) - lag(data0 ,-13)
#   colnames(fcst_x) <- colnames(nowEM$xfcst)
#   fator <- nowEM$factors$dynamic_factors[,1]
#   #fcst_x <- tail(window(fcst_x, end = end(data), freq = 12),4)
#   
#   pim_estac <- diff(diff(pim),12)
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
#   print(summary(m))
#   fit <- ts(fitted(m), end = end(na.omit(variaveis0)), freq = 12)
#   #ts.plot(variaveis0[,"PIM"], fit, col = 1:2)
#   
#   # previsão
#   if(ncol(variaveis0) == 2){
#     prev <- ts(predict(m, data.frame(fator1 = tail(variaveis0[,-1],sum(is.na(variaveis0))))), end = end(variaveis0 <- cbind(pim_estac,fator)), freq = 12)
#   }else{
#     prev <- ts(predict(m, tail(variaveis0[,-1],sum(sum(is.na(variaveis0))))), end = tail(as.yearmon(as.Date(na.omit(variaveis0)) + months(1)),1), freq = 12)
#   }
#   # retornar pro nível e despadronizar
#   fit_nivel <- fit + lag(pim, k = -1) + lag(pim, k = -12) - lag(pim, k = -13)
#   prev_nivel <- prev + lag(pim, k = -1) + lag(pim, k = -12) - lag(pim, k = -13)
#   
#   fit_ajustado <- pimSeas(pim = fit_nivel, carnaval)
#   pim_ajustada <- pimSeas(pim = ts(c(pim, prev_nivel), start = start(pim), freq = 12), carnaval)
#   prev_margem <- ts((tail(pim_ajustada,1)/c(tail(pim_cajuste,1))-1)*100, start = start(prev_nivel), freq = 12)
#   prev_inter <- (prev_nivel/tail(pim,12)[1]-1)*100
#   
#   # output
#   list(fit = fit_nivel, 
#        fit_ajustado = fit_ajustado, 
#        prev_nivel = prev_nivel, 
#        prev_nivel_cajuste = tail(pim_ajustada,1), 
#        prev_margem = prev_margem , 
#        prev_inter = prev_inter, 
#        resid = resid(m))
# }
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
#   print(summary(m))
#   fit <- ts(fitted(m), end = end(na.omit(variaveis0)), freq = 12)
#   #ts.plot(variaveis0[,"PIM"], fit, col = 1:2)
#   
#   # previsão
#   if(ncol(variaveis0) == 2){
#     prev <- ts(predict(m, data.frame(fator1 = tail(variaveis0[,-1],sum(is.na(variaveis0))))), end = end(variaveis0 <- cbind(pim_estac,fator)), freq = 12)
#   }else{
#     prev <- ts(predict(m, tail(variaveis0[,-1],sum(sum(is.na(variaveis0))))), end = end(variaveis0 <- cbind(pim_estac,fator)), freq = 12)
#   }
#   # retornar pro nível e despadronizar
#   fit_nivel <- (fit + lag(pim_pad,-1))*sd(pim) + mean(pim)
#   prev_nivel <- (prev + c(tail(pim_pad,1)))*sd(pim) + mean(pim)
#   
#   fit_ajustado <- pimSeas(pim = fit_nivel, carnaval)
#   pim_ajustada <- pimSeas(pim = ts(c(pim, prev_nivel), start = start(pim), freq = 12), carnaval)
#   prev_margem <- ts((tail(pim_ajustada,1)/c(tail(pim_cajuste,1))-1)*100, start = start(prev_nivel), freq = 12)
#   prev_inter <- (prev_nivel/tail(pim,12)[1]-1)*100
#   
#   # output
#   list(fit = fit_nivel,
#        fit_ajustado = fit_ajustado,
#        prev_nivel = prev_nivel,
#        prev_nivel_cajuste = tail(pim_ajustada,1),
#        prev_margem = prev_margem ,
#        prev_inter = prev_inter,
#        resid = resid(m))
# }