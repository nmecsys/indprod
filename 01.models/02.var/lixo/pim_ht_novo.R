# check pckgs and dir ----------------------------------------------

setwd("C:/Users/daiane.mattos/Dropbox/08 Previsão da PIM-PF")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(vars, urca, tseries, forecast, readxl, xtable, seasonal, BETS)

source("02.var/estabilidade.var2.R")
source("04.sarima/aux_functions/dummy.R")

MAPE <- function(y,x){ 
  data <- na.omit(cbind(y,x))
  mean(abs((data[,1] - data[,2])/data[,1]))*100
}


# read data and ts -------------------------------------------------
data1 <- ts(read.csv2("00.data/V_Alerta_Antecedentes_Tendencias_PIM.csv")[,-1], start = c(2005,1), freq = 12)
pim <- readRDS("00.data/pim_comajuste.rds")
data <- window(cbind(pim,data1), start = c(2005,1), freq = 12)
colnames(data) <- c("PIM", colnames(data1))
plot(data)

dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2009,1))
dummies <- cbind(dummy1, dummy2, dummy3)


# variables and order
ord <- c("FGV","ONS","Anfavea","ABCR","ABPO...Nova", "Arrecadação.de.IPI",
         "Petrobrás", "IBS","Funcex","Fiesp","PIM")
data_var <- na.omit(cbind(data[,ord], dummies))
colnames(data_var) <- c(ord, "dummy1", "dummy2", "dummy3")
dim(data_var)
plot(data_var)

# análise das variáveis com mais correlação
mcor <- cor(data_var[,c(1:11)])
corrplot::corrplot(mcor)
mcor[11,]
# petrobrás e ici são as mais fracas em nível
data_var_variacao <- (data_var[,1:11]/lag(data_var[,1:11],-1) - 1)*100
colnames(data_var_variacao) <- colnames(data_var)[1:11]
mcor <- cor(data_var_variacao)
corrplot::corrplot(mcor)
mcor[11,]
# em variação, Fiesp, ABPO e Anfavea são mais fortes

# var estimation em variação sem cointegração -------------------------------------------------
data_var1 <- na.omit(cbind(data_var_variacao, data_var[,12:13]))
colnames(data_var1) <- c(colnames(data_var_variacao), "dummy1", "dummy2")

VARselect(data_var1[,c(1,3,8:11)], lag.max = 6, exogen = data_var1[,12:13])
var <- VAR(data_var1[,c(1,3,8:11)], p = 1, exogen = data_var1[,12:13])
summary(var)

# diagnost tests  -------------------------------------------------
serial.test(var)  # autocorrelação
arch.test(var) # heterocedasticidade
roots(var)
residuos <- ts(resid(var), end = end(data_var1), freq = 12)
plot(residuos)
resid_padr <- (residuos[,"PIM"] - mean(residuos[,"PIM"]))/sd(residuos[,"PIM"]) 
plot(resid_padr)
abline(h = c(-3,3), col = 2, lty = 2)

# forecast  -------------------------------------------------------
h = 24
M <- matrix(NA, ncol = 1, nrow = h)
for(i in h:1){
  var <- VAR(data_var1[1:(nrow(data_var1)-i),c(1,3,8:11)], p = 2, exogen = data_var1[1:(nrow(data_var1)-i),12:13])
  var_for <- predict(var, n.ahead=1, ci=0.95, dumvar = t(data_var1[(nrow(data_var1)-i)+1,12:13]))
  M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
}

previsao <- ts(M, end = end(data_var1), freq = 12)
d <- round(na.omit(cbind(previsao,data_var1[,"PIM"])),4)
ts.plot(d, col = 2:1)
sqrt(mean((d[,1] - d[,2])^2)) # RMSE
pim_margem <- (pim/lag(pim,-1)-1)*100
ts.plot(pim_margem, d[,1], col = 1:2)


# var estimation com vec ------------------------------------------
# teste de cointegração
VARselect(data_var_variacao[,c(1:11)], lag.max = 6, exogen = data_var[-1,12:13])
teste <- ca.jo(data_var[,c(1:11)], dumvar = data_var[,12:13], type = "eigen", K = 2, ecdet = "const")
summary(teste)
# 1 relação de cointegração considerando todas as variáveis
summary(cajorls(teste, r = 1)$rlm)

# removendo variáveis

VARselect(data_var_variacao[,c(1,3:4,9,11)], lag.max = 6, exogen = data_var[-1,12:13])
teste <- ca.jo(data_var[,c(1,3:4,9,11)], dumvar = data_var[,12:13], type = "eigen", K = 2, ecdet = "const")
summary(teste)
summary(cajorls(teste, r = 2)$rlm)

# modelo final
var2 <- vec2var(teste, r = 2)
summary(var2$vecm)


# diagnost tests  -------------------------------------------------
serial.test(var2)  # autocorrelação
arch.test(var2) # heterocedasticidade
residuos <- ts(resid(var2), end = end(data_var), freq = 12)
plot(residuos)
resid_padr <- (residuos[,"resids of PIM"] - mean(residuos[,"resids of PIM"]))/sd(residuos[,"resids of PIM"]) 
plot(resid_padr)
abline(h = c(-3,3), col = 2, lty = 2)

# forecast  -------------------------------------------------------
h = 24
M <- matrix(NA, ncol = 1, nrow = h)
for(i in h:1){
  teste <- ca.jo(data_var[1:(nrow(data_var)-i),c(1,3:4,9,11)], dumvar = data_var[1:(nrow(data_var)-i),12:13], type = "eigen", K = 2, ecdet = "const")
  var2 <- vec2var(teste, r = 2)
  var_for <- predict(var2, n.ahead=1, ci=0.95, dumvar = t(data_var[(nrow(data_var)-i)+1,12:13]))
  M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
}

previsao_var2_nivel <- ts(M, end = end(data_var), freq = 12)
prevs_margem <- data.frame(as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 1)))
prevs_margem$margem_prev <- (prevs_margem[,3]/prevs_margem[,2]-1)*100
prev_margem <- ts(na.omit(prevs_margem$margem), end = end(previsao_var2_nivel), freq = 12)
ts.plot(round(cbind(prev_margem,pim_margem),2), col = 2:1)
pim_margem <- (pim/lag(pim,-1) - 1)*100
sqrt(mean((pim_margem - prev_margem)^2)) # RMSE

# exportar resultados
# resultados2 <- list(prev_cajuste = prev_margem)
# saveRDS(resultados2, "02.var/previsao_VAR_2anos.rds")

# lixo ou não -------------------------------------
# # read data and ts 
# data1 <- na.omit(readRDS("00.data/data_dessaz.rds"))
# data <- window(data1, start = c(2002,1), end = c(2017,7), freq = 12)
# plot(data)
# 
# dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
# dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
# dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2009,1))
# dummies <- cbind(dummy1, dummy2, dummy3)
# 
# 
# # variables and order
# ord <- c("IIEBR","SELIC_R","CAMB","COMD","DESEM","PIM")
# data <- na.omit(cbind(data[,ord], dummies))
# colnames(data) <- c(ord, "dummy1", "dummy2", "dummy3")
# plot(data)
# 
# # var estimation 
# VARselect(data[,c(1:6)], lag.max = 6, exogen = data[,7:8])
# var <- VAR(data[,c(1:6)], lag.max = 2, exogen = data[,7:8])
# summary(var)
# 
# # diagnost tests  
# serial.test(var)  # autocorrelação
# arch.test(var) # heterocedasticidade
# roots(var)
# plot(ts(resid(var), end = end(data), freq = 12))
# 
# # forecast  
# h = 24
# M <- matrix(NA, ncol = 1, nrow = h)
# for(i in h:1){
#   var <- VAR(data[1:(nrow(data)-i),1:6], p = 2, exogen = data[1:(nrow(data)-i),7:8])
#   var_for <- predict(var, n.ahead=1, ci=0.95, dumvar = t(data[(nrow(data)-i)+1,7:8]))
#   M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
# }
# 
# previsao <- ts(M, end = end(data), freq = 12)
# d <- na.omit(cbind(previsao,data[,"PIM"]))
# ts.plot(d, col = 2:1)
# MAPE(d[,2],d[,1])
# 
# # read data and ts 
# data1 <- na.omit(readRDS("00.data/data_estrutural.rds"))
# data <- window(data1, start = c(2002,1), end = c(2017,7), freq = 12)
# plot(data)
# 
# dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
# dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
# dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2009,1))
# dummies <- cbind(dummy1, dummy2, dummy3)
# 
# 
# # variables and order
# ord <- c("PROD_PREV","ICI","ISA","IE","IIEBR","PIM")
# data <- na.omit(cbind(data[,ord], dummies))
# colnames(data) <- c(ord, "dummy1", "dummy2", "dummy3")
# plot(data)
# 
# # var estimation  
# VARselect(data[,c(1,5,6)], lag.max = 6, exogen = data[,7:8])
# var <- VAR(data[,c(1,5,6)], lag.max = 2, exogen = data[,7:8])
# summary(var)
# 
# VARselect(data[,c(2,5,6)], lag.max = 6, exogen = data[,7:8])
# var <- VAR(data[,c(2,5,6)], lag.max = 2, exogen = data[,7:8])
# summary(var)
# 
# VARselect(data[,c(3,4,5,6)], lag.max = 6, exogen = data[,7:8])
# var <- VAR(data[,c(3,4,5,6)], lag.max = 2, exogen = data[,7:8])
# summary(var)
# 
# VARselect(data[,c(3,4,6)], lag.max = 6, exogen = data[,7:8])
# var <- VAR(data[,c(3,4,6)], lag.max = 2, exogen = data[,7:8])
# summary(var)
# 
# VARselect(data[,c(2,6)], lag.max = 6, exogen = data[,7:8])
# var <- VAR(data[,c(2,6)], lag.max = 2, exogen = data[,7:8])
# summary(var)
# 
# VARselect(data[,c(1,6)], lag.max = 6, exogen = data[,7:8])
# var <- VAR(data[,c(1,6)], lag.max = 2, exogen = data[,7:8])
# summary(var)
# 
# # diagnost tests  
# serial.test(var)  # autocorrelação
# arch.test(var) # heterocedasticidade
# roots(var)
# residuos <- ts(resid(var), end = end(data), freq = 12)
# plot(residuos)
# resid_padr <- (residuos[,"PIM"] - mean(residuos[,"PIM"]))/sd(residuos[,"PIM"]) 
# plot(resid_padr)
# abline(h = c(-3,3), col = 2, lty = 2)
# 
# 
# # forecast  
# h = 24
# M <- matrix(NA, ncol = 1, nrow = h)
# for(i in h:1){
#   var <- VAR(data[1:(nrow(data)-i),c(1,5,6)], p = 2, exogen = data[1:(nrow(data)-i),7:8])
#   var_for <- predict(var, n.ahead=1, ci=0.95, dumvar = t(data[(nrow(data)-i)+1,7:8]))
#   M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
# }
# 
# previsao <- ts(M, end = end(data), freq = 12)
# d <- na.omit(cbind(previsao,data[,"PIM"]))
# ts.plot(d, col = 2:1)
# MAPE(d[,2],d[,1])
# 
# saveRDS(previsao, "02.var/previsao_VAR.rds")




# VARselect(data_var[,c(1:11)], lag.max = 6, exogen = data_var[,12:13])
# var <- VAR(data_var[,c(1:11)], p = 1, exogen = data_var[,12:13])
# summary(var)
# 
# VARselect(data_var[,c(1:4,6,8:11)], lag.max = 6, exogen = data_var[,12:13])
# var <- VAR(data_var[,c(1:4,6,8:11)], p = 1, exogen = data_var[,12:13])
# summary(var)
# 
# VARselect(diff(data_var[,c(1:4,9,11)]), lag.max = 6, exogen = data_var[-1,12:13])
# var <- VAR(data_var[,c(1:4,9,11)], p = 3, exogen = data_var[,12:13])
# summary(var)
# 
# 
# VARselect(data_var[,c(1:3,11)], lag.max = 6, exogen = data_var[,12:13])
# var <- VAR(data_var[,c(1:3,11)], p = 3, exogen = data_var[,12:13])
# summary(var)
# 
# 
# VARselect(diff(log(data_var[,c(1,3:4,9,11)])), lag.max = 6, exogen = data_var[-1,12:13])
# var <- VAR(data_var[,c(1,3:4,9,11)], p = 2, exogen = data_var[,12:13])
# summary(var)
# 
# VARselect(diff(log(data_var[,c(1,3:4,8,9,11)])), lag.max = 6, exogen = data_var[-1,12:13])
# var <- VAR(diff(log(data_var[,c(1,3:4,8,9,11)])), p = 2, exogen = data_var[-1,12:13])
# summary(var)
# 
# # diagnost tests 
# serial.test(var)  # autocorrelação
# arch.test(var) # heterocedasticidade
# roots(var)
# residuos <- ts(resid(var), end = end(data), freq = 12)
# plot(residuos)
# resid_padr <- (residuos[,"PIM"] - mean(residuos[,"PIM"]))/sd(residuos[,"PIM"]) 
# plot(resid_padr)
# abline(h = c(-3,3), col = 2, lty = 2)
# 
# 
# # forecast  
# data_var2 <- diff(log(data_var))
# data_var2[,12:14] <- data_var[-1,12:14]
# 
# h = 24
# M <- matrix(NA, ncol = 1, nrow = h)
# for(i in h:1){
#   var <- VAR(data_var2[1:(nrow(data_var2)-i),c(1,3:4,8,9,11)], p = 2, exogen = data_var2[1:(nrow(data_var2)-i),12:13])
#   var_for <- predict(var, n.ahead=1, ci=0.95, dumvar = t(data_var2[(nrow(data_var2)-i)+1,12:13]))
#   M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
# }
# 
# previsao <- ts(M, end = end(data_var2), freq = 12)
# d <- round(na.omit(cbind(previsao,data_var2[,"PIM"])),4)
# ts.plot(d, col = 2:1)
# MAPE(100*(d[,2]+1),100*(d[,1]+1))
# 
# #saveRDS(previsao, "02.var/previsao_VAR.rds")
# 
# 
# # VEC
# VARselect(diff(data_var[,c(1,3,4,8,9,11)]), lag.max = 6, exogen = data_var[-1,12:13])
# teste <- ca.jo(data_var[,c(1,3,4,8,9,11)], dumvar = data_var[,12:13], type = "eigen", K = 2, ecdet = "const")
# summary(teste)
# summary(cajorls(teste, r = 1)$rlm)
