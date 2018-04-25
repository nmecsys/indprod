# diretório + funções ----------------------------------------------
setwd("C:/Users/daiane.mattos/Dropbox/08 Previsão da PIM-PF")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(vars, urca, tseries, forecast, readxl, xtable, seasonal, BETS, dygraphs, lubridate)

RMSE <- function(y,x){ 
  data <- na.omit(cbind(y,x))
  sqrt(mean((data[,1] - data[,2])^2))
}

MAPE <- function(y,x){ 
  data <- na.omit(cbind(y,x))
  mean(abs((data[,1] - data[,2])/data[,1]))*100
}

sMAPE <- function(y,yhat){ 
  data <- na.omit(data.frame(cbind(y,yhat)))
  num <- sum(abs(data[,1] - data[,2]))
  dem <- sum(abs(data[,1] + data[,2])[-1])
  (1*nrow(data))*num/dem
}

# read data and ts -------------------------------------------------
data <- window(readRDS("00.data/data_multivariado_var.rds"), start = c(2004,1), freq = 12)[,c(1:5,8)]
plot(data)

dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2012,1))
dummy4 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2016,6))
dummy5 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2009,1))
dummy6 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,10))
dummies <- cbind(dummy1, dummy2, dummy3, dummy4, dummy5, dummy6)

pim <- data[,"PIM"]
pim_inter <- (pim/lag(pim,-12) - 1)*100

# mts de variáveis que podem entrar no var
data_var <- na.omit(cbind(data, dummies))
colnames(data_var) <- c(colnames(data), "dummy1", "dummy2", "dummy3","dummy4","dummy5","dummy6")
dim(data_var)
plot(data_var[,1:6])

# análise de correlação --------------------------------

# análise das variáveis com mais correlação no tempo t
mcor <- cor(data_var[,1:6])
corrplot::corrplot(mcor)
mcor[6,]

# análise das variáveis com mais correlação no tempo t-1
data_vart1 <- lag(data_var, -1)
data_vart1[,c(6:12)] <- rbind(data_var[-1,c(6:12)],NA)
mcor1 <- cor(data_vart1[,1:6], use = "complete.obs")
corrplot::corrplot(mcor1)
mcor1[6,]

# análise das variáveis com mais correlação no tempo t-2
data_vart2 <- lag(data_var, -2)
data_vart2[,c(6:12)] <- rbind(data_var[-c(1:2),c(6:12)],NA,NA)
mcor2 <- cor(data_vart2[,1:6], use = "complete.obs")
corrplot::corrplot(mcor2)
mcor2[6,]

# análise das variáveis com mais correlação no tempo t-3
data_vart3 <- lag(data_var, -3)
data_vart3[,c(6:12)] <- rbind(data_var[-c(1:3),c(6:12)],NA,NA,NA)
mcor3 <- cor(data_vart3[,1:6], use = "complete.obs")
corrplot::corrplot(mcor3)
mcor3[6,]

# correlação forte com as 5 variáveis, t com t-1 é mais forte exceto para ICI

# estacionarizar variáveis - via variação mensal
data_var_taxa <- (data_var[,1:6]/lag(data_var[,1:6],-1) - 1)*100
data_var_taxa <- cbind(data_var_taxa, dummies)
colnames(data_var_taxa) <- c(colnames(data_var)[1:6], "dummy1", "dummy2", "dummy3","dummy4","dummy5","dummy6")

data_var_taxat1 <- (data_vart1[,1:6]/lag(data_vart1[,1:6],-1) - 1)*100
colnames(data_var_taxat1) <- colnames(data_vart1)[1:6]

data_var_taxat2 <- (data_vart2[,1:6]/lag(data_vart2[,1:6],-1) - 1)*100
colnames(data_var_taxat2) <- colnames(data_vart2)[1:6]

data_var_taxat3 <- (data_vart3[,1:6]/lag(data_vart3[,1:6],-1) - 1)*100
colnames(data_var_taxat3) <- colnames(data_vart3)[1:6]

# análise das variáveis taxa com mais correlação no tempo t
mcor <- cor(data_var_taxa[,1:6], use = "complete.obs")
corrplot::corrplot(mcor)
mcor[6,]

# análise das variáveis taxa com mais correlação no tempo t-1
mcor1 <- cor(data_var_taxat1, use = "complete.obs")
corrplot::corrplot(mcor1)
mcor1[6,]

# análise das variáveis taxa com mais correlação no tempo t-2
mcor2 <- cor(data_var_taxat2, use = "complete.obs")
corrplot::corrplot(mcor2)
mcor2[6,]

# análise das variáveis taxa com mais correlação no tempo t-3
mcor3 <- cor(data_var_taxat3, use = "complete.obs")
corrplot::corrplot(mcor3)
mcor3[6,]

summary(lm(PIM ~ ., data = data_var)) 
summary(lm(PIM ~ ., data = data_vart1))
summary(lm(PIM ~ ., data = data_vart2))
summary(lm(PIM ~ ., data = data_vart3))

summary(lm(PIM ~ ., data = data_var_taxa)) 
summary(lm(PIM ~ ., data = data_var_taxat1))
summary(lm(PIM ~ ., data = data_var_taxat2))
summary(lm(PIM ~ ., data = data_var_taxat3))

# em variação, nada é significativo praticamente

# teste de cointegração -------------------------------------------

# MODELO BOM
VARselect(log(data_var[,c(1:6)]), lag.max = 6, exogen = data_var[,7:8])
teste <- ca.jo(log(data_var[,c(1:6)]), type = "eigen", K = 5, ecdet = "const", dumvar = data_var[,7:8]) 
summary(teste)
# 2 relações de cointegração
summary(cajorls(teste, r = 2)$rlm)
var2 <- vec2var(teste, r = 2)

# # MODELO NOVO
# VARselect(log(data_var[,c(1,2,7)]), lag.max = 6, exogen = data_var[,8:9])
# teste <- ca.jo(log(data_var[,c(1,2,7)]), type = "eigen", K = 5, ecdet = "const", dumvar = data_var[,8:9]) 
# summary(teste)
# # 2 relações de cointegração
# summary(cajorls(teste, r = 1)$rlm)
# var2 <- vec2var(teste, r = 1)


# diagnost tests  -------------------------------------------------
serial.test(var2)  # autocorrelação
arch.test(var2) # heterocedasticidade
residuos <- ts(resid(var2), end = end(data_var), freq = 12)
plot(residuos)
dygraph(residuos[,1])
fits <- ts(fitted(var2), end = end(data_var), freq = 12)
fit_pim <- cbind(pim, exp(fits[,"fit of PIM"]))
MAPE(fit_pim[,1], fit_pim[,2])
sMAPE(fit_pim[,1], fit_pim[,2])

ts.plot(pim,exp(fits[,"fit of PIM"]), col = 1:2)

resid_padr <- (residuos[,"resids of PIM"] - mean(residuos[,"resids of PIM"]))/sd(residuos[,"resids of PIM"]) 
plot(resid_padr)
abline(h = c(-3,3), col = 2, lty = 2)

# forecast  -------------------------------------------------------
h = 24
M <- matrix(NA, ncol = 1, nrow = h)
for(i in h:1){
  teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c(1:6)]), dumvar = data_var[1:(nrow(data_var)-i),c(7:8)], type = "eigen", K = 5, ecdet = "const")
  var2 <- vec2var(teste, r = 2)
  var_for <- predict(var2, n.ahead=1, ci=0.95, dumvar = t(data_var[(nrow(data_var)-i)+1,c(7:8)]))
  M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
}
previsao_var2_nivel <- ts(exp(M), end = end(data_var), freq = 12)
ts.plot(pim, previsao_var2_nivel, col = 1:2)

prevs_inter <- data.frame(data = as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 12)))
prevs_inter$inter_prev <- (prevs_inter[,3]/prevs_inter[,2]-1)*100
prev_inter <- ts(na.omit(prevs_inter$inter_prev), end = end(previsao_var2_nivel), freq = 12)
ts.plot(prev_inter,pim_inter, col = 2:1)
sqrt(mean((pim_inter - prev_inter)^2)) # RMSE
MAPE(pim_inter, prev_inter)
sMAPE(pim_inter, prev_inter)

prevs_margem <- data.frame(as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 1)))
prevs_margem$margem_prev <- (prevs_margem[,3]/prevs_margem[,2]-1)*100
prev_margem <- ts(na.omit(prevs_margem$margem), end = end(previsao_var2_nivel), freq = 12)
pim_margem <- (pim/lag(pim,-1) - 1)*100
ts.plot(round(cbind(prev_margem,pim_margem),2), col = 2:1)
sqrt(mean((pim_margem - prev_margem)^2)) # RMSE
MAPE(pim_margem, prev_margem)
sMAPE(pim_margem, prev_margem)

# previsão fora da amostra --------------------------------------------------------------------

teste <- ca.jo(log(data_var[,c(1:6)]), type = "eigen", K = 5, ecdet = "const", dumvar = data_var[,7:8]) 
var2 <- vec2var(teste, r = 2)
d <- matrix(rep(c(0,0),5), ncol = 2)
colnames(d) <- c("dummy1","dummy2")

data_pim <- tail(as.Date(pim),1) + months(1)
prev <- ts(exp(predict(var2, n.ahead = nrow(d), dumvar = d)$fcst$PIM[,1]),
           start = as.numeric(c(substr(data_pim,1,4),substr(data_pim,6,7))), freq = 12)
ts.plot(pim,prev)




# NOVOS DADOS ----------------------------------------------------------------------------------
# read data and ts -------------------------------------------------
data <- window(readRDS("00.data/data_multivariado_var.rds"), start = c(2004,1), freq = 12)[,c(1:5,7:8)]
plot(data)

dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2012,1))
dummy4 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2016,6))
dummy5 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2009,1))
dummy6 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,10))
dummies <- cbind(dummy1, dummy2, dummy3, dummy4, dummy5, dummy6)

pim <- data[,"PIM"]
pim_inter <- (pim/lag(pim,-12) - 1)*100

# mts de variáveis que podem entrar no var
data_var <- na.omit(cbind(data, dummies))
colnames(data_var) <- c(colnames(data), "dummy1", "dummy2", "dummy3","dummy4","dummy5","dummy6")
dim(data_var)
plot(data_var[,1:7])

# análise de correlação --------------------------------

# análise das variáveis com mais correlação no tempo t
mcor <- cor(data_var[,1:7])
corrplot::corrplot(mcor)
mcor[7,]

# análise das variáveis com mais correlação no tempo t-1
data_vart1 <- lag(data_var, -1)
data_vart1[,c(7:13)] <- rbind(data_var[-1,c(7:13)],NA)
mcor1 <- cor(data_vart1[,1:7], use = "complete.obs")
corrplot::corrplot(mcor1)
mcor1[7,]

# análise das variáveis com mais correlação no tempo t-2
data_vart2 <- lag(data_var, -2)
data_vart2[,c(7:13)] <- rbind(data_var[-c(1:2),c(7:13)],NA,NA)
mcor2 <- cor(data_vart2[,1:7], use = "complete.obs")
corrplot::corrplot(mcor2)
mcor2[7,]

# análise das variáveis com mais correlação no tempo t-3
data_vart3 <- lag(data_var, -3)
data_vart3[,c(7:13)] <- rbind(data_var[-c(1:3),c(7:13)],NA,NA,NA)
mcor3 <- cor(data_vart3[,1:7], use = "complete.obs")
corrplot::corrplot(mcor3)
mcor3[7,]

# correlação forte com as variáveis, exceto ABPO

# estacionarizar variáveis - via variação mensal
data_var_taxa <- (data_var[,1:7]/lag(data_var[,1:7],-1) - 1)*100
data_var_taxa <- cbind(data_var_taxa, dummies)
colnames(data_var_taxa) <- c(colnames(data_var)[1:7], "dummy1", "dummy2", "dummy3","dummy4","dummy5","dummy6")

data_var_taxat1 <- (data_vart1[,1:7]/lag(data_vart1[,1:7],-1) - 1)*100
colnames(data_var_taxat1) <- colnames(data_vart1)[1:7]

data_var_taxat2 <- (data_vart2[,1:7]/lag(data_vart2[,1:7],-1) - 1)*100
colnames(data_var_taxat2) <- colnames(data_vart2)[1:7]

data_var_taxat3 <- (data_vart3[,1:7]/lag(data_vart3[,1:7],-1) - 1)*100
colnames(data_var_taxat3) <- colnames(data_vart3)[1:7]

# análise das variáveis taxa com mais correlação no tempo t
mcor <- cor(data_var_taxa[,1:7], use = "complete.obs")
corrplot::corrplot(mcor)
mcor[7,]

# análise das variáveis taxa com mais correlação no tempo t-1
mcor1 <- cor(data_var_taxat1, use = "complete.obs")
corrplot::corrplot(mcor1)
mcor1[7,]

# análise das variáveis taxa com mais correlação no tempo t-2
mcor2 <- cor(data_var_taxat2, use = "complete.obs")
corrplot::corrplot(mcor2)
mcor2[7,]

# análise das variáveis taxa com mais correlação no tempo t-3
mcor3 <- cor(data_var_taxat3, use = "complete.obs")
corrplot::corrplot(mcor3)
mcor3[7,]

summary(lm(PIM ~ ., data = data_var)) 
summary(lm(PIM ~ ., data = data_vart1))
summary(lm(PIM ~ ., data = data_vart2))
summary(lm(PIM ~ ., data = data_vart3))

summary(lm(PIM ~ ., data = data_var_taxa)) 
summary(lm(PIM ~ ., data = data_var_taxat1))
summary(lm(PIM ~ ., data = data_var_taxat2))
summary(lm(PIM ~ ., data = data_var_taxat3))

# em variação, nada é significativo praticamente

# teste de cointegração -------------------------------------------

# MODELO BOM 1: ICI + ANFAVEA + ABCR + FUNCEX + ABPO + IPI
VARselect(log(data_var[,c(1:7)]), lag.max = 6, exogen = data_var[,8:9])
teste <- ca.jo(log(data_var[,c(1:7)]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[,8:9]) 
summary(teste)
# 2 relações de cointegração
summary(cajorls(teste, r = 2)$rlm)
var2 <- vec2var(teste, r = 2)

# MODELO BOM 2: ICI + ABCR + FUNCEX + ABPO + IPI
VARselect(log(data_var[,c(1,3:7)]), lag.max = 6, exogen = data_var[,8:9])
teste <- ca.jo(log(data_var[,c(1,3:7)]), type = "eigen", K =5, ecdet = "const", dumvar = data_var[,8:9]) 
summary(teste)
# 2 relações de cointegração
summary(cajorls(teste, r = 2)$rlm)
var2 <- vec2var(teste, r = 2)


# diagnost tests  -------------------------------------------------
serial.test(var2)  # autocorrelação
arch.test(var2) # heterocedasticidade
residuos <- ts(resid(var2), end = end(data_var), freq = 12)
plot(residuos)
dygraph(residuos[,"resids of PIM"])
fits <- ts(fitted(var2), end = end(data_var), freq = 12)
fit_pim <- cbind(pim, exp(fits[,"fit of PIM"]))
MAPE(fit_pim[,1], fit_pim[,2])
sMAPE(fit_pim[,1], fit_pim[,2])

ts.plot(pim,exp(fits[,"fit of PIM"]), col = 1:2)

resid_padr <- (residuos[,"resids of PIM"] - mean(residuos[,"resids of PIM"]))/sd(residuos[,"resids of PIM"]) 
plot(resid_padr)
abline(h = c(-3,3), col = 2, lty = 2)

# forecast  -------------------------------------------------------
h = 24
M <- matrix(NA, ncol = 1, nrow = h)
for(i in h:1){
  teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c(1,3:7)]), dumvar = data_var[1:(nrow(data_var)-i),c(8:9)], type = "eigen", K = 5, ecdet = "const")
  #teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c(1:7)]), type = "eigen", K = 3, ecdet = "const")
  var2 <- vec2var(teste, r = 2)
  var_for <- predict(var2, n.ahead=1, ci=0.95, dumvar =  t(data_var[(nrow(data_var)-i)+1,c(8:9)]))
  M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
}
previsao_var2_nivel <- ts(exp(M), end = end(data_var), freq = 12)
ts.plot(pim, previsao_var2_nivel, col = 1:2)

prevs_inter <- data.frame(data = as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 12)))
prevs_inter$inter_prev <- (prevs_inter[,3]/prevs_inter[,2]-1)*100
prev_inter <- ts(na.omit(prevs_inter$inter_prev), end = end(previsao_var2_nivel), freq = 12)
ts.plot(prev_inter,pim_inter, col = 2:1)
sqrt(mean((pim_inter - prev_inter)^2)) # RMSE
MAPE(pim_inter, prev_inter)
sMAPE(pim_inter, prev_inter)

prevs_margem <- data.frame(as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 1)))
prevs_margem$margem_prev <- (prevs_margem[,3]/prevs_margem[,2]-1)*100
prev_margem <- ts(na.omit(prevs_margem$margem), end = end(previsao_var2_nivel), freq = 12)
pim_margem <- (pim/lag(pim,-1) - 1)*100
ts.plot(round(cbind(prev_margem,pim_margem),2), col = 2:1)
sqrt(mean((pim_margem - prev_margem)^2)) # RMSE
MAPE(pim_margem, prev_margem)
sMAPE(pim_margem, prev_margem)

# previsão fora da amostra --------------------------------------------------------------------

teste <- ca.jo(log(data_var[,c(1,3:7)]), type = "eigen", K = 5, ecdet = "const", dumvar = data_var[,8:9]) 
var2 <- vec2var(teste, r = 2)
d <- matrix(rep(c(0,0),5), ncol = 2)
colnames(d) <- c("dummy1","dummy2")

data_pim <- tail(as.Date(na.omit(pim)),1) + months(1)
prev <- ts(exp(predict(var2, n.ahead = nrow(d), dumvar = d)$fcst$PIM[,1]),
           start = as.numeric(c(substr(data_pim,1,4),substr(data_pim,6,7))), freq = 12)
ts.plot(pim,prev)
prev
(prev[1]/tail(na.omit(pim),1)-1)*100
(prev[1]/tail(na.omit(pim),12)[1]-1)*100




# NOVOS DADOS 2----------------------------------------------------------------------------------
# read data and ts -------------------------------------------------
data <- window(readRDS("00.data/data_multivariado_var.rds"), start = c(2004,1), end = c(2017,7), freq = 12)[,-6]#[,c(1:5,7:8)]
plot(data)

dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2012,1))
dummy4 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2016,6))
dummy5 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2009,1))
dummy6 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,10))
dummies <- cbind(dummy1, dummy2)#, dummy3, dummy4, dummy5, dummy6)

pim <- data[,"PIM"]
pim_inter <- (pim/lag(pim,-12) - 1)*100

# mts de variáveis que podem entrar no var
data_var <- na.omit(cbind(data, dummies[,1:2]))
colnames(data_var) <- c(colnames(data), "dummy1", "dummy2")
dim(data_var)
plot(data_var[,1:10])

# análise de correlação --------------------------------

# análise das variáveis com mais correlação no tempo t
mcor <- cor(data_var[,1:11])
corrplot::corrplot(mcor)
mcor[11,]

# análise das variáveis com mais correlação no tempo t-1
data_vart1 <- lag(data_var, -1)
data_vart1[,c(11:13)] <- rbind(data_var[-1,c(11:13)],NA)
mcor1 <- cor(data_vart1[,1:11], use = "complete.obs")
corrplot::corrplot(mcor1)
mcor1[11,]

# análise das variáveis com mais correlação no tempo t-2
data_vart2 <- lag(data_var, -2)
data_vart2[,c(11:13)] <- rbind(data_var[-c(1:2),c(11:13)],NA,NA)
mcor2 <- cor(data_vart2[,1:11], use = "complete.obs")
corrplot::corrplot(mcor2)
mcor2[11,]

# análise das variáveis com mais correlação no tempo t-3
data_vart3 <- lag(data_var, -3)
data_vart3[,c(11:13)] <- rbind(data_var[-c(1:3),c(11:13)],NA,NA,NA)
mcor3 <- cor(data_vart3[,1:11], use = "complete.obs")
corrplot::corrplot(mcor3)
mcor3[11,]

# correlação forte com as variáveis, exceto ABPO

# estacionarizar variáveis - via variação mensal
data_var_taxa <- (data_var[,1:11]/lag(data_var[,1:11],-1) - 1)*100
data_var_taxa <- cbind(data_var_taxa, dummies)
colnames(data_var_taxa) <- c(colnames(data_var)[1:11], "dummy1", "dummy2")

data_var_taxat1 <- (data_vart1[,1:11]/lag(data_vart1[,1:11],-1) - 1)*100
colnames(data_var_taxat1) <- colnames(data_vart1)[1:11]

data_var_taxat2 <- (data_vart2[,1:11]/lag(data_vart2[,1:11],-1) - 1)*100
colnames(data_var_taxat2) <- colnames(data_vart2)[1:11]

data_var_taxat3 <- (data_vart3[,1:11]/lag(data_vart3[,1:11],-1) - 1)*100
colnames(data_var_taxat3) <- colnames(data_vart3)[1:11]

# análise das variáveis taxa com mais correlação no tempo t
mcor <- cor(data_var_taxa[,1:11], use = "complete.obs")
corrplot::corrplot(mcor)
mcor[11,]

# análise das variáveis taxa com mais correlação no tempo t-1
mcor1 <- cor(data_var_taxat1, use = "complete.obs")
corrplot::corrplot(mcor1)
mcor1[11,]

# análise das variáveis taxa com mais correlação no tempo t-2
mcor2 <- cor(data_var_taxat2, use = "complete.obs")
corrplot::corrplot(mcor2)
mcor2[11,]

# análise das variáveis taxa com mais correlação no tempo t-3
mcor3 <- cor(data_var_taxat3, use = "complete.obs")
corrplot::corrplot(mcor3)
mcor3[11,]

summary(lm(PIM ~ ., data = data_var)) 
summary(lm(PIM ~ ., data = data_vart1))
summary(lm(PIM ~ ., data = data_vart2))
summary(lm(PIM ~ ., data = data_vart3))

summary(lm(PIM ~ ., data = data_var_taxa)) 
summary(lm(PIM ~ ., data = data_var_taxat1))
summary(lm(PIM ~ ., data = data_var_taxat2))
summary(lm(PIM ~ ., data = data_var_taxat3))

# em variação, nada é significativo praticamente

# teste de cointegração -------------------------------------------

# MODELO BOM: ICI + ABCR FUNCEX ABPO IPI HT-CNI NUCI-CNI INAc(1,3:6,8,9,10,11)
VARselect(log(data_var[,c("ICI","ABCR", "FUNCEX", "ABPO", "IPI", "HT-CNI", "NUCI-CNI", "INA","PIM")]), lag.max = 6, exogen = data_var[,c("dummy1","dummy2")])
teste <- ca.jo(log(data_var[,c("ICI","ABCR", "FUNCEX", "ABPO", "IPI", "HT-CNI", "NUCI-CNI", "INA","PIM")]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[,c("dummy1","dummy2")]) 
summary(teste)
# 2 relações de cointegração
summary(cajorls(teste, r = 2)$rlm)
var2 <- vec2var(teste, r = 2)



# diagnost tests  -------------------------------------------------
serial.test(var2)  # autocorrelação
arch.test(var2) # heterocedasticidade
residuos <- ts(resid(var2), end = end(data_var), freq = 12)
plot(residuos)
dygraph(residuos[,"resids of PIM"])
fits <- ts(fitted(var2), end = end(data_var), freq = 12)
fit_pim <- cbind(pim, exp(fits[,"fit of PIM"]))
MAPE(fit_pim[,1], fit_pim[,2])
sMAPE(fit_pim[,1], fit_pim[,2])

ts.plot(pim,exp(fits[,"fit of PIM"]), col = 1:2)

resid_padr <- (residuos[,"resids of PIM"] - mean(residuos[,"resids of PIM"]))/sd(residuos[,"resids of PIM"]) 
plot(resid_padr)
abline(h = c(-3,3), col = 2, lty = 2)

# forecast  -------------------------------------------------------
h = 24
M <- matrix(NA, ncol = 1, nrow = h)
for(i in h:1){
  teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c("ICI","ABCR", "FUNCEX", "ABPO", "IPI", "HT-CNI", "NUCI-CNI", "INA","PIM")]), dumvar = data_var[1:(nrow(data_var)-i),c("dummy1","dummy2")], type = "eigen", K = 3, ecdet = "const")
  #teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c(1:7)]), type = "eigen", K = 3, ecdet = "const")
  var2 <- vec2var(teste, r = 2)
  var_for <- predict(var2, n.ahead=1, ci=0.95, dumvar =  t(data_var[(nrow(data_var)-i)+1,c("dummy1","dummy2")]))
  M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
}
previsao_var2_nivel <- ts(exp(M), end = end(data_var), freq = 12)
ts.plot(pim, previsao_var2_nivel, col = 1:2)

prevs_inter <- data.frame(data = as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 12)))
prevs_inter$inter_prev <- (prevs_inter[,3]/prevs_inter[,2]-1)*100
prev_inter <- ts(na.omit(prevs_inter$inter_prev), end = end(previsao_var2_nivel), freq = 12)
ts.plot(prev_inter,pim_inter, col = 2:1)
sqrt(mean((pim_inter - prev_inter)^2)) # RMSE
MAPE(pim_inter, prev_inter)
sMAPE(pim_inter, prev_inter)

prevs_margem <- data.frame(as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 1)))
prevs_margem$margem_prev <- (prevs_margem[,3]/prevs_margem[,2]-1)*100
prev_margem <- ts(na.omit(prevs_margem$margem), end = end(previsao_var2_nivel), freq = 12)
pim_margem <- (pim/lag(pim,-1) - 1)*100
ts.plot(round(cbind(prev_margem,pim_margem),2), col = 2:1)
sqrt(mean((pim_margem - prev_margem)^2)) # RMSE
MAPE(pim_margem, prev_margem)
sMAPE(pim_margem, prev_margem)

# previsão fora da amostra --------------------------------------------------------------------

teste <- ca.jo(log(data_var[,c(1,3:6,8,9,10,11)]), type = "eigen", K = 5, ecdet = "const", dumvar = data_var[,12:13]) 
var2 <- vec2var(teste, r = 2)
d <- matrix(rep(c(0,0),5), ncol = 2)
colnames(d) <- c("dummy1","dummy2")

data_pim <- tail(as.Date(na.omit(pim)),1) + months(1)
prev <- ts(exp(predict(var2, n.ahead = nrow(d), dumvar = d)$fcst$PIM[,1]),
           start = as.numeric(c(substr(data_pim,1,4),substr(data_pim,6,7))), freq = 12)
ts.plot(pim,prev)
prev
(prev[1]/tail(na.omit(pim),1)-1)*100
(prev[1]/tail(na.omit(pim),12)[1]-1)*100


# NOVOS DADOS 3----------------------------------------------------------------------------------
# read data and ts -------------------------------------------------
data <- window(readRDS("00.data/data_multivariado_var2.rds"), start = c(2004,1), end = c(2017,7), freq = 12)#[,c(1:5,7:8)]
plot(data)

dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2012,1))
dummy4 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2016,6))
dummy5 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2009,1))
dummy6 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,10))
dummies <- cbind(dummy1, dummy2, dummy3, dummy4, dummy5, dummy6)

pim <- data[,"PIM"]
pim_inter <- (pim/lag(pim,-12) - 1)*100

# mts de variáveis que podem entrar no var
data_var <- na.omit(cbind(data, dummies))
colnames(data_var) <- c(colnames(data), "dummy1", "dummy2","dummy3","dummy4","dummy5","dummy6")
dim(data_var)
plot(data_var[,1:10])

# análise de correlação --------------------------------

# análise das variáveis com mais correlação no tempo t
mcor <- cor(data_var[,1:ncol(data)])
corrplot::corrplot(mcor)
mcor[ncol(data),]

# análise das variáveis com mais correlação no tempo t-1
data_vart1 <- lag(data_var, -1)[,1:ncol(data)]
data_vart1[,ncol(data)] <- c(data_var[-1,ncol(data)],NA)
mcor1 <- cor(data_vart1, use = "complete.obs")
corrplot::corrplot(mcor1)
mcor1[ncol(data),]

# análise das variáveis com mais correlação no tempo t-2
data_vart2 <- lag(data_var, -2)[,1:ncol(data)]
data_vart2[,ncol(data)] <- c(data_var[-c(1:2),ncol(data)],NA,NA)
mcor2 <- cor(data_vart2, use = "complete.obs")
corrplot::corrplot(mcor2)
mcor2[ncol(data),]

# análise das variáveis com mais correlação no tempo t-3
data_vart3 <- lag(data_var, -3)[,1:ncol(data)]
data_vart3[,ncol(data)] <- c(data_var[-c(1:3),ncol(data)],NA,NA,NA)
mcor3 <- cor(data_vart3, use = "complete.obs")
corrplot::corrplot(mcor3)
mcor3[ncol(data),]

corr <- rbind(mcor1[ncol(data),],mcor2[ncol(data),],mcor3[ncol(data),])
corr
# variáveis com correlação > 0.50
varname60 <- names(which(colSums(abs(corr) > 0.5) > 0))
varname60
length(varname60)


# estacionarizar variáveis - via variação mensal
data_var_taxa <- (data_var[,1:ncol(data)]/lag(data_var[,1:ncol(data)],-1) - 1)*100
colnames(data_var_taxa) <- colnames(data_var)[1:ncol(data)]

data_var_taxat1 <- (data_vart1[,1:ncol(data)]/lag(data_vart1[,1:ncol(data)],-1) - 1)*100
colnames(data_var_taxat1) <- colnames(data_vart1)[1:ncol(data)]

data_var_taxat2 <- (data_vart2[,1:ncol(data)]/lag(data_vart2[,1:ncol(data)],-1) - 1)*100
colnames(data_var_taxat2) <- colnames(data_vart2)[1:ncol(data)]

data_var_taxat3 <- (data_vart3[,1:ncol(data)]/lag(data_vart3[,1:ncol(data)],-1) - 1)*100
colnames(data_var_taxat3) <- colnames(data_vart3)[1:ncol(data)]

# análise das variáveis taxa com mais correlação no tempo t
mcor <- cor(data_var_taxa, use = "complete.obs")
corrplot::corrplot(mcor)
mcor[ncol(data),]

# análise das variáveis taxa com mais correlação no tempo t-1
mcor1 <- cor(data_var_taxat1, use = "complete.obs")
corrplot::corrplot(mcor1)
mcor1[ncol(data),]

# análise das variáveis taxa com mais correlação no tempo t-2
mcor2 <- cor(data_var_taxat2, use = "complete.obs")
corrplot::corrplot(mcor2)
mcor2[ncol(data),]

# análise das variáveis taxa com mais correlação no tempo t-3
mcor3 <- cor(data_var_taxat3, use = "complete.obs")
corrplot::corrplot(mcor3)
mcor3[ncol(data),]

corrtaxa <- rbind(mcor1[ncol(data),],mcor2[ncol(data),],mcor3[ncol(data),])
corrtaxa
# variáveis com correlação > 0.50
vartaxaname60 <- names(which(colSums(abs(corrtaxa) > 0.5) > 0))
vartaxaname60
length(vartaxaname60)

summary(lm(PIM ~ ., data = data_var)) 
summary(lm(PIM ~ ., data = data_vart1))
summary(lm(PIM ~ ., data = data_vart2))
summary(lm(PIM ~ ., data = data_vart3))

summary(lm(PIM ~ ., data = data_var_taxa)) 
summary(lm(PIM ~ ., data = data_var_taxat1))
summary(lm(PIM ~ ., data = data_var_taxat2))
summary(lm(PIM ~ ., data = data_var_taxat3))

m_nivel1 <- summary(lm(PIM ~ ., data = data_var))$coefficients[,"Pr(>|t|)"] <= 0.05
m_nivel1 <- names(m_nivel1[m_nivel1])
m_nivel2 <- summary(lm(PIM ~ ., data = data_vart1))$coefficients[,"Pr(>|t|)"] <= 0.05
m_nivel2 <- names(m_nivel2[m_nivel2])
m_nivel3 <- summary(lm(PIM ~ ., data = data_vart2))$coefficients[,"Pr(>|t|)"] <= 0.05
m_nivel3 <- names(m_nivel3[m_nivel3])
m_nivel4 <- summary(lm(PIM ~ ., data = data_vart3))$coefficients[,"Pr(>|t|)"] <= 0.05
m_nivel4 <- names(m_nivel4[m_nivel4])

m_nivel1 # t
m_nivel2 # t-1
m_nivel3 # t-2
m_nivel4 # t-3
nivel_sign <- unique(c(m_nivel1,m_nivel2,m_nivel3,m_nivel4))
nivel_sign <- gsub("`","", nivel_sign)[-11]


m_taxa1 <- summary(lm(PIM ~ ., data = data_var_taxa))$coefficients[,"Pr(>|t|)"] <= 0.05
m_taxa1 <- names(m_taxa1[m_taxa1])
m_taxa2 <- summary(lm(PIM ~ ., data = data_var_taxat1))$coefficients[,"Pr(>|t|)"] <= 0.05
m_taxa2 <- names(m_taxa2[m_taxa2])
m_taxa3 <- summary(lm(PIM ~ ., data = data_var_taxat2))$coefficients[,"Pr(>|t|)"] <= 0.05
m_taxa3 <- names(m_taxa3[m_taxa3])
m_taxa4 <- summary(lm(PIM ~ ., data = data_var_taxat3))$coefficients[,"Pr(>|t|)"] <= 0.05
m_taxa4 <- names(m_taxa4[m_taxa4])

m_taxa1 # t
m_taxa2 # t-1
m_taxa3 # t-2
m_taxa4 # t-3
taxas_sign <- unique(c(m_taxa1,m_taxa2,m_taxa3,m_taxa4))


# teste de cointegração -------------------------------------------

# variáveis com correlação > 0.50
varname80 <- names(which(colSums(corr > 0.8) > 0))
varname80
length(varname80)

corr[,varname80]

# MODELO BOM: ICI + ABCR FUNCEX ABPO IPI HT-CNI NUCI-CNI INA

VARselect(log(data_var[,c("ANFAVEA","FAT-CNI","HT-CNI","INA","PIM")]), lag.max = 6, exogen = data_var[,-c(1:ncol(data))])
teste <- ca.jo(log(data_var[,c("ANFAVEA","FAT-CNI","HT-CNI","INA","PIM")]), type = "eigen", K = 4, ecdet = "const", dumvar = data_var[,-c(1:ncol(data))]) 
summary(teste)
# 2 relações de cointegração
summary(cajorls(teste, r = 1)$rlm)
var2 <- vec2var(teste, r = 1)

mapes <- matrix(NA, ncol = ncol(data) - 1, nrow = 2)
colnames(mapes) <- colnames(data)[1:(ncol(data) - 1)]
rownames <- c("MAPE","sMAPE")

# testar variáveis 1 por 1
for(i in 1:(ncol(data) - 1)){
  teste <- ca.jo(log(data_var[,c(colnames(data)[i],"PIM")]), type = "eigen", K = 2, ecdet = "const", dumvar = data_var[,-c(1:ncol(data))]) 
  var2 <- vec2var(teste, r = 1)
  fits <- ts(fitted(var2), end = end(data_var), freq = 12)
  fit_pim <- cbind(pim, exp(fits[,"fit of PIM"]))
  mapes[,i] <- c(MAPE(fit_pim[,1], fit_pim[,2]), sMAPE(fit_pim[,1], fit_pim[,2]))
}

mapes[,order(t(mapes[1,]))]

# MODELO BOM 1
VARselect(log(data_var[,c(taxas_sign,"PIM")]), lag.max = 6, exogen = data_var[,c("dummy1","dummy2","dummy3","dummy6")]) #exogen = data_var[,-c(1:ncol(data))])
teste <- ca.jo(log(data_var[,c(taxas_sign,"PIM")]), type = "eigen", K = 4, ecdet = "const", dumvar = data_var[,c("dummy1","dummy2","dummy3","dummy6")])#dumvar = data_var[,-c(1:ncol(data))]) 
summary(teste)
# 2 relações de cointegração
summary(cajorls(teste, r = 4)$rlm)
var2 <- vec2var(teste, r = 4)


# MODELO BOM 2 - removendo variáveis nao significativas do modelo anterior
# VARIÁVEIS QUE SOBRARAM: TSN  TFPR FPRM POP THP 

VARselect(log(data_var[,c(taxas_sign[-c(1,2,4,8)],"PIM")]), lag.max = 6, exogen = data_var[,c("dummy1","dummy2","dummy3","dummy6")]) #exogen = data_var[,-c(1:ncol(data))])
teste <- ca.jo(log(data_var[,c(taxas_sign[-c(1,2,4,8)],"PIM")]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[,c("dummy1","dummy2","dummy3","dummy6")])#dumvar = data_var[,-c(1:ncol(data))]) 
summary(teste)
# 2 relações de cointegração
summary(cajorls(teste, r = 3)$rlm)
var2 <- vec2var(teste, r = 3)


# MODELO BOM 3 - adicionando variáveis no modelo anterior
# variáveis que ajudaram: ONS, IPI, ANFAVEA
#ICI + ABCR FUNCEX ABPO IPI HT-CNI NUCI-CNI INA

VARselect(log(data_var[,c(taxas_sign[-c(1,2,4)], nivel_sign[c(1,2,8)],"TN", "PIM")]), lag.max = 6, exogen = data_var[,c("dummy1","dummy2","dummy3","dummy6")]) #exogen = data_var[,-c(1:ncol(data))])
teste <- ca.jo(log(data_var[,c(taxas_sign[-c(1,2,4)], nivel_sign[c(1,2,8)],"TN", "PIM")]), type = "eigen", K = 2, ecdet = "const", dumvar = data_var[,c("dummy1","dummy2","dummy3","dummy6")])#dumvar = data_var[,-c(1:ncol(data))]) 
summary(teste)
# 2 relações de cointegração
summary(cajorls(teste, r = 2)$rlm)
var2 <- vec2var(teste, r = 2)


# MODELO FINAL - adicionando variáveis no modelo anterior
# variáveis que ajudaram: ONS, IPI, ANFAVEA
#ICI + ABCR FUNCEX ABPO IPI HT-CNI NUCI-CNI INA

VARselect(log(data_var[,c("TSN","TFPR","FPRM","POP","THP","ONS","IPI","ANFAVEA","PIM")]), lag.max = 6, exogen = data_var[,c("dummy1","dummy2","dummy3","dummy6")]) #exogen = data_var[,-c(1:ncol(data))])
teste <- ca.jo(log(data_var[,c("TSN","TFPR","FPRM","POP","THP","ONS","IPI","ANFAVEA","PIM")]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[,c("dummy1","dummy2","dummy3","dummy6")])#dumvar = data_var[,-c(1:ncol(data))]) 
summary(teste)
# 3 relações de cointegração
summary(cajorls(teste, r = 3)$rlm)
var2 <- vec2var(teste, r = 3)

# diagnost tests  -------------------------------------------------
serial.test(var2)  # autocorrelação
arch.test(var2) # heterocedasticidade
residuos <- ts(resid(var2), end = end(data_var), freq = 12)
plot(residuos)
dygraph(residuos[,"resids of PIM"])
fits <- ts(fitted(var2), end = end(data_var), freq = 12)
fit_pim <- cbind(pim, exp(fits[,"fit of PIM"]))
MAPE(fit_pim[,1], fit_pim[,2])
sMAPE(fit_pim[,1], fit_pim[,2])

ts.plot(pim,exp(fits[,"fit of PIM"]), col = 1:2)

resid_padr <- (residuos[,"resids of PIM"] - mean(residuos[,"resids of PIM"]))/sd(residuos[,"resids of PIM"]) 
plot(resid_padr)
abline(h = c(-3,3), col = 2, lty = 2)

# forecast  -------------------------------------------------------
h = 12
M <- matrix(NA, ncol = 1, nrow = h)
for(i in h:1){
  # teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c(taxas_sign,"PIM")]), type = "eigen", K = 4, ecdet = "const", dumvar = data_var[1:(nrow(data_var)-i),c("dummy1","dummy2","dummy3","dummy6")])
  # var2 <- vec2var(teste, r = 4)
  # teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c(taxas_sign[-1],"PIM")]), type = "eigen", K = 4, ecdet = "const", dumvar = data_var[1:(nrow(data_var)-i),c("dummy1","dummy2","dummy3","dummy6")])
  # var2 <- vec2var(teste, r = 4)
  # teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c(taxas_sign[-c(1:2)],"PIM")]), type = "eigen", K = 4, ecdet = "const", dumvar = data_var[1:(nrow(data_var)-i),c("dummy1","dummy2","dummy3","dummy6")])
  # var2 <- vec2var(teste, r = 3)
  # teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c(taxas_sign[-c(1,2,4,8)],"PIM")]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[1:(nrow(data_var)-i),c("dummy1","dummy2","dummy3","dummy6")])
  # var2 <- vec2var(teste, r = 3)
  teste <- ca.jo(log(data_var[1:(nrow(data_var)-i),c("TSN","TFPR","FPRM","POP","THP","ONS","IPI","ANFAVEA","PIM")]), 
                 type = "eigen", K = 3, ecdet = "const", dumvar = data_var[1:(nrow(data_var)-i),c("dummy1","dummy2","dummy3","dummy6")])
  var2 <- vec2var(teste, r = 3)
  var_for <- predict(var2, n.ahead=1, ci=0.95, dumvar =  t(data_var[(nrow(data_var)-i)+1,c("dummy1","dummy2","dummy3","dummy6")]))
  M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
}
previsao_var2_nivel <- ts(exp(M), end = end(data_var), freq = 12)
ts.plot(pim, previsao_var2_nivel, col = 1:2)

prevs_inter <- data.frame(data = as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 12)))
prevs_inter$inter_prev <- (prevs_inter[,3]/prevs_inter[,2]-1)*100
prev_inter <- ts(na.omit(prevs_inter$inter_prev), end = end(previsao_var2_nivel), freq = 12)
ts.plot(prev_inter,pim_inter, col = 2:1)
sqrt(mean((pim_inter - prev_inter)^2)) # RMSE
MAPE(pim_inter, prev_inter)
sMAPE(pim_inter, prev_inter)

prevs_margem <- data.frame(as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 1)))
prevs_margem$margem_prev <- (prevs_margem[,3]/prevs_margem[,2]-1)*100
prev_margem <- ts(na.omit(prevs_margem$margem), end = end(previsao_var2_nivel), freq = 12)
pim_margem <- (pim/lag(pim,-1) - 1)*100
ts.plot(round(cbind(prev_margem,pim_margem),2), col = 2:1)
sqrt(mean((pim_margem - prev_margem)^2)) # RMSE
MAPE(pim_margem, prev_margem)
sMAPE(pim_margem, prev_margem)

# previsão fora da amostra --------------------------------------------------------------------

teste <- ca.jo(log(data_var[,c("TSN", "TFPR","FPRM","POP","THP","ONS","IPI","ANFAVEA","PIM")]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[,c("dummy1","dummy2","dummy3","dummy6")]) 
var2 <- vec2var(teste, r = 3)
d <- matrix(rep(c(0,0,0,0),5), ncol = 4)
colnames(d) <- c("dummy1","dummy2","dummy3","dummy6")

data_pim <- tail(as.Date(na.omit(pim)),1) + months(1)
prev <- ts(exp(predict(var2, n.ahead = nrow(d), dumvar = d)$fcst$PIM[,1]),
           start = as.numeric(c(substr(data_pim,1,4),substr(data_pim,6,7))), freq = 12)
ts.plot(pim,prev)
prev
(prev[1]/tail(na.omit(pim),1)-1)*100
(prev[1]/tail(na.omit(pim),12)[1]-1)*100

# STEPWISE ---------------------------------------------------------------------------------------

data <- window(readRDS("00.data/data_multivariado_var2.rds"), start = c(2004,1), end = c(2017,7), freq = 12)#[,c(1:5,7:8)]

# m <- lm(PIM ~., data = diff(data))
# step <- stepAIC(m, direction="forward")
# step <- stepAIC(m, direction="backward")
# xnames <- names(step$coefficients)[-c(1,2,7)]
# 
# m <- lm(PIM ~., data = diff(data)[,c(xnames,"PIM")])
# step <- stepAIC(m, direction="both")
# xnames <- names(step$coefficients)[-c(1)]

exp2 <- cbind(t(combn(colnames(data)[-ncol(data)],2)),"PIM")
exp3 <- cbind(t(combn(colnames(data)[-ncol(data)],3)),"PIM")
exp4 <- cbind(t(combn(colnames(data)[-ncol(data)],4)),"PIM")
exp5 <- cbind(t(combn(colnames(data)[-ncol(data)],5)),"PIM")
exp6 <- cbind(t(combn(colnames(data)[-ncol(data)],6)),"PIM")

dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2012,1))
dummy4 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2016,6))
dummy5 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2009,1))
dummy6 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,6))
dummies <- cbind(dummy1, dummy2, dummy3, dummy4, dummy5, dummy6)

data_var <- cbind(data, dummies)
colnames(data_var) <- c(colnames(data), colnames(dummies))

# forecast
h = 24

erros_nivel <- matrix(NA, ncol = 1, nrow = nrow(exp6))
erros_margem <- matrix(NA, ncol = 1, nrow = nrow(exp6))
erros_inter <- matrix(NA, ncol = 1, nrow = nrow(exp6))
pim_margem <- (data[,"PIM"]/lag(data[,"PIM"],-1)-1)*100 
pim_inter <- (data[,"PIM"]/lag(data[,"PIM"],-12)-1)*100 

a <- Sys.time()
for(i in 1:nrow(exp6)){
  M <- matrix(NA, ncol = 1, nrow = h)
  for(j in h:1){
    teste <- tryCatch(ca.jo(data_var[1:(nrow(data_var)-j),exp6[i,]], type = "eigen", K = 2, ecdet = "const", dumvar = data_var[1:(nrow(data_var)-j),c("dummy2","dummy5")]), error = function(e) NULL)
    var2 <- tryCatch(vec2var(teste, r = 2), error = function(e) NULL)
    var_for <- tryCatch(predict(var2, n.ahead=1, ci=0.95, dumvar =  t(data_var[(nrow(data_var)-j)+1,c("dummy2","dummy5")])), error = function(e) NULL)
    M[h-j+1,] <- tryCatch(round(var_for$fcst$PIM[,1],4), error = function(e) NA)
  }
  prev_nivel <- ts(M, end = end(data[,"PIM"]), freq = 12)
  prev_margem <- (prev_nivel/lag(data[,"PIM"],-1)-1)*100 
  prev_inter <- (prev_nivel/lag(data[,"PIM"],-12)-1)*100 
  
  erros_nivel[i,] <- tryCatch(MAPE(data[,"PIM"],prev_nivel), error = function(e) NA)
  erros_margem[i,] <- tryCatch(RMSE(pim_margem,prev_margem), error = function(e) NA)
  erros_inter[i,] <- tryCatch(RMSE(pim_inter,prev_inter), error = function(e) NA)
  message(i)
}
b <- Sys.time()
b-a

#Time difference of 3.132327 hours 1:67246
# saveRDS(list(erros_nivel = erros_nivel, erros_margem = erros_margem, erros_inter = erros_inter),
#         "erros_var6.rds")


which(erros_nivel == min(erros_nivel, na.rm = T))
which(erros_margem == min(erros_margem, na.rm = T))
which(erros_inter == min(erros_inter, na.rm = T))

variaveis_finais <- exp6[62586,] # exp6[63325,]
# #exp5[106824,]
# variaveis_finais <- unique(c(exp6[63325,], exp6[62586,]))

hist(erros_nivel)
hist(erros_margem)
hist(erros_inter)

VARselect((data_var[,variaveis_finais]), lag.max = 6, exogen = data_var[,c("dummy1","dummy2","dummy3","dummy6")])#,"dummy5")])
teste <- ca.jo((data_var[,variaveis_finais]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[,c("dummy1","dummy2","dummy3","dummy6")])#,"dummy5")])#dumvar = data_var[,-c(1:ncol(data))]) 
summary(teste)
summary(cajorls(teste, r = 3)$rlm)
var2 <- vec2var(teste, r = 3)

# diagnost tests
serial.test(var2)  # autocorrelação
arch.test(var2) # heterocedasticidade
residuos <- ts(resid(var2), end = end(data_var), freq = 12)
plot(residuos)
dygraph((residuos[,"resids of PIM"] - mean(residuos[,"resids of PIM"]))/sd(residuos[,"resids of PIM"])) %>%
  dyShading(from = -3, to = 3, color = "#959595", axis = "y")
fits <- ts(fitted(var2), end = end(data_var), freq = 12)
fit_pim <- cbind(pim, (fits[,"fit of PIM"]))
MAPE(fit_pim[,1], fit_pim[,2])
sMAPE(fit_pim[,1], fit_pim[,2])

ts.plot(pim,(fits[,"fit of PIM"]), col = 1:2)

resid_padr <- (residuos[,"resids of PIM"] - mean(residuos[,"resids of PIM"]))/sd(residuos[,"resids of PIM"]) 
plot(resid_padr)
abline(h = c(-3,3), col = 2, lty = 2)

# forecast
h = 24
M <- matrix(NA, ncol = 1, nrow = h)
for(i in h:1){
  teste <- ca.jo((data_var[1:(nrow(data_var)-i),variaveis_finais]), dumvar = data_var[1:(nrow(data_var)-i),c("dummy1","dummy2","dummy3","dummy6")], 
                 type = "eigen", K = 3, ecdet = "const")
  var2 <- vec2var(teste, r = 3)
  var_for <- predict(var2, n.ahead=1, ci=0.95, dumvar =  t(data_var[(nrow(data_var)-i)+1,c("dummy1","dummy2","dummy3","dummy6")]))
  M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
}
previsao_var2_nivel <- ts((M), end = end(data_var), freq = 12)
ts.plot(pim, previsao_var2_nivel, col = 1:2)

prevs_inter <- data.frame(data = as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 12)))
prevs_inter$inter_prev <- (prevs_inter[,3]/prevs_inter[,2]-1)*100
prev_inter <- ts(na.omit(prevs_inter$inter_prev), end = end(previsao_var2_nivel), freq = 12)
ts.plot(prev_inter,pim_inter, col = 2:1)
sqrt(mean((pim_inter - prev_inter)^2)) # RMSE
MAPE(pim_inter, prev_inter)
sMAPE(pim_inter, prev_inter)

prevs_margem <- data.frame(as.Date(pim), cbind(pim, lag(previsao_var2_nivel, 1)))
prevs_margem$margem_prev <- (prevs_margem[,3]/prevs_margem[,2]-1)*100
prev_margem <- ts(na.omit(prevs_margem$margem), end = end(previsao_var2_nivel), freq = 12)
pim_margem <- (pim/lag(pim,-1) - 1)*100
ts.plot(round(cbind(prev_margem,pim_margem),2), col = 2:1)
sqrt(mean((pim_margem - prev_margem)^2)) # RMSE
MAPE(pim_margem, prev_margem)
sMAPE(pim_margem, prev_margem)

# previsão fora da amostra --------------------------------------------------------------------

teste <- ca.jo(log(data_var[,variaveis_finais]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[,c("dummy1","dummy2","dummy3","dummy6")]) 
var2 <- vec2var(teste, r = 3)
d <- matrix(rep(c(0,0,0,0),1), ncol = 4)
colnames(d) <- c("dummy1","dummy2","dummy3","dummy6")

data_pim <- tail(as.Date(na.omit(pim)),1) + months(1)
prev <- ts(exp(predict(var2, n.ahead = nrow(d), dumvar = d)$fcst$PIM[,1]),
           start = as.numeric(c(substr(data_pim,1,4),substr(data_pim,6,7))), freq = 12)
ts.plot(pim,prev)
prev
(prev[1]/tail(na.omit(pim),1)-1)*100
(prev[1]/tail(na.omit(pim),12)[1]-1)*100


