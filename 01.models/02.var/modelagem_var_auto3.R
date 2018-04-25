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
pim <- readRDS("00.data/pim_semajuste.rds")
pim_cajuste <- readRDS("00.data/pim_comajuste.rds")

data <- window(readRDS("00.data/data_multivariado_var2.rds"), start = c(2004,1), freq = 12)[,c("FUNCEX","ABPO","ONS","TSR","TVR","NUCI","PIM")]
plot(data)

dummy1 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,11))
dummy2 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,12))
dummy3 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2012,1))
dummy6 <- BETS.dummy(start = start(data), end = end(data), frequency = 12, date = c(2008,6))
dummies <- cbind(dummy1, dummy2, dummy3, dummy6)

pim <- na.omit(data[,"PIM"])
pim_margem <- (pim/lag(pim,-1) - 1)*100
pim_inter <- (pim/lag(pim,-12) - 1)*100

# mts de variáveis que podem entrar no var
data_var <- na.omit(cbind(data, dummies))
colnames(data_var) <- c(colnames(data), "dummy1", "dummy2","dummy3","dummy6")

# numero de variaveis
nv <- ncol(data_var)
nd <- ncol(dummies)

# estimação do modelo -------------------------------------------
teste <- ca.jo((data_var[,1:(nv-nd)]), type = "eigen", K = 3, ecdet = "const", dumvar = data_var[,(nv-nd+1):(nv)]) 
var2 <- vec2var(teste, r = 3)
fit <- ts(fitted(var2), end = end(data_var), freq = 12)
ts.plot(pim, (fit[,"fit of PIM"]), col = c(1, "orangered"), lty = c(3,1))

# previsão fora da amostra --------------------------------------------------------------------
n <- 1
d <- matrix(rep(c(0,0,0,0),n), ncol = 4)
colnames(d) <- c("dummy1", "dummy2","dummy3","dummy6")
data_pim <- tail(as.Date(pim),1) + months(1)
prev <- ts((predict(var2, n.ahead = nrow(d), dumvar = d)$fcst$PIM[,1]),
           start = as.numeric(c(substr(data_pim,1,4),substr(data_pim,6,7))), freq = 12)

prev_margem <- (prev/as.numeric(tail(pim,1)) - 1)*100
prev_inter <- (prev/as.numeric(tail(pim,12)[1])-1)*100

# exportar resultados --------------------------------------------
resultados <- list(pim_nivel = prev, 
                   pim_margem = prev_margem,
                   pim_inter = prev_inter)

saveRDS(resultados, "02.var/previsao_VAR.rds")

# previsão dentro da amostra  -------------------------------------------------------

h = 24
M <- matrix(NA, ncol = 1, nrow = h)
for(i in h:1){
  teste <- ca.jo((data_var[1:(nrow(data_var)-i),1:(nv-nd)]), dumvar = data_var[1:(nrow(data_var)-i),(nv-nd+1):(nv)], type = "eigen", K = 3, ecdet = "const")
  var2 <- vec2var(teste, r = 3)
  var_for <- predict(var2, n.ahead=1, ci=0.95, dumvar = t(data_var[(nrow(data_var)-i)+1,(nv-nd+1):(nv)]))
  M[h-i+1,] <- round(var_for$fcst$PIM[,1],4)
}
previsao_var2_nivel <- ts((M), end = end(data_var), freq = 12)
ts.plot(pim, previsao_var2_nivel, col = 1:2)

prev_inter <- (previsao_var2_nivel/lag(pim,-12)-1)*100
ts.plot(prev_inter,pim_inter, col = 2:1)

prev_margem <- (previsao_var2_nivel/lag(pim_cajuste,-1)-1)*100
ts.plot(round(cbind(prev_margem,pim_margem),2), col = 2:1)

RMSE(pim_margem, prev_margem)
RMSE(pim_inter, prev_inter)

# exportar resultados
resultados2 <- list(pim_nivel =  previsao_var2_nivel,
                    pim_margem = prev_margem,
                    pim_inter = prev_inter)
saveRDS(resultados2, "02.var/previsao_VAR_2anos.rds")

