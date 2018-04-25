# check pckgs and dir ----------------------------------------------

setwd("C:/Users/daiane.mattos/Dropbox/08 Previsão da PIM-PF")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(vars, urca, tseries, forecast, readxl, xtable, seasonal)

source("02.var/estabilidade.var.R")

MAPE <- function(y,x){ 
  data <- na.omit(cbind(y,x))
  mean(abs((data[,1] - data[,2])/data[,1]))*100
}

# read data and ts -------------------------------------------------
data1 <- na.omit(read_excel("00.data/data_dessaz.xlsx"))
data2 <- ts(data1[,-1], start = c(2003,1), freq = 12)
data <- window(data2, start = c(2004,1), freq = 12)

# data1 <- na.omit(readRDS("00.data/data_dessaz.rds"))
# data <- window(data1, start = c(2004,1), end = c(2017,5), freq = 12)

# ipca seas adj ----------------------------------------------------
data[,"IPCA"] <- seas(data[,"IPCA"], regression.aictest = NULL)$series$s11
data[,"SELIC_R"] <- data[,"SELIC"] - data[,"IPCA"]

# variables and order
ord <- c("IIEBR","SELIC_R","CAMB","COMD","DESEM","PIM")
data <- data[,ord]
plot(data)

# var estimation  -------------------------------------------------
data_var <- window(data, end=c(2017,10), freq=12) # data
#data_var <- window(data, end=c(2017,5), freq=12)

VARselect(data_var, lag.max = 6)
var <- VAR(data_var, lag.max = 3)

# diagnost tests  -------------------------------------------------
serial.test(var)  # autocorrelação
arch.test(var) # heterocedasticidade
roots(var)
plot(ts(resid(var)))

# forecast  -------------------------------------------------------
var_for <- predict(var, n.ahead=12, ci=0.95)
pim_for <- ts(round(var_for$fcst$PIM[,1],1),
              start=c(2015,11), frequency = 12)
prevs <- estabilidade.var(data, h = 12)
d <- na.omit(cbind(prevs,data[,"PIM"]))
ts.plot(d, col = 2:1)

# VAR DEFASADO ------------------------------------------------------------

PIM_def <- ts(data[,"PIM"], start=c(2003,12), frequency = 12)
data_def<-ts.intersect(data[,1:5],PIM_def)
colnames(data_def)<-c("IIEBR","SELIC_R","CAMB","COMD","DESEM","PIM")
data_var<-window(data_def, end=c(2015,9), freq=12)

# data_def <- na.omit(cbind(data[,-6], lag(data[,"PIM"],1)))
# colnames(data_def) <- ord

VARselect(data_def, lag.max = 6)
var <- VAR(data_def, lag.max = 2)

# diagnost tests  -------------------------------------------------
serial.test(var)  # autocorrelação
arch.test(var) # heterocedasticidade
roots(var)

prevs <- estabilidade.var(data_def, h = 12)
d <- na.omit(cbind(prevs,data_def[,"PIM"]))
ts.plot(d, col = 2:1)

MAPE(d[,1],d[,2])

# d <- BETS.dummy(start = start(data), end = end(data), year = 2008, month = 12)
# 
# # var estimation  -------------------------------------------------
# data3 <- cbind(data,d)
# VARselect(data3[,-7], lag.max = 6, exogen = data3[,7])
# var<-VAR(data[,-7], lag.max = 2, exogen = data3[,7])
# summary(var)
# 
# # diagnost tests  -------------------------------------------------
# res <- ts(resid(var), end = end(data), freq = 12)
# res
# serial.test(var)$serial$p.value  # autocorrelação
# arch.test(var)$arch.mul$p.value # heterocedasticidade
# roots(var)
# 
# prevs <- estabilidade.var(data, h = 12)
# d <- na.omit(cbind(prevs,data[,"PIM"]))
# ts.plot(d, col = 2:1)