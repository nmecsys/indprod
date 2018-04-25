#  diretório de trabalho ----

diretorios = list("C:\\Users\\lucas.farias\\Dropbox\\3.Work\\FGV\\IIE-Br - Estudos\\Incerteza vs variáveis macroeconômicas",
                  "C:\\Users\\lucasfariaslf\\Dropbox\\3.Work\\FGV\\IIE-Br - Estudos\\Incerteza vs variáveis macroeconômicas",
                  "/home/lucasfariaslf/Dropbox/3.Work/FGV/IIE-Br - Estudos/Incerteza vs variáveis macroeconômicas/01.code"
)
for (diret in diretorios){
  try(setwd(diret), silent = T)
}
rm(diret, diretorios)
getwd()


# pkgs --------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(vars, urca, tseries, forecast, readxl, devtools, mFilter, xtables)

# read --------------------------------------------------------------------
base0 <- na.omit(read_excel("C:\\Users\\lucas.farias\\Dropbox\\3.Work\\NMEC\\iiebr\\Incerteza vs variáveis macroeconômicas\\02.data\\data.xlsx"))

# into ts -----------------------------------------------------------------
base1 <- ts(base0[,-1], start = c(2002,1), freq = 12)
dim(base1)

# time window -------------------------------------------------------------
base2 <- window(base1, start = c(2003,12), freq = 12)

# plot --------------------------------------------------------------------
# plot(base2[,1:10])
# plot(base2[,11:20])

vars <- c("IIEBR","CAMB","COMD","DESEM","IBC")

base3<-base2

for(i in seq_along(vars)){
  base3[,vars[i]] <- hpfilter(log(base2[,vars[i]]))$cycle
}

base3[,"SELIC"]<-hpfilter(base2[,"SELIC"])$cycle
# base3[,"FED"]<-hpfilter(base2[,"FED"])$cycle
base3[,"IPCA"]<-hpfilter(base2[,"IPCA"])$cycle
dim(base3)

data<-cbind(base3[,vars],base3[,"SELIC"],base3[,"IPCA"],
            base2[,"var_4"],base2[,"var_8"],base2[,"var_10"],base2[,"var_15"],
            base2[,"niv_90"],base2[,"niv_95"],base2[,"niv_100"],
            base2[,"niv_105"],base2[,"niv_110"],base2[,"niv_115"],base2[,"niv_120"])
dim(data)

colnames(data) <- c("IIEBR","CAMB","COMD","DESEM","IBC",
                    "SELIC","IPCA",
                    "var_4","var_8","var_10","var_15",
                    "niv_90","niv_95","niv_100","niv_105","niv_110","niv_115","niv_120")

data<-data[,c("IIEBR","SELIC","IPCA","CAMB","COMD","DESEM","IBC",
              "var_4","var_8","var_10","var_15",
              "niv_90","niv_95","niv_100","niv_105","niv_110","niv_115","niv_120")]
dim(data)

###############################################################

# order <- data[,c("IBOV","SELIC","CAMB","IBC")]
order <- data[,c("var_4","SELIC","IPCA","CAMB","COMD","DESEM","IBC")]

VARselect(order, lag.max = 6)
# VARselect(order1, lag.max = 6, exogen = data[,c("FED","COMD","CAMB")])

#estimação  em forma reduzida
m <- VAR(order, p = 1, type = "const")
summary(m)

#diagnóstico
serial.test(m)$serial$p.value  # autocorrelação
arch.test(m)$arch.mul$p.value # heterocedasticidade
roots(m) # invertibilidade

roots(vh)

fri <- irf(m, impulse = "SELIC", response = "IBC", n.ahead = 50, cumulative = F, ci = 0.9)
plot(fri, main ="SEM IIEBR")