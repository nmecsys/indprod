# producao industrial
pim <- readRDS("00.data/pim_semajuste.rds")
pim_cajuste <- readRDS("00.data/pim_comajuste.rds")
pim_margem <- (pim_cajuste/lag(pim_cajuste,-1) -1)*100
pim_inter <- (pim/lag(pim,-12) -1)*100

# dummies
hoje <- as.numeric(c(substr(Sys.Date(),1,4), substr(Sys.Date(),6,7)))
dummy1 <- BETS.dummy(start = start(pim), end = hoje, frequency = 12, date = c(2008,11))
dummy2 <- BETS.dummy(start = start(pim), end = hoje, frequency = 12, date = c(2008,12))
dummy3 <- BETS.dummy(start = start(pim), end = hoje, frequency = 12, date = c(2009,1))
dummies <- cbind(dummy1, dummy2, dummy3)

# st do carnaval
carnaval <- readRDS("00.data/carnaval.rds")

# base de dados para os modelos
database <- readRDS("00.data/database.rds")
fim1 <- end(ts(c(na.omit(database$dados1[,"PIM"]),NA), start = start(na.omit(database$dados1[,"PIM"])), freq = 12))
fim2 <- end(ts(c(na.omit(database$dados2[,"PIM"]),NA), start = start(na.omit(database$dados2[,"PIM"])), freq = 12))
database$dados1 <- window(database$dados1, end = fim1, freq = 12)
database$dados2 <- window(database$dados2, end = fim2, freq = 12)

# previsão via DFM da base de dados
#fcstx <- readRDS("01.models/01.dfm/dfm_fcstx.rds")

# base de dados do mercado
market <- read.csv2("00.data/Mediana_ecos_pim.csv")
market <- ts(market[nrow(market):1,-c(1,4)]*100, end = c(2017,8), freq = 12)
colnames(market) <- c("mom","yoy")

rm(dummy1,dummy2,dummy3,hoje,fim1,fim2)