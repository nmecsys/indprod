# update data

# produção industrial ------------------------------------------------------------------

# pim sem ajuste sazonal 
pim_sajuste <- ts(BETS.sidra.get(x = 3653, from = c("200201"), to = paste0(format(Sys.Date(),"%Y"),format(Sys.Date(),"%m")), variable = 3135, sections = 129314, cl = 544)[[1]]$Valor,
                  start = c(2002,01), freq = 12)

if(!is.null(pim_sajuste)){
  message("PIM sem ajuste:")
  print(tail(pim_sajuste))
}else{
  stop("PIM sem ajuste!")
}

# pim com ajuste sazonal
pim_cajuste <- ts(BETS.sidra.get(x = 3653, from = c("200201"), to = paste0(format(Sys.Date(),"%Y"),format(Sys.Date(),"%m")), variable = 3134, sections = 129314, cl = 544)[[1]]$Valor,
                  start = c(2002,01), freq = 12)

if(!is.null(pim_cajuste)){
  message("PIM com ajuste:")
  print(tail(pim_cajuste))
}else{
  stop("PIM com ajuste!")
}

# anfavea ------------------------------------------------------------------------------
# PRODUÇÃO TOTAL DE AUTOVEICULOS - SEM AJUSTE SAZONAL
anfavea <- ts(BETS.get("1373")[,2], start = c(1993,1), freq = 12)

if(!is.null(anfavea)){
  message("Anfavea:")
  print(tail(anfavea))
}else{
  stop("Anfavea!")
}

# abcr ---------------------------------------------------------------------------------
# ABCR - Fluxo de veículos pesados - Brasil - sem ajuste sazonal
download.file("http://www.abcr.org.br/Download.ashx?arquivo=IndiceABCR.xls", 
              destfile = "00.data/indice_abcr.xls", mode = "wb", quiet = T)
abcr_semajuste <- ts(read_excel("00.data/indice_abcr.xls", sheet = 3, skip = 2)[,3],
                     start = c(1999,1), freq = 12)


if(!is.null(abcr_semajuste)){
  message("ABCR sem ajuste:")
  print(tail(abcr_semajuste))
}else{
  stop("ABCR sem ajuste!")
}

# abpo ---------------------------------------------------------------------------------
# ABPO - papelão - sem ajuste sazonal
abpo <- window(ts(data.frame(series_ipeadata(arg1 = "31873", periodicity = 'M'))[,2], start = c(1980,1), freq = 12),
               start = c(2000,1), freq = 12)

if(!is.null(abpo)){
  message("ABPO:")
  print(tail(abpo))
}else{
  stop("ABPO!")
}

# eletrobras ---------------------------------------------------------------------------
# ELETROBRAS - CONSUMO MÉDIO BRASIL TOTAL - SEM AJUSTE SAZONAL
eletrobras <- ts(BETS.get("1406")[,2], start = c(1980,1), freq = 12)

if(!is.null(eletrobras)){
  message("Eletrobras:")
  print(tail(eletrobras))
}else{
  stop("Eletrobras!")
}

# funcex -------------------------------------------------------------------------------
# FUNCEX - ipeadata - IMP - total
funcex_imp_total <- ts(data.frame(series_ipeadata(arg1 = "35590", periodicity = 'M'))[,2], start = c(1978,1), freq = 12)

if(!is.null(funcex_imp_total)){
  message("FUNCEX - Importação - Total:")
  print(tail(funcex_imp_total))
}else{
  stop("FUNCEX - Importação - Total!")
}

# FUNCEX - ipeadata - IMP - intermediarios
funcex_imp_int <- ts(data.frame(series_ipeadata(arg1 = "31017", periodicity = 'M'))[,2], start = c(1997,1), freq = 12)

if(!is.null(funcex_imp_int)){
  message("FUNCEX - Importação - Intermediários:")
  print(tail(funcex_imp_int))
}else{
  stop("FUNCEX - Importação - Intermediários!")
}

# sondagem da indústria ----------------------------------------------------------------
# FGV SONDAGEM DA INDÚSTRIA - SEM AJUSTE SAZONAL
link <- "V:/Informacoes empresariais/Sondagem da Indústria/Séries históricas/Série histórica - Sondagem da Indústria de Transformação - sem ajuste sazonal_mensal.xlsx"
sond_semajuste <- data.frame(read_excel(link, sheet = 1, skip = 4))
sond_semajuste <- sond_semajuste[1:(min(which(is.na(as.numeric(sond_semajuste[,1]))))-1),c(1,6,14:25)]
colnames(sond_semajuste) <- c("data","NUCI","DT","DI","DE","NE","SAN","DTP","DIP","DEP", "PP","EP","TN","ICI")
sond_semajuste[,1] <- as.Date(as.numeric(sond_semajuste[,1]), origin = "1900-01-01")
data_fim <- as.numeric(c(substr(sond_semajuste[nrow(sond_semajuste),1],1,4),substr(sond_semajuste[nrow(sond_semajuste),1],6,7)))
sond_semajuste <- ts(sond_semajuste[,-1], freq = 12, end = data_fim)
colnames(sond_semajuste) <- paste0("SOND_",colnames(sond_semajuste))

if(!is.null(sond_semajuste)){
  message("Sondagens:")
  print(tail(sond_semajuste))
}else{
  stop("Sondagens!")
}

# CNI ----------------------------------------------------------------------------------

# FATURAMENTO REAL CNI - com ajuste sazonal
fat_cni <- window(ts(data.frame(series_ipeadata(arg1 = "33218", periodicity = 'M'))[,2], start = c(1992,1), freq = 12), start = c(2000,1), freq = 12)
fat_cni_semajuste <- window(ts(data.frame(series_ipeadata(arg1 = "33217", periodicity = 'M'))[,2], start = c(1992,1), freq = 12), start = c(2000,1), freq = 12)

if(!is.null(fat_cni_semajuste)){
  message("FATURAMENTO - CNI - sem ajuste:")
  print(tail(fat_cni_semajuste))
}else{
  stop("FATURAMENTO - CNI - sem ajuste!")
}

if(!is.null(fat_cni)){
  message("FATURAMENTO - CNI - com ajuste:")
  print(tail(fat_cni))
}else{
  stop("FATURAMENTO - CNI - com ajuste!")
}

# FIESP --------------------------------------------------------------------------------

# INA
link_ina <- read_html("http://www.fiesp.com.br/indices-pesquisas-e-publicacoes/ina-levantamento-de-conjuntura-3/") %>%
  html_nodes("p a")
link_ina <- link_ina[str_detect(link_ina, "vo.msecnd.net")] %>% html_attr(name = "href")
link_ina_sem_ajuste <- link_ina[str_detect(link_ina, "inasemajuste")]
if(is.null(link_ina_sem_ajuste)){ link_ina_sem_ajuste <- link_ina[str_detect(link_ina, "ina_sem_ajuste")]}
link_ina_categorias <- link_ina[str_detect(link_ina, "total")]
download.file(link_ina_sem_ajuste, destfile = "00.data/ina.xlsx", mode = "wb", quiet = T)
download.file(link_ina_categorias, destfile = "00.data/ina_categorias.xlsx", mode = "wb", quiet = T)
ina <- ts(read_excel("00.data/ina.xlsx")[,2], start = c(2001,6), freq = 12)
ina_categorias <- ts(read_excel("00.data/ina_categorias.xlsx", skip = 2, sheet = 2)[,c("TVR","TSR","TFPR","HTP")], start = c(2001,1), freq = 12)

if(!is.null(ina)){
  message("INA:")
  print(tail(ina))
}else{
  stop("INA!")
}

if(!is.null(ina_categorias)){
  message("INA - Categorias:")
  print(tail(ina_categorias))
}else{
  stop("INA - Categorias!")
}


# AÇO BRUTO ----------------------------------------------------------------------------------

# Produção de aço bruto
aco <- window(ts(data.frame(series_ipeadata(arg1 = "36495", periodicity = 'M'))[,2], start = c(1980,1), freq = 12), start = c(2000,1), freq = 12)

if(!is.null(aco)){
  message("PRODUÇÃO DE AÇO BRUTO:")
  print(tail(aco))
}else{
  stop("PRODUÇÃO DE AÇO BRUTO!")
}

# EXPORTAR -----------------------------------------------------------------------------

# pim
saveRDS(pim_cajuste, "00.data/pim_comajuste.rds")
saveRDS(pim_sajuste, "00.data/pim_semajuste.rds")

# multivariado
dados <- list(
  dados1 = window(cbind(pim_sajuste, anfavea, abcr_semajuste, abpo, eletrobras, funcex_imp_total, sond_semajuste[,c("SOND_TN","SOND_DEP")], fat_cni_semajuste, ina, ina_categorias[,c("TVR","TSR","TFPR","HTP")]),
                start = c(2001,1), freq = 12),
  dados2 =  window(cbind(pim_sajuste, anfavea, abcr_semajuste, abpo, eletrobras, funcex_imp_total, sond_semajuste, fat_cni_semajuste, ina, ina_categorias, aco),
                   start = c(2001,1), freq = 12),
  dados3 =  window(cbind(pim_sajuste, anfavea, abcr_semajuste, abpo, funcex_imp_int, sond_semajuste[,c("SOND_NUCI","SOND_ICI","SOND_NE")], aco),
                   start = c(2001,1), freq = 12)
)
colnames(dados$dados1) <- c("PIM","ANFAVEA","ABCR","ABPO","ELETROBRAS","FUNCEX_IMP_TOTAL","SOND_TN","SOND_DEP","FAT_CNI","INA","TVR","TSR","TFPR","HTP")
colnames(dados$dados2) <- c("PIM","ANFAVEA","ABCR","ABPO","ELETROBRAS","FUNCEX_IMP_TOTAL",colnames(sond_semajuste),"FAT_CNI","INA",colnames(ina_categorias), "ACO")
colnames(dados$dados3) <- c("PIM","ANFAVEA","ABCR","ABPO","FUNCEX_IMP_INT","SOND_NUCI","SOND_ICI","SOND_NE","ACO")
saveRDS(dados, "00.data/database.rds")
rm(dados,ina_categorias, sond_semajuste, abcr_semajuste, abpo, anfavea, data_fim, eletrobras, fat_cni, fat_cni_semajuste,
   funcex_imp_total,funcex_imp_int,ina,link,link_ina,link_ina_categorias,link_ina_sem_ajuste,pim_cajuste,pim_sajuste,aco)
