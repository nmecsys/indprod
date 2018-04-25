# Script desenvolvido por Daiane Marcolino de Mattos
# nov/2017
# o que faz?
# - carrega pacotes
# - atualiza base de dados
# - estima previsões para a produção industrial usando vários modelos
# - manda dados para o app em shiny
### ATENÇÃO: 
#  a linha de atualização da base dados [18] está comentada, 
#  descomente sempre que quiser atualizar a base de dados.

# diretório
rm(list = ls())
setwd("C:/Users/daiane.mattos/Dropbox/08 Previsão da PIM-PF/03.paper/replicação para os últimos anos/")

# carregar pacotes
source("02.functions/loadPackages.R")

# atualizar dados para previsão
# source("02.functions/updateData.R", encoding = "utf8")

# leitura dos dados que serão utilizados, apenas um passo de previsão
source("02.functions/readData.R")

# carregar todas as funções que serão utilizadas na previsão
sapply(paste0(getwd(),"/02.functions/", list.files("./02.functions")[-which(list.files("./02.functions") %in% c("loadPackages.R","updateData.R","readData.R"))]),
       FUN = source)

# previsão via modelo SARIMA
source("01.models/03.sarima/fcstSARIMA.R")

# previsão via modelo DFM
# source("01.models/01.dfm/fcstDFM.R")

# previsão via modelo DFM (KF)
source("01.models/01.dfm/fcstKF.R")

# previsão via modelo de regressão (LM)
source("01.models/04.regression/fcstLM.R")

# previsão via modelo de regressão (LM) - IPEA
source("01.models/04.regression/fcstLM_ipea.R")

# previsão via modelo Lasso e Ridge
source("01.models/06.lasso/fcstLassoRidge.R")

# previsão via Random Forest (RF)
source("01.models/07.forest/fcstRF.R")

# combinação de previsões
source("01.models/05.combination/fcstComb.R")

# histórico de previsões
source("02.functions/monthFcst.R")

# exportar dados para a pasta do Shiny
move2shiny()

