
pimSARIMA <- function(pim = NULL, dummies = NULL, order = NULL, seasonal = NULL, lambda = NULL){
  # ajusta modelo SARIMA com dummies para a PIM
  #
  # Argumentos:
  #   pim: é a variável pim em nível (formato ts)
  #   dummies: são as dummies que podem ser usadas no modelo (formato ts)
  #   order: parte não sazonal do modelo SARIMA (formato: c(p,d,q))
  #   seasonal: parte sazonal do modelo SARIMA (formato: c(P,D,Q))
  #
  # Retorna:
  #   o modelo estimado, valores ajustados e resíduos padronizados
  
  # arrumar as variáveis que serão utilizadas
  x <- cbind(pim, dummies)
  data_modelo <- na.omit(x)
  
  # estimar modelo
  if(is.null(dim(x))){ # sem dummies
    m <- Arima(x, order = order, seasonal = seasonal, lambda = lambda)
  }else{ # com dummies
    m <- Arima(data_modelo[,1], order = order, seasonal = seasonal, xreg = data_modelo[,-1], lambda = lambda)
  }
  
  # retornar fit, resíduos padronizados e modelo
  return(list(fit = fitted(m), resid = (resid(m) - mean(resid(m)))/sd(resid(m)), model = m))
}