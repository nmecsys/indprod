
pimSeasFcst <- function(pim = NULL, pim_cajuste = NULL, fcstIN = NULL, carnaval = NULL){
  # Faz o ajuste sazonal da previsão da PIM utilizando a metodologia do IBGE
  #
  # Argumentos:
  #   pim: é a variável pim em nível (formato ts)
  #   pim_cajuste: pim em nível com ajuste sazonal (formato ts)
  #   fcstIN: previsoes 1 passo à frente in sample (formato ts), saída da função pimFcstInOut
  #   carnaval: série temporal do carnaval (formato ts)
  #
  # Retorna:
  #   as previsões da pim com ajuste sazonal em formato ts
  
  n.fcst <- length(fcstIN)
  x <- cbind(pim, fcstIN)
  n <- nrow(x)
  
  # objeto com as datas da ST
  datas <- data.frame(anos = as.numeric(substr(as.Date(x)[(n - n.fcst):n],1,4)),
                      meses = as.numeric(substr(as.Date(x)[(n - n.fcst):n],6,7)))
  
  # ajuste mês a mês
  ajustes <- NULL
  for(i in 1:n.fcst){
    Sys.sleep(0.01)
    data_atual <- datas[i,]
    novapim <- ts(c(window(pim, end = c(datas[i,1],datas[i,2]), freq = 12), fcstIN[i]), start = start(pim), freq = 12)
    ajuste <- pimSeas(novapim, carnaval)
    ts.plot(ajuste, pim_cajuste, col = 2:1, lty = c(1,3))
    ajustes <- c(ajustes,ajuste[length(ajuste)])
  }
  
  # retornar previsão ajustada mês a mês
  return(ts(ajustes, start = start(fcstIN), freq = 12))
  
}