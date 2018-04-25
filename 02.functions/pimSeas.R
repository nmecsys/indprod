
pimSeas <- function(pim = NULL, carnaval = NULL){
  # Faz o ajuste sazonal da PIM utilizando a metodologia do IBGE
  #
  # Argumentos:
  #   pim: é a variável pim em nível (formato ts)
  #   carnaval: série temporal do carnaval (formato ts)
  #
  # Retorna:
  #   a pim com ajuste sazonal em formato ts
  
  if(tail(as.Date(pim),1) < as.Date("2013-12-01")){
    end_span <- paste0(end(pim)[1],".",c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")[end(pim)[2]])
  }else{
    end_span <- "2013.dec"
  }
  if(head(as.Date(pim),1) > as.Date("2002-01-01")){
    start_span <- paste0(start(pim)[1],".",c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")[start(pim)[2]])
  }else{
    start_span <- "2002.jan"
  }
  
  ajuste <- seas(pim, x11 = "",
                 regression.aictest = c("td", "easter"),
                 xreg = carnaval,
                 regression.usertype = c("holiday"),
                 arima.model = "(0 1 1)(0 2 2)",
                 slidingspans.additivesa = "percent",
                 outlier.types = "all",
                 series.modelspan = paste0(start_span,",",end_span),
                 transform.function = "log",
                 series.decimals = "2",
                 x11.seasonalma = "MSR")

  return(final(ajuste))
}