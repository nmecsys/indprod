pimSARIMAFcst <- function(pim = NULL, dummies = NULL, model = NULL, level = 95, h = 24, IN = TRUE){
  # pim: produção industrial (formato ts)
  # dummies: xreg no modelo
  # model: saída da função pimSARIMA ($model)
  # n.fcst: número de previsões à frente
  # IN: logico (TRUE ou FALSE), TRUE para exercício dentro da amostra (1 passo à frente)
  # level: nível de confiança
  n.fcst <- h
  # tamanho da ST
  n <- length(pim)
  
  # variáveis que serão utilizadas
  x <- cbind(pim, dummies)  
  
  # definir modelo igual ao utilizado na função pimSARIMA
  order <- model$arma[c(1,6,2)]
  seasonal <- model$arma[c(3,7,4)]
  lambda <- model$lambda[1]
  
  # se IN = TRUE: previsão dentro da amostra
  if(IN){
    
    if(is.null(dim(x))){ # SE NÃO TEM XREG
      
      y0 <- na.omit(pim)
      
      # objeto com as datas da ST
      datas <- data.frame(anos = as.numeric(substr(as.Date(y0),1,4)),
                          meses = as.numeric(substr(as.Date(y0),6,7)))
      
      # objeto para armazenar as previsões
      previsoes <- matrix(NA, ncol = 1, nrow = n.fcst)
      colnames(previsoes) <- "PIM CHAPÉU"
      
      for(i in n.fcst:1){
        y1 <- ts(y0[1:(nrow(y0)-i),], end = c(datas[nrow(datas)-i,1],datas[nrow(datas)-i,2]), freq = 12)
        new.model <- Arima(y1[,1], order = order, seasonal = seasonal, xreg = y1[,-1], lambda = lambda)
        new.xreg <- window(y0[,-1], start = c(datas[nrow(datas)-i+1,1],datas[nrow(datas)-i+1,2]), 
                           end = c(datas[nrow(datas)-i+1,1],datas[nrow(datas)-i+1,2]), freq = 12)
        prev <- forecast(new.model, level = level)
        previsoes[n.fcst-i+1] <- prev$mean[1]
      }
      
    }else{ # SE HOUVER XREG
      
      y0 <- na.omit(x)
      
      # objeto com as datas da ST
      datas <- data.frame(anos = as.numeric(substr(as.Date(y0),1,4)),
                          meses = as.numeric(substr(as.Date(y0),6,7)))
      
      # objeto para armazenar as previsões
      previsoes <- matrix(NA, ncol = 1, nrow = n.fcst)
      colnames(previsoes) <- "PIM CHAPÉU"
      
      for(i in n.fcst:1){
        y1 <- ts(y0[1:(nrow(y0)-i),], end = c(datas[nrow(datas)-i,1],datas[nrow(datas)-i,2]), freq = 12)
        new.model <- Arima(y1[,1], order = order, seasonal = seasonal, xreg = y1[,-1], lambda = lambda)
        new.xreg <- window(y0[,-1], start = c(datas[nrow(datas)-i+1,1],datas[nrow(datas)-i+1,2]), 
                           end = c(datas[nrow(datas)-i+1,1],datas[nrow(datas)-i+1,2]), freq = 12)
        prev <- forecast(new.model, xreg = new.xreg, level = level)
        previsoes[n.fcst-i+1] <- prev$mean[1]
      }
    }
    
    previsoes.ts <- ts(previsoes, end = end(y0), freq = 12)
    
    # retornar previsões um passo à frente IN SAMPLE
    return(previsoes.ts)
    
  }else{ # SE IN = FALSE: previsão fora da amostra
    
    if(is.null(dim(x))){ # SE NÃO TEM XREG
      y0 <- pim
      h <- sum(is.na(y0))
      
      # estimar modelo
      m <- Arima(na.omit(y0)[,1], order = order, seasonal = seasonal, lambda = lambda)
      
      # previsão
      previsao <- forecast(m, level = level, h = h)
      
    }else{
      
      y0 <- x
      h <- sum(is.na(x[,1]))
      
      # estimar modelo
      m <- Arima(na.omit(y0)[,1], order = order, seasonal = seasonal, xreg = na.omit(y0)[,-1], lambda = lambda)
      
      # novos dados para previsão
      #novo_xreg <- t((data.frame(y0[is.na(y0[,1]), -1])))
      novo_xreg <- data.frame(matrix(0, ncol = ncol(y0) - 1, nrow = h))
      colnames(novo_xreg) <- colnames(y0[,-1])
      
      # previsão
      previsao <- forecast(m, level = level, xreg = novo_xreg)
      
    }
    
    # retornar previsao OUT OF SAMPLE
    return(previsao)
    
  }
  
}


