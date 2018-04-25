pimRFFcst <- function(data = NULL, pim_cajuste = NULL, carnaval = NULL, diff = F, outDFM = NULL, type = "regression", h = 24){
  
  message("Variaveis previstas: ",paste0(colnames(data)[which(is.na(tail(data,1)) & colnames(data) != "PIM")], collapse = ", "))
  out <- outDFM[,which(colnames(outDFM) %in% colnames(data))]
  data[(nrow(data)-3):nrow(data),-which(colnames(data) == "PIM")] <- out
 
  data <- na.omit(data)
  pim <- na.omit(data[,"PIM"] )
  if(diff){
    data <- na.omit(diff(data))
  }

  # matriz com as previsões
  #h <- 24
  previsao <- matrix(NA, nrow = h, ncol = 1)
  
  trainData <- NULL
  testData <- NULL
  m <- NULL
  prev <- NULL
  n <- nrow(data)
  
  for(i in h:1){
    
    # treino e teste
    trainData <- data.frame(data[1:(n-i),])
    testData <- data.frame(t(data[(n-i+1),]))
    
    # criar árvore
    if(type == "regression"){
      m <- rpart(PIM ~ ., data = trainData, method = "anova")
    }else if(type == "rf"){
      m <- randomForest(PIM ~ ., data = trainData)
    }
    
    previsao[h-i+1,1] <-  predict(m, newdata = testData[,-which(colnames(testData) == "PIM")])
    
  }
  
  previsao <- ts(previsao, end = end(data), freq = 12)
  
  if(diff){
    previsao <- previsao + lag(pim,-1)
  }
  
  # ajuste sazonal
  prev_cajuste <- pimSeasFcst(pim, pim_cajuste, previsao, carnaval)
  
  previsao_margem <- (prev_cajuste/lag(pim_cajuste,-1) - 1)*100
  previsao_inter <- (previsao/lag(pim,-12) - 1)*100
  

  list(nivel = list(RMSE = RMSE(pim, previsao), MAPE = MAPE(pim, previsao)),
       margem = list(RMSE = RMSE(pim_margem, previsao_margem)),
       inter = list(RMSE = RMSE(pim_inter, previsao_inter)),
       sinal_margem = mean(sign(previsao_margem) == sign(pim_margem))*100,
       sinal_inter = mean(sign(previsao_inter) == sign(pim_inter))*100,
       out = cbind(previsao,
                   prev_cajuste,
                   previsao_margem,
                   previsao_inter))
}