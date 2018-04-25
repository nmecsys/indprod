pimRF <- function(data, carnaval = NULL, outDFM = NULL, diff = F, type = "regression"){
  
  message("Variaveis previstas: ",paste0(colnames(data)[which(is.na(tail(data,1)) & colnames(data) != "PIM")], collapse = ", "))
  out <- outDFM[,which(colnames(outDFM) %in% colnames(data))]
  data[(nrow(data)-3):nrow(data),-which(colnames(data) == "PIM")] <- out
  
  pim <- na.omit(data[,"PIM"])
  
  if(diff){
    data <- diff(data)
  }
 
  # treino e teste
  n <- nrow(data) - 1
  trainData <- na.omit(data.frame(data[1:n,]))
  testData <- data.frame(t(data[n+1,-which(colnames(data) == "PIM")]))
  
  # criar árvore
  if(type == "regression"){
    m <- rpart(PIM ~ ., data = trainData, method = "anova")
  }else if(type == "rf"){
    m <- randomForest(PIM ~ ., data = trainData)
  }
  
  # valores preditos e previstos
  pred <- ts(predict(m), start = c(2002,2), freq = 12)
  prev <- ts(predict(m, newdata = testData), end = end(data), freq = 12)
  
  if(diff){
    pred <- pred + lag(pim,-1)
    prev <- prev + lag(pim,-1)
  }
  
  # ajuste sazonal
  pred_ajustada <- pimSeas(pred, carnaval)
  prev_ajustada <- pimSeas(ts(c(pim,prev), start = start(pim), freq = 12), carnaval)

  prev_margem <- (tail(prev_ajustada,1)/c(tail(pim_cajuste,1)) - 1)*100
  prev_inter <- (prev/tail(pim,12)[1] - 1)*100
  
  # resíduos
  residuos <- pim-pred

  list(fit = pred, fit_ajustado = pred_ajustada, resid = residuos, prev_nivel = prev, prev_nivel_cajuste = tail(prev_ajustada,1), prev_margem = prev_margem, prev_inter = prev_inter)
  
}


# screeplot(pim)
# cl <- kmeans(pim, 4)
# cl$cluster <- as.factor(cl$cluster)
# 
# minimos <- aggregate(data.frame(pim), by = list(cl$cluster), FUN = min)
# maximos <- aggregate(data.frame(pim), by = list(cl$cluster), FUN = max)
# minimos <- minimos[order(minimos$pim),]
# maximos <- maximos[order(maximos$pim),]
# 
# intervalo <- cbind(minimos[,2], maximos[,2])
# dygraph(pim) %>%
#   dySeries("V1", label = "PIM", color = "#084594", strokeWidth = 1.5) %>%
#   dyAxis("y", valueRange = c(min(minimos[,2]), max(maximos[,2])+2)) %>%
#   dyShading(from = intervalo[1,1], to = intervalo[1,2], axis = "y", color = "#FFE8E5") %>%
#   dyShading(from = intervalo[2,1], to = intervalo[2,2], axis = "y", color = "#FFEEE5") %>%
#   dyShading(from = intervalo[3,1], to = intervalo[3,2], axis = "y", color = "#FFFAE5") %>%
#   dyShading(from = intervalo[4,1], to = intervalo[4,2], axis = "y", color = "#F5FAEB")
# 
# 
# # criar 3 clusters então
# data0 <- data.frame(id = cl$cluster, data[,-which(colnames(data) == "PIM")])
# 
# # treino e teste
# n <- nrow(data0)
# k <- 12
# trainData <- data0[1:(n-k),]
# testData <- data0[(n-k+1):n,]
# 
# 
# arvore_dec <- rpart(id ~ ., data=trainData, method = "class", control = rpart.control(minsplit = 30))
# fancyRpartPlot(arvore_dec)
# 
# # tabela de acertos e erros
# pred <- as.vector(apply(round(predict(arvore_dec)), MARGIN = 1, FUN = function(x)  which(x == 1)))
# table(pred,trainData$id)
# data2 <- data.frame(id = data0[,"id"])
# data2$predict <- c(pred, rep(NA,k))
# data2$igual <- data2$predict == data2$id
# sum((data2$predict == data2$id), na.rm = T)/(n-k)
# 
# # base de teste
# pred2 <- predict(arvore_dec, newdata = testData)
# pred2 <- as.vector(apply(round(pred2), MARGIN = 1, FUN = function(x)  which(x == 1)))
# table(pred2,testData$id)
# data2$predict_test <- c(rep(NA,n-k), pred2)
# data2$igual_test <- data2$predict_test == data2$id
# sum((data2$predict_test == data2$id), na.rm = T)/(k)
# 
# # VALORES PREVISTOS
# prev_class <- data.frame(prev = pred2, real = testData$id)
# prev_class$min <- ifelse(prev_class$prev == 1, minimos[which(minimos[,1] == 1),2], 
#                          ifelse(prev_class$prev == 2, minimos[which(minimos[,1] == 2),2],
#                                 ifelse(prev_class$prev == 3, minimos[which(minimos[,1] == 3),2], minimos[which(minimos[,1] == 4),2])))
# prev_class$max <- ifelse(prev_class$prev == 1, maximos[which(maximos[,1] == 1),2], 
#                          ifelse(prev_class$prev == 2, maximos[which(maximos[,1] == 2),2],
#                                 ifelse(prev_class$prev == 3, maximos[which(maximos[,1] == 3),2], maximos[which(maximos[,1] == 4),2])))
# 
# x <- na.omit(cbind(pim, ts(prev_class[,c("min","max")], end = end(pim), freq = 12)))
# colnames(x) <- c("pim","Min","Max")
# dygraph(x) %>%
#   dySeries("pim", strokePattern = "dashed", color = "#084594", strokeWidth = 1.5) %>%
#   dySeries(c("Min","pim","Max"), strokePattern = "dashed", color = "#084594", strokeWidth = 1.5) %>%
#   dyLegend(labelsSeparateLines = T, width = 400 )
