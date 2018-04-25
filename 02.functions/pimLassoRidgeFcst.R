pimLassoRidgeFcst <- function(data, log = F, outDFM = NULL, carnaval = NULL, pim_cajuste = NULL, diff = F, alpha = NULL, h = 24){
  
  #data <- database$dados2
  out <- outDFM[,which(colnames(outDFM) %in% colnames(data))]
  data[(nrow(data)-3):nrow(data),-which(colnames(data) == "PIM")] <- out
  data <- window(data, start = start(na.omit(data[,"PIM"])), end = end(na.omit(data[,"PIM"])), freq = 12)
 
  #h = 24
  M <- matrix(NA, ncol = 1, nrow = h)
  
  if(log){data <- log(data)}
  
  pim <- data[,"PIM"]

  first_fcst <- nrow(data)-h+1
  
  for(i in first_fcst:nrow(data)){
    
    # padronizar
    mean_data <- apply(data[1:i,], MARGIN = 2, FUN = mean)
    sd_data <- apply(data[1:i,], MARGIN = 2, FUN = sd)
    data_pad <- apply(data[1:i,], MARGIN = 2, FUN = function(x) (x - mean(x))/sd(x))
    data_pad[nrow(data_pad),"PIM"] <- NA
    
    if(diff){
      last_pim <- tail(data_pad,2)[1]
      data_pad <- rbind(NA,diff(data_pad))
    }
    
    data_in <- as.matrix(na.omit(data.frame(data_pad[1:(i-1),])))
    data_out <- t(data_pad[i,-which(colnames(data_pad) == "PIM")])
    
    cv.out <- cv.glmnet(x = data_in[,-which(colnames(data_pad) == "PIM")], y = data_in[,"PIM"],
                        alpha = alpha)
    m <- glmnet(x = data_in[,-which(colnames(data_pad) == "PIM")], y = data_in[,"PIM"],
                alpha = alpha, lambda = cv.out$lambda.min)

    fcst <- predict(m, s = cv.out$lambda.min, newx = data_out)
    
    if(diff){
      fcst <- fcst + last_pim
    }
    
    
    
    # despadronizar
    fcst <- fcst*sd_data["PIM"] + mean_data["PIM"]
    
    M[nrow(data) - i + 1,] <- fcst
  }
  
  previsao <- ts(M[h:1], end = end(pim), freq = 12)  
 
  if(log){
    pim <- exp(pim)
    previsao <- exp(previsao)
  }
  
  pim_inter <- 100*(pim/lag(pim,-12)-1)
  
  prev_cajuste <- pimSeasFcst(pim, pim_cajuste, previsao, carnaval)
  previsao_margem <- (prev_cajuste/lag(pim_cajuste,-1)-1)*100
  previsao_inter <- 100*(previsao/lag(pim,-12)-1)

   
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