# modelo dentro da amostra ----------------------------------------------------------
# out <- tail(database[,-1],1)
# out[is.na(out)] <- c(tail(database[,-1],2)[1,is.na(out)])
# names(out)[which(names(out) == "FAT.CNI")] <- "FAT-CNI"
# m <- pimLM(data = database[,-2], lagsy = c(1,2,3,12), outDFM = out[,-1], carnaval = carnaval, log = F, diff = T)

m1 <- pimFactors(database$dados1[,-2], lagsy = NULL, lagsf = NULL, n.fatores = 1, p = 1, log = F,  diff = T,
                 carnaval = carnaval, pim_cajuste = pim_cajuste)

#saveRDS(m1$fcst_x,"06.shiny/data/fcstx.rds")
m <- pimLM(database$dados1[,-2], lagsy = c(1,2,3,12), outDFM = m1$fcst_x, carnaval = carnaval, log = F, diff = T)

# exercício fora da amostra -----------------------------------------------------
m_out <-  pimLMFcst(database$dados1[,-c(2)], lagsy = c(1,2,3,12), outDFM = m1$fcst_x, log = F, carnaval = carnaval, pim_cajuste = pim_cajuste, diff = T, h = 48)

# ts.plot(na.omit(cbind(m_out$out[,"previsao_inter"], pim_inter)), col = 2:1, main = "year over year")
# ts.plot(na.omit(cbind(m_out$out[,"previsao"], pim)), col = 2:1, main = "nível")
# ts.plot(na.omit(cbind(m_out$out[,"previsao_margem"], pim_margem)), col = 2:1, main = "margem")
# 
# RMSE(m_out$out[,"previsao_inter"], pim_inter)
# RMSE(m_out$out[,"previsao_margem"], pim_margem)
# RMSE(m_out$out[,"previsao"], pim)
# MAPE(m_out$out[,"previsao"], pim)

# exportar resultados --------------------------------------------
resul_out <- cbind(m$prev_nivel,m$prev_nivel_cajuste, m$prev_margem, m$prev_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste", "margem","inter")

resul_in <- m_out$out
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/04.regression/regression_model.rds")

resultadosLM <- resultados
rm(m1, m, m_out, resul_out, resul_in, resultados)
