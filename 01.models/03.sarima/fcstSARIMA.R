
# modelo pim ---------------------------------------------------------
m <- pimSARIMA(pim, dummies = dummies[,2:3], order = c(2,1,2), seasonal = c(0,1,1), lambda = NULL)
#BETS.t_test(m$model)
#plot(m$model)
#ts.plot(pim, m$fit, col = c(1, "orangered"), lty = c(3,1))
#dygraph(cbind(pim,m$fit), main = "PIM Fit")

# exercício de previsão dentro da amostra pim -------------------------------------

# previsão sem ajuste
previsao_sajuste <- pimSARIMAFcst(pim, dummies = dummies[,2:3], model = m$model, h = 48, IN = T)
#ts.plot(pim,previsao_sajuste, col = c(1,"orangered"), lty = c(3,1))
#dygraph(na.omit(cbind(pim,previsao_sajuste), main = "PIM fcst in-sample"))

# previsão com ajuste
previsao_cajuste <- pimSeasFcst(pim, pim_cajuste, previsao_sajuste, carnaval)
#ts.plot(previsao_cajuste, pim_cajuste, col = 2:1, lty = c(1,3))
#dygraph(na.omit(cbind(pim_cajuste,previsao_cajuste)), main = "PIM fcst in-sample adjusted")

# margem e interanual
prev_margem <- (previsao_cajuste/lag(pim_cajuste,-1)-1)*100
prev_inter <- (previsao_sajuste/lag(pim,-12)-1)*100
# ts.plot(prev_inter,pim_inter, col = 2:1)
# ts.plot(prev_margem,pim_margem, col = 2:1)
#dygraph(na.omit(cbind(pim_inter,prev_inter)), main = "interanual in-sample")
#dygraph(na.omit(cbind(pim_margem,prev_margem)), main = "margem in-sample")

# previsão fora da amostra pim ---------------------------------------
previsaoOUT <- pimSARIMAFcst(pim, dummies = dummies[,2:3], model = m$model, IN = F)

# nova pim
nova_pim <- ts(c(pim, previsaoOUT$mean[1]), start = start(pim), freq = 12)

# pim (previsao) em nível com ajuste sazonal
pim_ajustada <- pimSeas(nova_pim, carnaval = carnaval)

# margem e interanual
previsaoOUT_margem <-  window(tail((pim_ajustada/lag(pim_cajuste,-1)-1)*100,1), end = end(pim_ajustada), freq = 12)
previsaoOUT_inter <- window((previsaoOUT$mean/lag(pim,-12)-1)*100, end = end(previsaoOUT_margem), freq = 12)


# exportar resultados --------------------------------------------
resul_out <- cbind(window(previsaoOUT$mean, start = start(previsaoOUT_margem), end = end(previsaoOUT_margem), freq = 12),
                   window(pim_ajustada, start = start(previsaoOUT_margem), end = end(previsaoOUT_margem)),
                   previsaoOUT_margem, previsaoOUT_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resul_in <- cbind(previsao_sajuste, previsao_cajuste, prev_margem, prev_inter)
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/03.sarima/sarima_model.rds")

resultadosSARIMA <- resultados
rm(m, previsao_sajuste, previsao_cajuste, prev_margem, prev_inter, previsaoOUT, nova_pim, pim_ajustada,
   previsaoOUT_margem, previsaoOUT_inter, resul_out, resul_in, resultados)