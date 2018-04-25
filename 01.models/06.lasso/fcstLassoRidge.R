
# LASSO -----------------------------------------------------------------------------

# modelo dentro da amostra ----------------------------------------------------------
m1 <- pimFactors(database$dados2, lagsy = NULL, lagsf = NULL, n.fatores = 1, p = 1, log = T,  diff = T,
                 carnaval = carnaval, pim_cajuste = pim_cajuste)
#saveRDS(m1$fcst_x,"06.shiny/data/fcstx_lasso.rds")
m <- pimLassoRidge(database$dados2, outDFM = m1$fcst_x, carnaval = carnaval, log = T, diff = T, pim_cajuste = pim_cajuste, alpha = 1)

# exercício fora da amostra ---------------------------------------------------------
m_out <-  pimLassoRidgeFcst(database$dados2,  outDFM = m1$fcst_x, log = T, diff = T, carnaval = carnaval, pim_cajuste = pim_cajuste, alpha = 1, h = 48)

# ts.plot(na.omit(cbind(m_out$out[,"previsao_inter"], pim_inter)), col = 2:1, main = "year over year")
# ts.plot(na.omit(cbind(m_out$out[,"previsao"], pim)), col = 2:1, main = "nível")
# ts.plot(na.omit(cbind(m_out$out[,"previsao_margem"], pim_margem)), col = 2:1, main = "margem")

# exportar resultados ---------------------------------------------------------------
resul_out <- cbind(m$prev_nivel,m$prev_nivel_cajuste, m$prev_margem, m$prev_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste", "margem","inter")

resul_in <- m_out$out
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/06.lasso/lasso_model.rds")

message("LASSO OK")

resultadosLASSO <- resultados


# RIDGE -----------------------------------------------------------------------------

# modelo dentro da amostra ----------------------------------------------------------
m <- pimLassoRidge(database$dados2, outDFM = m1$fcst_x, carnaval = carnaval, log = T, diff = T, pim_cajuste = pim_cajuste, alpha = 0)

# exercício fora da amostra ---------------------------------------------------------
m_out <-  pimLassoRidgeFcst(database$dados2,  outDFM = m1$fcst_x, log = T, diff = T, carnaval = carnaval, pim_cajuste = pim_cajuste, alpha = 0, h = 48)

# ts.plot(na.omit(cbind(m_out$out[,"previsao_inter"], pim_inter)), col = 2:1, main = "year over year")
# ts.plot(na.omit(cbind(m_out$out[,"previsao"], pim)), col = 2:1, main = "nível")
# ts.plot(na.omit(cbind(m_out$out[,"previsao_margem"], pim_margem)), col = 2:1, main = "margem")

# exportar resultados ---------------------------------------------------------------
resul_out <- cbind(m$prev_nivel,m$prev_nivel_cajuste, m$prev_margem, m$prev_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste", "margem","inter")

resul_in <- m_out$out
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/06.lasso/ridge_model.rds")

message("RIDGE OK")
resultadosRIDGE <- resultados
rm(m, m1,m_out, resul_out, resul_in, resultados)