
# REGRESSION TREE ----------------------------------------------------------------
# modelo dentro da amostra -------------------------------------------------------

m1 <- pimFactors(database$dados2, lagsy = NULL, lagsf = NULL, n.fatores = 1, p = 1, log = F,  diff = T,
                 carnaval = carnaval, pim_cajuste = pim_cajuste)

m <- pimRF(database$dados2, outDFM = m1$fcst_x, carnaval = carnaval, diff = T, type = "regression")

# exercício fora da amostra ------------------------------------------------------
m_out <-  pimRFFcst(database$dados2, outDFM = m1$fcst_x, carnaval = carnaval, pim_cajuste = pim_cajuste, diff = T, type = "regression", h = 48)

# exportar resultados ------------------------------------------------------------
resul_out <- cbind(m$prev_nivel,m$prev_nivel_cajuste, m$prev_margem, m$prev_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste", "margem","inter")

resul_in <- m_out$out
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/07.forest/regtree_model.rds")

resultadosRegTree <- resultados

# RANDOM FOREST ----------------------------------------------------------------
# modelo dentro da amostra -------------------------------------------------------
m <- pimRF(database$dados2, outDFM = m1$fcst_x, carnaval = carnaval, diff = T, type = "rf")

# exercício fora da amostra ------------------------------------------------------
m_out <-  pimRFFcst(database$dados2, outDFM = m1$fcst_x, carnaval = carnaval, pim_cajuste = pim_cajuste, diff = T, type = "rf", h = 48)

# exportar resultados ------------------------------------------------------------
resul_out <- cbind(m$prev_nivel,m$prev_nivel_cajuste, m$prev_margem, m$prev_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste", "margem","inter")

resul_in <- m_out$out
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/07.forest/rf_model.rds")

resultadosRF <- resultados
rm(m1, m, m_out, resul_out, resul_in, resultados)