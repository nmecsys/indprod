# univariate ----------------------

# previsão fora da amostra
m <- pimKF(pim = pim, type = "univariate", carnaval = carnaval, pim_cajuste = pim_cajuste)

# exercício fora da amostra
m_out <- pimKFFcst(pim = pim, type = "univariate", carnaval = carnaval, pim_cajuste = pim_cajuste, h = 48)

# RMSE(pim, m_out$out[,1])
# RMSE(pim_cajuste, m_out$out[,2])
# RMSE(pim_margem, m_out$out[,3])
# RMSE(pim_inter, m_out$out[,4])

# exportar resultados --------------------------------------------

resul_out <- cbind(m$prev_nivel,m$prev_nivel_cajuste, m$prev_margem, m$prev_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste", "margem","inter")

resul_in <- m_out$out
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/01.dfm/kfuniv_model.rds")

resultadosKF_univ <- resultados
rm(m, m_out, resul_out, resul_in, resultados)

# multivariate  ----------------------

# previsão fora da amostra
m <- pimKF(data = database$dados2, type = "multivariate", carnaval = carnaval, pim_cajuste = pim_cajuste)

# exercício fora da amostra
m_out <- pimKFFcst(data = database$dados2, type = "multivariate", carnaval = carnaval, pim_cajuste = pim_cajuste, h = 48)


# exportar resultados --------------------------------------------

resul_out <- cbind(m$prev_nivel,m$prev_nivel_cajuste, m$prev_margem, m$prev_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste", "margem","inter")

resul_in <- m_out$out
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/01.dfm/kfmult_model.rds")

resultadosKF_mult <- resultados
rm(m, m_out, resul_out, resul_in, resultados)