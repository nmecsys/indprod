# modelo dentro da amostra ----------------------------------------------------------
m <- pimFactors(database$dados2[,-c(2,4,21,23,24)], lagsy = c(1,3,12,13), lagsf = c(1,12), n.fatores = 1, p = 1, log = F,  diff = T,
                carnaval = carnaval, pim_cajuste = pim_cajuste)

# exercício fora da amostra -----------------------------------------------------
m_out <- pimFactorsFcst(database$dados2[,-c(2,4,21,23,24)],
                        lagsy = c(1,3,12,13), lagsf = c(1,12),  n.fatores = 1, p = 1,
                         log = F, diff = T, carnaval = carnaval, pim_cajuste = pim_cajuste)

# exportar resultados --------------------------------------------

resul_out <- cbind(m$prev_nivel, m$prev_nivel_cajuste, m$prev_margem, m$prev_inter)
colnames(resul_out) <- c("nivel_sajuste","nivel_cajuste", "margem","inter")

resul_in <- m_out$out
colnames(resul_in) <- c("nivel_sajuste","nivel_cajuste","margem","inter")

resultados <- list(IN = resul_in, OUT = resul_out)
saveRDS(resultados, "01.models/01.dfm/dfm_model.rds")

resultadosDFM <- resultados
rm(m, m_out, resul_out, resul_in, resultados)