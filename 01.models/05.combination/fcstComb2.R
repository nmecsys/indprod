
# NIVEL -------------------------------------
nivel_sajuste <- cbind(resultadosLM$IN[,"nivel_sajuste"],resultadosLM_ipea$IN[,"nivel_sajuste"],resultadosLASSO$IN[,"nivel_sajuste"],resultadosRIDGE$IN[,"nivel_sajuste"],resultadosRF$IN[,"nivel_sajuste"])
nivel_cajuste <- cbind(resultadosLM$IN[,"nivel_cajuste"],resultadosLM_ipea$IN[,"nivel_cajuste"],resultadosLASSO$IN[,"nivel_cajuste"],resultadosRIDGE$IN[,"nivel_cajuste"],resultadosRF$IN[,"nivel_cajuste"])
colnames(nivel_sajuste) = colnames(nivel_cajuste) <- c("LM","LM_ipea","LASSO","RIDGE","RF")

comb_sajuste <- pimComb(pim, nivel_sajuste)
comb_cajuste <- pimComb(pim_cajuste, nivel_cajuste)

previsoes_nivel <- na.omit(cbind(pim, nivel_sajuste, comb_sajuste$combinations))
colnames(previsoes_nivel) <- c("PIM",colnames(nivel_sajuste),"MEAN","MEDIAN","RMSE","OLS")
ts.plot(previsoes_nivel[,c("PIM", "MEAN","MEDIAN","RMSE","OLS")], col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2))
legend("top", legend = c("PIM", "MEAN","MEDIAN","RMSE","OLS"), col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2), bty = "n", cex = 0.9)

previsoes_nivel_cajuste <- na.omit(cbind(pim_cajuste, nivel_cajuste, comb_cajuste$combinations))
colnames(previsoes_nivel_cajuste) <-c("PIM",colnames(nivel_cajuste),"MEAN","MEDIAN","RMSE","OLS")
ts.plot(previsoes_nivel_cajuste[,c("PIM","MEAN","MEDIAN","RMSE","OLS")], col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2))
legend("top", legend = c("PIM", "MEAN","MEDIAN","RMSE","OLS"), col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2), bty = "n", cex = 0.9)

# MARGEM -------------------------------------
margem <- cbind(resultadosLM$IN[,"margem"],resultadosLM_ipea$IN[,"margem"],resultadosLASSO$IN[,"margem"],resultadosRIDGE$IN[,"margem"],resultadosRF$IN[,"margem"])
colnames(margem) <- c("LM","LM_ipea","LASSO","RIDGE","RF")
comb_margem <- pimComb(pim_margem, margem)

previsoes_margem <- na.omit(cbind(pim_margem, margem, comb_margem$combinations))
colnames(previsoes_margem) <- c("PIM",colnames(margem),"MEAN","MEDIAN","RMSE","OLS")
ts.plot(na.omit(previsoes_margem)[,c("PIM", "MEAN","MEDIAN","RMSE","OLS")], col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2))
legend("top", legend = c("PIM", "MEAN","MEDIAN","RMSE","OLS"), col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2), bty = "n", cex = 0.9)

# INTER --------------------------------------
inter <- cbind(resultadosLM$IN[,"inter"],resultadosLM_ipea$IN[,"inter"],resultadosLASSO$IN[,"inter"],resultadosRIDGE$IN[,"inter"],resultadosRF$IN[,"inter"])
colnames(inter) <- c("LM","LM_ipea","LASSO","RIDGE","RF")
comb_inter <- pimComb(pim_inter, inter)

previsoes_inter <- cbind(pim_inter,inter, comb_inter$combinations)
colnames(previsoes_inter) <- c("PIM",colnames(inter),"MEAN","MEDIAN","RMSE","OLS")
ts.plot(na.omit(previsoes_inter)[,c("PIM", "MEAN","MEDIAN","RMSE","OLS")], col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2))
legend("top", legend = c("PIM", "MEAN","MEDIAN","RMSE","OLS"), col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2), bty = "n", cex = 0.9)

# tabela de erros - combinação --------------------------------

erros_rmse <- matrix(NA, ncol = 4, nrow = 9)
erros_mape <- matrix(NA, ncol = 4, nrow = 9)
colnames(erros_rmse) = colnames(erros_mape) <- c("nivel","nivel_cajuste","margem","inter")
rownames(erros_rmse) = rownames(erros_mape) <- c("LM","LM_ipea","LASSO","RIDGE","RF","MEAN","MEDIAN","RMSE","OLS")

for(i in rownames(erros_rmse)){
  erros_rmse[i,1] <- RMSE(previsoes_nivel[,1], previsoes_nivel[,i])
  erros_rmse[i,2] <- RMSE(previsoes_nivel_cajuste[,1], previsoes_nivel_cajuste[,i])
  erros_rmse[i,3] <- RMSE(previsoes_margem[,1], previsoes_margem[,i])
  erros_rmse[i,4] <- RMSE(previsoes_inter[,1], previsoes_inter[,i])
}

for(i in rownames(erros_mape)){
  erros_mape[i,1] <- MAPE(previsoes_nivel[,1], previsoes_nivel[,i])
  erros_mape[i,2] <- MAPE(previsoes_nivel_cajuste[,1], previsoes_nivel_cajuste[,i])
}

round(erros_mape,3)
round(erros_rmse,3)

saveRDS(round(erros_mape,3), "01.models/05.combination/erros_mapeNOVO.rds")
saveRDS(round(erros_rmse,3),"01.models/05.combination/erros_rmseNOVO.rds")


dygraph(na.omit(previsoes_nivel[,c("PIM","MEAN","MEDIAN","RMSE","OLS")])) %>%
  dyOptions(colors = brewer.pal(5,"Set1")) %>%
  dySeries("PIM",strokeWidth = 2) %>%
  dySeries("MEAN", strokeWidth = 1) %>%
  dySeries("MEDIAN",strokeWidth = 1, strokePattern = "dotted") %>%
  dySeries("RMSE",strokeWidth = 2, strokePattern = "dashed")  %>%
  dySeries("OLS",strokeWidth = 2, strokePattern = "dotted")

dygraph(na.omit(previsoes_margem[,c("PIM","MEAN","MEDIAN","RMSE","OLS")])) %>%
  dyOptions(colors = brewer.pal(5,"Set1")) %>%
  dySeries("PIM",strokeWidth = 2) %>%
  dySeries("MEAN", strokeWidth = 1) %>%
  dySeries("MEDIAN",strokeWidth = 1, strokePattern = "dotted") %>%
  dySeries("RMSE",strokeWidth = 2, strokePattern = "dashed")  %>%
  dySeries("OLS",strokeWidth = 2, strokePattern = "dotted")

dygraph(na.omit(previsoes_inter[,c("PIM","MEAN","MEDIAN","RMSE","OLS")])) %>%
  dyOptions(colors = brewer.pal(5,"Set1")) %>%
  dySeries("PIM",strokeWidth = 2) %>%
  dySeries("MEAN", strokeWidth = 1) %>%
  dySeries("MEDIAN",strokeWidth = 1, strokePattern = "dotted") %>%
  dySeries("RMSE",strokeWidth = 2, strokePattern = "dashed")  %>%
  dySeries("OLS",strokeWidth = 2, strokePattern = "dotted")

# NOVA PREVISÃO PIM ---------------------------
prev_nivel_sajuste <- ts(resultadosSARIMA$OUT[1]*comb_sajuste$weights$ols[1] + resultadosKF_univ$OUT[1]*comb_sajuste$weights$ols[2] + resultadosKF_mult$OUT[1]*comb_sajuste$weights$ols[3] + resultadosLM$OUT[1]*comb_sajuste$weights$ols[4] + resultadosLM_ipea$OUT[1]*comb_sajuste$weights$ols[5] + 
                           resultadosLASSO$OUT[1]*comb_sajuste$weights$ols[6] + resultadosRIDGE$OUT[1]*comb_sajuste$weights$ols[7] + resultadosRegTree$OUT[1]*comb_sajuste$weights$ols[8] + resultadosRF$OUT[1]*comb_sajuste$weights$ols[9], 
                  start = start(resultadosSARIMA$OUT), freq = 12)

prev_nivel_cajuste <- ts(resultadosSARIMA$OUT[2]*comb_cajuste$weights$ols[1] + resultadosKF_univ$OUT[2]*comb_cajuste$weights$ols[2] + resultadosKF_mult$OUT[2]*comb_cajuste$weights$ols[3] + resultadosLM$OUT[2]*comb_cajuste$weights$ols[4] + resultadosLM_ipea$OUT[2]*comb_cajuste$weights$ols[5] + 
                           resultadosLASSO$OUT[2]*comb_cajuste$weights$ols[6] + resultadosRIDGE$OUT[2]*comb_cajuste$weights$ols[7] + resultadosRegTree$OUT[2]*comb_cajuste$weights$ols[8] + resultadosRF$OUT[2]*comb_cajuste$weights$ols[9],
                 start = start(resultadosSARIMA$OUT), freq = 12)

prev_margem <- ts(resultadosSARIMA$OUT[3]*comb_margem$weights$ols[1] + resultadosKF_univ$OUT[3]*comb_margem$weights$ols[2] + resultadosKF_mult$OUT[3]*comb_margem$weights$ols[3] + resultadosLM$OUT[3]*comb_margem$weights$ols[4] + resultadosLM_ipea$OUT[3]*comb_margem$weights$ols[5] +
                    resultadosLASSO$OUT[3]*comb_margem$weights$ols[6] + resultadosRIDGE$OUT[3]*comb_margem$weights$ols[7] + resultadosRegTree$OUT[3]*comb_margem$weights$ols[8] + resultadosRF$OUT[3]*comb_margem$weights$ols[9],
                  start = start(resultadosSARIMA$OUT), freq = 12)

prev_inter <- ts(resultadosSARIMA$OUT[4]*comb_inter$weights$ols[1] + resultadosKF_univ$OUT[4]*comb_inter$weights$ols[2] + resultadosKF_mult$OUT[4]*comb_inter$weights$ols[3] + resultadosLM$OUT[4]*comb_inter$weights$ols[4] + resultadosLM_ipea$OUT[4]*comb_inter$weights$ols[5] + 
                   resultadosLASSO$OUT[4]*comb_inter$weights$ols[6] + resultadosRIDGE$OUT[4]*comb_inter$weights$ols[7] + resultadosRegTree$OUT[4]*comb_inter$weights$ols[8] + resultadosRF$OUT[4]*comb_inter$weights$ols[9],
                  start = start(resultadosSARIMA$OUT), freq = 12)


prev_individuais <- data.frame(matrix(c(resultadosSARIMA$OUT, resultadosKF_univ$OUT, resultadosKF_mult$OUT, resultadosLM$OUT, resultadosLM_ipea$OUT,
                             resultadosLASSO$OUT, resultadosRIDGE$OUT, resultadosRegTree$OUT, 
                             resultadosRF$OUT, prev_nivel_sajuste, prev_nivel_cajuste, prev_margem, prev_inter), byrow = T, ncol = 4))

colnames(prev_individuais) <- c("nivel_sajuste","nivel_cajuste","margem","inter")
rownames(prev_individuais) <- c(colnames(nivel_sajuste),"Combination")
saveRDS(prev_individuais, "01.models/05.combination/prev_individuais.rds")

resultados <- list(OUT = cbind(nivel_sajuste = prev_nivel_sajuste, nivel_cajuste = prev_nivel_cajuste, 
                               margem = prev_margem, inter = prev_inter),
                   IN = list(nivel_sajuste = na.omit(previsoes_nivel),
                             nivel_cajuste = na.omit(previsoes_nivel_cajuste),
                             margem = na.omit(previsoes_margem),
                             inter = na.omit(previsoes_inter))
)
saveRDS(resultados, "01.models/05.combination/comb_model.rds")

# Comparar com mercado ---------------------------
mercado <- read.csv2("00.data/Mediana_ecos_pim.csv")
mercado <- ts(mercado[nrow(mercado):1,2:3], start = c(2001,6), freq = 12)
colnames(mercado) <- c("margem","inter")
tudo <- window(cbind(pim_margem, pim_inter, mercado*100, previsoes_margem[,"OLS"],previsoes_inter[,"OLS"]), start = c(2015,10), freq = 12)
colnames(tudo) <- c("PIM_MARGEM","PIM_INTER","MERCADO_MARGEM","MERCADO_INTER","NMEC_MARGEM","NMEC_INTER")
dygraph(tudo[,c(1,3,5)]) %>%
  dySeries("PIM_MARGEM", strokePattern = "dotted",strokeWidth = 2, color = "black") %>%
  dySeries("MERCADO_MARGEM",  color = "orangered") %>%
  dySeries("NMEC_MARGEM", strokeWidth = 2, color = "deepskyblue")

erros_mercado <- matrix(c(RMSE(tudo[,1], tudo[,3]),
                          RMSE(tudo[,1], tudo[,5]),
                          RMSE(tudo[,2], tudo[,4]),
                          RMSE(tudo[,2], tudo[,6])),
                        ncol = 2, nrow = 2, byrow = T)

colnames(erros_mercado) <- c("MERCADO","NMEC")
rownames(erros_mercado) <- c("MARGEM","INTER")

# GRÁFICOS PREVISÃO DENTRO DA AMOSTRA
# # 10 x 5 pdf
# 
# 
# 
# 

# 
# 
# # mat = matrix(c(1,2,3), ncol = 1)
# # layout(mat)
# 
# # Gráfico 1 - SARIMA, VAR e DFM - Nível
# plot(as.vector(previsoes_nivel[,1]), xaxt = "n", bty = "l", lty = 3, type = "l", col = "white", ylim = c(80,90), ylab = "Nível", xlab = "")
# axis(1, at = seq(1,24,3), labels = substr(as.Date(previsoes_nivel)[seq(1,24,3)],1,7))
# abline(h = seq(80,90,2), v =  seq(1,24,3), col = "lightgrey", lty = 3)
# lines(as.vector(previsoes_nivel[,"PIM"]), type = "l", col = 1, lty = 1, lwd = 2)
# lines(as.vector(previsoes_nivel[,"SARIMA"]), type = "l", col = "dodgerblue", lty = 2)
# lines(as.vector(previsoes_nivel[,"VAR"]), type = "l", col = "purple", lty = 3)
# lines(as.vector(previsoes_nivel[,"NOW"]), type = "l", col = "orangered", lty = 1)
# legend("top", legend = c("PIM", "SARIMA","VAR","DFM"), col = c(1,"dodgerblue","purple","orangered"), lty = c(1,2,3,1), lwd = c(2,1,1,1), bty = "n", cex = 0.9)
# 
# # Gráfico 2 - SARIMA, VAR e DFM - Margem
# plot(as.vector(previsoes_margem[,1]), xaxt = "n", bty = "l", lty = 3, type = "l", col = "white", ylim = c(-6.5,2.5), ylab = "Variação Percentual (%)", xlab = "")
# axis(1, at = seq(1,24,3), labels = substr(as.Date(previsoes_margem)[seq(1,24,3)],1,7))
# abline(h = seq(-6,2,2), v =  seq(1,24,3), col = "lightgrey", lty = 3)
# lines(as.vector(previsoes_margem[,"PIM"]), type = "l", col = 1, lty = 1, lwd = 2)
# lines(as.vector(previsoes_margem[,"SARIMA"]), type = "l", col = "dodgerblue", lty = 2)
# lines(as.vector(previsoes_margem[,"VAR"]), type = "l", col = "purple", lty = 3)
# lines(as.vector(previsoes_margem[,"NOW"]), type = "l", col = "orangered", lty = 1)
# legend("bottom", legend = c("PIM", "SARIMA","VAR","DFM"), col = c(1,"dodgerblue","purple","orangered"), lty = c(1,2,3,1), lwd = c(2,1,1,1), bty = "n", cex = 0.9)
# 
# # Gráfico 3 - SARIMA, VAR e DFM - Inter
# plot(as.vector(previsoes_inter[,1]), xaxt = "n", bty = "l", lty = 3, type = "l", col = "white", ylim = c(-14,6), ylab = "Variação Percentual", xlab = "")
# axis(1, at = seq(1,24,3), labels = substr(as.Date(previsoes_inter)[seq(1,24,3)],1,7))
# abline(h = seq(-10,5,5), v =  seq(1,24,3), col = "lightgrey", lty = 3)
# lines(as.vector(previsoes_inter[,"PIM"]), type = "l", col = 1, lty = 1, lwd = 2)
# lines(as.vector(previsoes_inter[,"SARIMA"]), type = "l", col = "dodgerblue", lty = 2)
# lines(as.vector(previsoes_inter[,"VAR"]), type = "l", col = "purple", lty = 3)
# lines(as.vector(previsoes_inter[,"NOW"]), type = "l", col = "orangered", lty = 1)
# legend("top", legend = c("PIM", "SARIMA","VAR","DFM"), col = c(1,"dodgerblue","purple","orangered"), lty = c(1,2,3,1), lwd = c(2,1,1,1), bty = "n", cex = 0.9)
# 
# mat = matrix(c(1,2,3), ncol = 1)
# layout(mat)
# 
# # Gráfico 4 - Combinações - Nível
# plot(as.vector(previsoes_nivel[,1]), xaxt = "n", bty = "l", lty = 3, type = "l", col = "white", ylim = c(82,90), xlab = "", ylab = "Nível")
# axis(1, at = seq(1,24,3), labels = substr(as.Date(previsoes_nivel)[seq(1,24,3)],1,7))
# abline(h = seq(80,90,2), v =  seq(1,24,3), col = "lightgrey", lty = 3)
# lines(as.vector(previsoes_nivel[,"PIM"]), type = "l", col = 1, lty = 1, lwd = 2)
# lines(as.vector(previsoes_nivel[,"MEAN"]), type = "l", col = "dodgerblue", lty = 2)
# lines(as.vector(previsoes_nivel[,"MEDIAN"]), type = "l", col = "purple", lty = 3)
# lines(as.vector(previsoes_nivel[,"RMSE"]), type = "l", col = "orangered", lty = 1)
# lines(as.vector(previsoes_nivel[,"OLS"]), type = "l", col = "#659D32", lty = 2, lwd = 2)
# legend("top", legend = c("PIM", "Média","Mediana","RMSE","MQO"), col = c(1,"dodgerblue","purple","orangered","#659D32"), lty = c(1,2,3,1,2), lwd = c(2,1,1,1,2), bty = "n", cex = 0.9)
# 
# # Gráfico 5 - Combinações - Margem
# plot(as.vector(previsoes_margem[,1]), xaxt = "n", bty = "l", lty = 3, type = "l", col = "white", ylim = c(-4,3), xlab = "", ylab = "Variação Percentual (%)")
# axis(1, at = seq(1,24,3), labels = substr(as.Date(previsoes_margem)[seq(1,24,3)],1,7))
# abline(h = -4:3, v =  seq(1,24,3), col = "lightgrey", lty = 3)
# lines(as.vector(previsoes_margem[,"PIM"]), type = "l", col = 1, lty = 1, lwd = 2)
# lines(as.vector(previsoes_margem[,"MEAN"]), type = "l", col = "dodgerblue", lty = 2)
# lines(as.vector(previsoes_margem[,"MEDIAN"]), type = "l", col = "purple", lty = 3)
# lines(as.vector(previsoes_margem[,"RMSE"]), type = "l", col = "orangered", lty = 1)
# lines(as.vector(previsoes_margem[,"OLS"]), type = "l", col = "#659D32", lty = 2, lwd = 2)
# legend("topleft", legend = c("PIM", "Média","Mediana","RMSE","MQO"), col = c(1,"dodgerblue","purple","orangered","#659D32"), lty = c(1,2,3,1,2), lwd = c(2,1,1,1,2), bty = "n", cex = 0.9)
# 
# # Gráfico 6 - Combinações - Margem
# plot(as.vector(previsoes_inter[,1]), xaxt = "n", bty = "l", lty = 3, type = "l", col = "white", ylim = c(-15,5), xlab = "", ylab = "Variação Percentual (%)")
# axis(1, at = seq(1,24,3), labels = substr(as.Date(previsoes_inter)[seq(1,24,3)],1,7))
# abline(h = seq(-15,5,5), v =  seq(1,24,3), col = "lightgrey", lty = 3)
# lines(as.vector(previsoes_inter[,"PIM"]), type = "l", col = 1, lty = 1, lwd = 2)
# lines(as.vector(previsoes_inter[,"MEAN"]), type = "l", col = "dodgerblue", lty = 2)
# lines(as.vector(previsoes_inter[,"MEDIAN"]), type = "l", col = "purple", lty = 3)
# lines(as.vector(previsoes_inter[,"RMSE"]), type = "l", col = "orangered", lty = 1)
# lines(as.vector(previsoes_inter[,"OLS"]), type = "l", col = "#659D32", lty = 2, lwd = 2)
# legend("topleft", legend = c("PIM", "Média","Mediana","RMSE","MQO"), col = c(1,"dodgerblue","purple","orangered","#659D32"), lty = c(1,2,3,1,2), lwd = c(2,1,1,1,2), bty = "n", cex = 0.9)
# 
# ts.plot(previsoes_nivel[,-c(2:4)], col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2))
# legend("top", legend = c("PIM", "MEAN","MEDIAN","RMSE","OLS"), col = c(1,brewer.pal(4, "Set1")), lty = c(3,1,1,1,1), lwd = c(1,2,2,2,2), bty = "n", cex = 0.9)
# 






# ts.plot(nivel[,"PIM"], resultados2$pim_nivel, nivel[,2:4], col = c(1,"orangered","deepskyblue","forestgreen","black"), lty = c(3,1,1,1,1), lwd = c(1,1,2,3,4))
# legend("top", legend = c("PIM", "COMB","SARIMA","VAR","NOW"), col = c(1,"orangered","deepskyblue","forestgreen","black"), lty = c(3,1,1,1,1), lwd = c(1,1,2,3,4), bty = "n", cex = 0.9)
# 
# RMSE(nivel[,"PIM"],nivel[,"NOW"])
# RMSE(nivel[,"PIM"],resultados2$pim_nivel)
# 
# ts.plot(margem[,"PIM"], resultados2$pim_margem, margem[,2:4], col = c(1,"orangered","deepskyblue","forestgreen","black"), lty = c(3,1,1,1,1), lwd = c(1,1,2,3,4))
# legend("top", legend = c("PIM", "COMB","SARIMA","VAR","NOW"), col = c(1,"orangered","deepskyblue","forestgreen","black"), lty = c(3,1,1,1,1), lwd = c(1,1,2,3,4), bty = "n", cex = 0.9)
# 
# RMSE(margem[,"PIM"],margem[,"NOW"])
# RMSE(margem[,"PIM"],resultados2$pim_margem)
# 
# ts.plot(inter[,"PIM"], resultados2$pim_inter, inter[,2:4], col = c(1,"orangered","deepskyblue","forestgreen","black"), lty = c(3,1,1,1,1), lwd = c(1,1,2,3,4))
# legend("top", legend = c("PIM", "COMB","SARIMA","VAR","NOW"), col = c(1,"orangered","deepskyblue","forestgreen","black"), lty = c(3,1,1,1,1), lwd = c(1,1,2,3,4), bty = "n", cex = 0.9)
# 
# RMSE(inter[,"PIM"],inter[,"NOW"])
# RMSE(inter[,"PIM"],resultados2$pim_inter)
