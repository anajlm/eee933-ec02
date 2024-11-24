rm(list = ls()) 
#dev.off()
library(car)
library(ggplot2)

par(cex = 1.5) 

# Carregar os dados e reordena os dados cronologicamente:
dados <- read.csv("DadosAcoesGrupoH.csv")
colnames(dados) <- paste0('Acao_',1:5)
dados <- dados[nrow(dados):1, ]

# Dataframe dos retornos
retornos <- data.frame(matrix(NA, nrow = nrow(dados)-1, ncol = ncol(dados)))
colnames(retornos) <- colnames(dados)

# Calcular os retornos: (P[i+1] - P[i]) / P[i])
for (j in 1:ncol(dados)) {
  for (i in 2:nrow(dados)) {
    retornos[i-1,j] <- (dados[i,j] - dados[i-1,j]) / dados[i-1,j]
  }
}

retornos$Mes <- 1:nrow(retornos)

# VISUALIZAÇÃO #################################################################
# Retornos mensais:
dados_dataframe <- data.frame(
  Mes = rep(retornos$Mes, ncol(retornos) - 1),  # Meses
  Acao = rep(colnames(retornos)[-ncol(retornos)], each = nrow(retornos)), 
  Retorno = as.vector(as.matrix(retornos[, -ncol(retornos)])) 
)
# Visualização dos retornos, distribuição:
png("graficos/retorno_dist", width = 800, height = 600)
retorno_dist <- ggplot(dados_dataframe, aes(x = Acao,
                                            y = Retorno,
                                            fill = Acao)) + geom_boxplot() + geom_point() +
  ggtitle("Retornos Mensais por Ação", "Boxplots") + theme(legend.position = "none")
print(retorno_dist)
dev.off()

# Visualização dos retornos, serie temporal:
png("graficos/retorno_mensal", width = 800, height = 400)
plot(NULL, xlim = c(1, nrow(retornos)), ylim = range(dados_dataframe$Retorno, na.rm = TRUE),
     xlab = "Tempo (Meses)", ylab = "Retorno Mensal", main = "Retornos Mensais por Ação")
grid(col = "gray", lty = "dotted", lwd = 0.75)

cores_acoes <- rainbow(length(unique(dados_dataframe$Acao)))
acoes <- unique(dados_dataframe$Acao)

for (i in 1:length(acoes)) {
  linhas_acao <- dados_dataframe[dados_dataframe$Acao == acoes[i], ]
  lines(linhas_acao$Mes, linhas_acao$Retorno, col = cores_acoes[i], type = "o", pch = 16)
}
legend("topright", legend = acoes, col = cores_acoes, lty = 1, pch = 16, title = "Ações")
dev.off()

# Retornos Acumulados:
retornos_acumulados <- retornos[, -ncol(retornos)] 
for (j in 1:ncol(retornos_acumulados)) {
  retornos_acumulados[, j] <- cumprod(1 + retornos[, j]) - 1 
}

retornos_acumulados$Mes <- 1:nrow(retornos_acumulados)
dados_dataframe_acumulados <- data.frame(
  Mes = rep(retornos_acumulados$Mes, ncol(retornos_acumulados) - 1),  # Meses
  Acao = rep(colnames(retornos_acumulados)[-ncol(retornos_acumulados)], each = nrow(retornos_acumulados)),
  RetornoAcumulado = as.vector(as.matrix(retornos_acumulados[, -ncol(retornos_acumulados)]))
)
png("graficos/retorno_acumulado", width = 800, height = 400)
plot(NULL, xlim = c(1, nrow(retornos_acumulados)), ylim = range(dados_dataframe_acumulados$RetornoAcumulado, na.rm = TRUE),
     xlab = "Tempo (Meses)", ylab = "Retorno Acumulado", main = "Retorno Acumulado por Ação")
grid(col = "gray", lty = "dotted", lwd = 0.75)

cores_acoes_acumuladas <- rainbow(length(unique(dados_dataframe_acumulados$Acao))) 
acoes_acumuladas <- unique(dados_dataframe_acumulados$Acao)

for (i in 1:length(acoes_acumuladas)) {
  linhas_acao <- dados_dataframe_acumulados[dados_dataframe_acumulados$Acao == acoes_acumuladas[i], ]
  lines(linhas_acao$Mes, linhas_acao$RetornoAcumulado, col = cores_acoes_acumuladas[i], type = "o", pch = 16)
}

legend("topright", legend = acoes_acumuladas, col = cores_acoes_acumuladas, lty = 1, pch = 16, title = "Ações")
dev.off()

# TESTES E HIPOTESES ESTATISTICAS ##############################################
# Teste de Normalidade dos dados:

normalidade <- list()

for (acao in colnames(retornos)[-ncol(retornos)]) { 
  shapiro_test <- shapiro.test(retornos[[acao]]) 
  normalidade[[acao]] <- shapiro_test
  print(paste("Teste de Shapiro-Wilk, ação:", acao))
  print(shapiro_test)
}

# MODELO ANOVA ################################################################
anova <- aov(Retorno ~ Acao, data = dados_dataframe)
summary(anova)

# Teste de normalidade dos residuos:
shapiro_test <- shapiro.test(anova$residuals)
print(paste("Teste de Fligner, p-valor:", shapiro_test$p.value))
" o teste de Shapiro nos retornou um p-value de 0.933, significando que a distribuição dos residuos
 é normal."
png("graficos/qqPlot_anova_residuos", width = 800, height = 400)
qqPlot(anova$residuals, pch = 16, lwd = 3, cex = 2, las = 1,
       xlab = "Quantis", ylab = "Resíduos")
dev.off()

# Teste de Homogeniedade da variancia dos residuos:
fligner_test <- fligner.test(Retorno ~ Acao, data = dados_dataframe)
print(paste("Teste de Fligner, p-valor:", fligner_test$p.value))
" o teste de Flignern nos retornou um p-value de 0.026, significando que não existe homocedasticidade
 na varianção entre os 5 grupos de ação."

# Teste de idependencia:
durbinWatson_test <- durbinWatsonTest(anova)
print(paste("Teste de Fligner, p-valor:", durbinWatson_test$p))
" o teste de Durbin Watson nos retornou um p-value de 0.11, significando que não existe autocorrelação
entre os residuos."

png("graficos/residos_ordem", width = 800, height = 400)
plot(x = seq_along(anova$residuals), y = anova$residuals,
     type = "l", las = 1, lwd = 2, lty= 1,
     xlab = "Residual order", ylab = "Residual value", 
     main = "Idependencia entre Residos")
points(x    = seq_along(anova$residuals),
        y    = anova$residuals,
        type = "p",
        cex  = 2,
        pch  = 16,
        col  = as.numeric(dados_dataframe$Retorno))
 grid(NA,NULL, lwd=2, col = "#44444422")
dev.off()

# CHECANDO QUAL MELHOR AÇÃO PARA SE INVESTIR 2 A 2 ###################################################
# Tukey test:
TukeyHSD_test = TukeyHSD(anova)

tukey_dataframe <- as.data.frame(TukeyHSD_test$Acao)
tukey_dataframe$comparacao <- rownames(tukey_dataframe)
png("graficos/comparacoes", width = 800, height = 400)

tukey_plot <- ggplot(tukey_dataframe, aes(x = comparacao, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  coord_flip() + labs(title = "Diferenças, Tukey HSD",
                      x = "Ação x Ação",
                      y = "Diferença de Médias") + theme_minimal()
print(tukey_plot)
dev.off()