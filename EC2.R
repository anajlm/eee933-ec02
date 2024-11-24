rm(list = ls()) 
#dev.off()
library(car)
library(ggplot2)

# Carregar os dados e reordena os dados cronologicamente:
dados <- read.csv("DadosAcoesGrupoH.csv")
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
ggplot(dados_dataframe, 
       aes(x    = Acao,
           y    = Retorno,
           fill = Acao)) + 
  geom_boxplot() + geom_point() + 
  ggtitle("Retornos Mensais por Ação",
          "Boxplots") + 
  theme(legend.position = "none")


# Visualização dos retornos, serie temporal:
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

# TESTES E HIPOTESES ESTATISTICAS ##############################################
# Teste de Normalidade dos dados:

normalidade <- list()

for (acao in colnames(retornos)[-ncol(retornos)]) { 
  shapiro_test <- shapiro.test(retornos[[acao]]) 
  normalidade[[acao]] <- shapiro_test
  print(paste("Teste de Shapiro-Wilk, ação:", acao))
  print(shapiro_test)
}

# Modelo ANOVA:
anova <- aov(Retorno ~ Acao, data = dados_dataframe)
summary(anova)

# Teste de normalidade dos residuos:
shapiro.test(anova$residuals)
qqPlot(anova$residuals, pch = 16, lwd = 3, cex = 2, las = 1,
       xlab = "Quantis", ylab = "Resíduos")

# Teste de Homogeniedade da variancia dos residuos:
fligner <- fligner.test(Retorno ~ Acao, data = dados_dataframe)
print(paste("Teste de Fligner, p-valor:", fligner$p.value))