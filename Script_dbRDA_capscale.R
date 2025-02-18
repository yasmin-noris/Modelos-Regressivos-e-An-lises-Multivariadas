# Carregar pacotes necessários
library(vegan)
library(ggplot2)

# Passo 1: Criar a matriz de abundância de espécies (exemplo)
comunidade <- matrix(c(10, 5, 0, 2,
                       8, 7, 1, 3,
                       0, 12, 4, 6,
                       3, 9, 8, 1,
                       7, 2, 5, 4),
                     nrow = 5, byrow = TRUE,
                     dimnames = list(c("Local1", "Local2", "Local3", "Local4", "Local5"),
                                     c("Espécie_A", "Espécie_B", "Espécie_C", "Espécie_D")))

# Passo 2: Criar a matriz de variáveis ambientais (exemplo)
ambiente <- data.frame(
  Temperatura = c(25, 28, 22, 30, 26),
  pH = c(6.5, 7.0, 5.8, 7.2, 6.0),
  Umidade = c(70, 65, 80, 60, 75)
)

# Passo 3: Calcular a matriz de distância (Bray-Curtis)
distancia <- vegdist(comunidade, method = "bray")

# Passo 4: Realizar a dbRDA usando capscale
resultado_capscale <- capscale(distancia ~ Temperatura + pH + Umidade, data = ambiente)

# Passo 5: Verificar os resultados
resultado_sumario <- summary(resultado_capscale)

# Extrair a porcentagem de explicação de cada eixo
variancia_explicada <- round(100 * resultado_capscale$CCA$eig / sum(resultado_capscale$CCA$eig), 2)

# Passo 6: Preparar os dados para o gráfico
dados_grafico <- data.frame(
  Local = rownames(resultado_capscale$CCA$u),
  RDA1 = resultado_capscale$CCA$u[, 1],  # Eixo RDA1
  RDA2 = resultado_capscale$CCA$u[, 2]   # Eixo RDA2
)

# Preparar os dados dos vetores ambientais (biplot)
vetores_ambientais <- as.data.frame(resultado_capscale$CCA$biplot)
vetores_ambientais$variavel <- rownames(vetores_ambientais)  # Adicionar nomes das variáveis

# Renomear as colunas para RDA1 e RDA2
colnames(vetores_ambientais) <- c("RDA1", "RDA2", "variavel")

# Passo 7: Criar o gráfico com ggplot2
grafico <- ggplot(dados_grafico, aes(x = RDA1, y = RDA2)) +
  geom_point(size = 4) +  # Pontos representando os locais
  geom_text(aes(label = Local), vjust = 1.5, hjust = 1.5) +  # Rótulos dos locais
  geom_segment(data = vetores_ambientais,  # Vetores ambientais
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = vetores_ambientais,  # Rótulos dos vetores
            aes(x = RDA1, y = RDA2, label = variavel),
            color = "red", vjust = 1.5) +
  theme_minimal() +
  labs(title = "dbRDA - Análise de Redundância Baseada em Distância",
       x = paste0("RDA1 (", variancia_explicada[1], "%)"),  # Adicionar % de explicação do RDA1
       y = paste0("RDA2 (", variancia_explicada[2], "%)"))  # Adicionar % de explicação do RDA2

# Passo 8: Exibir o gráfico
print(grafico)
