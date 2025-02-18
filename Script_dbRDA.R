# Carregar pacotes necessários
install.packages("vegan")  # Para análises ecológicas
install.packages("ggplot2") # Para gráficos
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

# Passo 4: Realizar a PCoA (Análise de Coordenadas Principais)
pcoa <- cmdscale(distancia, k = nrow(comunidade) - 1, eig = TRUE)

# Extrair as coordenadas da PCoA
coordenadas_pcoa <- pcoa$points

# Passo 5: Aplicar a RDA sobre as coordenadas da PCoA
resultado_rda <- rda(coordenadas_pcoa ~ Temperatura + pH + Umidade, data = ambiente)

# Passo 6: Verificar os resultados
resultado_sumario <- summary(resultado_rda)

# Extrair a porcentagem de explicação de cada eixo
variancia_explicada <- round(100 * resultado_rda$CCA$eig / sum(resultado_rda$CCA$eig), 2)

# Passo 7: Preparar os dados para o gráfico
dados_grafico <- data.frame(
  Local = rownames(coordenadas_pcoa),
  RDA1 = resultado_rda$CCA$u[, 1],  # Eixo RDA1
  RDA2 = resultado_rda$CCA$u[, 2]   # Eixo RDA2
)

# Passo 8: Criar o gráfico com ggplot2
grafico <- ggplot(dados_grafico, aes(x = RDA1, y = RDA2)) +
  geom_point(size = 4) +  # Pontos representando os locais
  geom_text(aes(label = Local), vjust = 1.5, hjust = 1.5) +  # Rótulos dos locais
  geom_segment(data = as.data.frame(resultado_rda$CCA$biplot * 2),  # Vetores ambientais
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = as.data.frame(resultado_rda$CCA$biplot * 2),  # Rótulos dos vetores
            aes(x = RDA1, y = RDA2, label = rownames(resultado_rda$CCA$biplot)),
            color = "red", vjust = 1.5) +
  theme_minimal() +
  labs(title = "dbRDA - Análise de Redundância Baseada em Distância",
       x = paste0("RDA1 (", variancia_explicada[1], "%)"),  # Adicionar % de explicação do RDA1
       y = paste0("RDA2 (", variancia_explicada[2], "%)"))  # Adicionar % de explicação do RDA2

# Passo 9: Exibir o gráfico
print(grafico)
