#'Programa de Pos Graduaçao em Ecologia de Ambientes Aquaticos Continentais (PEA)
#'AMB-40: Topicos especiais: Modelos regressivos e Analises multivariadas
#'Script pratico de RDA e pRDA
#'Beatriz Melissa Campos (melissacampos1@hotmail.com)

#'CTRL + shift + H para chamar o diretório
#'Carregar os pacotes

library(vegan)
library (adespatial)
library(ggplot2)

#'Dados
especies<-read.delim("especies.txt", header = TRUE, dec = ".")
abioticos<-read.delim("abioticos.txt", header = TRUE, dec = ".")

#Transformação dos dados
esp.hel <- decostand (especies[, c(2:31)], method = "hellinger") #retirando a coluna categórica
esp.hel

#Verificar variáveis de diferentes naturezas
boxplot(abioticos[,-1]) 
abio.stand <- decostand (abioticos[, c(2:13)], method = "standardize") #retirando a coluna categórica
abio.stand

# Realizar a Análise de Redundância (RDA)
help(rda) #Página de ajuda para a função "rda"

#Contruir um modelo com apenas o intercepto e outro com todas variáveis 
rda.0<-rda(esp.hel~1,abio.stand) #Modelo de RDA somente com o intercepto, isto é, modelo sem nenhuma variável preditora
rda.1<-rda(esp.hel~.,abio.stand) #Modelo de RDA com todas as variáveis preditoras (RDA básica)

#'O modelo com intercepto serve como controle para testar se as variáveis explicativas realmente melhoram o modelo
#'Representa a variabilidade natural sem influência ambiental
#'Pode ser comparado ao modelo completo para avaliar se as variáveis realmente importam
#'
#'O modelo com todas as variáveis buscando explicar ao máximo a variação na matriz de resposta

# Visualizar o resumo da RDA
summary(rda.0)
summary(rda.1)

# Calcular o VIF (Fator de Inflação da Variância)
vif.cca(rda.1) #retirar variável < 5

#Refazendo o modelo retirando COD
rda.2<-rda(esp.hel~.,abio.stand [,-c(9)])

# Recalcular o VIF (Fator de Inflação da Variância)
vif.cca(rda.2) #retirar variável < 5

summary(rda.2)

# Extrair os eigenvalues (autovalores) da RDA
eigenvals <- eigenvals(rda.2)

# Calcular a proporção da variância explicada por cada eixo
var_explained <- eigenvals / sum(eigenvals) * 100

# Mostrar a porcentagem de explicação dos eixos
var_explained

# Teste de significância entre os modelos
anova_rda <- anova.cca(rda.2, rda.0, permutations = 999)
anova_rda
plot(rda.2)

#'o valor de p é 0.006, o modelo utilizando VIF (rda.2) 
#'explica significativamente mais variação na resposta (esp.hel) do que o modelo 
#'com apenas o intercepto (rda.0). Isso significa que as variáveis ambientais 
#'que incluídas são importantes para modelar a variação na resposta e devem 
#'ser mantidas no modelo.

#Interpretando o summary
#'ResDf: Graus de liberdade restantes para cada modelo.
#'ResChiSquare: A soma dos quadrados explicados para cada modelo.
#'Df: A diferença de graus de liberdade entre os modelos.
#'ChiSquare: A diferença na soma dos quadrados explicados entre os modelos.
#'F: O valor da estatística F, que mede a razão entre a variabilidade explicada
#' e a não explicada.
#'Pr(>F): O valor-p associado à estatística F, que indica a significância da 
#'diferença entre os modelos.

# Extrair os scores das amostras e das variáveis ambientais
scores_rda <- scores(rda.2, display = c("sites", "bp"))

# Criar um data frame com os scores das amostras
df_sites <- as.data.frame(scores_rda$sites)
df_sites$Sample <- rownames(df_sites)

# Criar um data frame com os scores das variáveis ambientais
df_bp <- as.data.frame(scores_rda$biplot)
df_bp$Variable <- rownames(df_bp)

# Verificar os nomes das colunas
print(colnames(df_sites)) # Verifique se as colunas são "RDA1", "RDA2", etc.
print(colnames(df_bp))    # Verifique se as colunas são "RDA1", "RDA2", etc.

# Renomear as colunas se necessário
colnames(df_sites) <- c("RDA1", "RDA2") # Ajuste conforme necessário
colnames(df_bp) <- c("RDA1", "RDA2", "Variável")    # Ajuste conforme necessário

# Criar o gráfico usando ggplot2
plot_RDA <- ggplot() +
  # Plotar as amostras
  geom_point(data = df_sites, aes(x = RDA1, y = RDA2), color = "purple", size = 2.5) +
  # Plotar as variáveis ambientais
  geom_segment(data = df_bp, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  # Adicionar labels para as variáveis ambientais
  geom_text(data = df_bp, aes(x = RDA1, y = RDA2, label = Variável), 
            color = "black", vjust = 1.5, hjust = 1.5) +
  # Adicionar labels para os eixos
  labs(x = paste("RDA1 (", round(var_explained[1], 2), "%)", sep = ""), 
    y = paste("RDA2 (", round(var_explained[2], 2), "%)", sep = ""), 
    title = "Análise de Redundância (RDA)") +
  # Adicionar tema
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = c(0.2, 0.2))

plot_RDA

plot_RDA+tiff("RDA.tiff",units = "in",width = 6,height = 4,res=600,compression = "lzw")
dev.off()
