#Correlação com exemplo do R
data(mtcars) #carregar a planilha do R
dados<-mtcars #Atribuir nome para o conjunto de dados carregados
head(dados) #Verificar as primeiras linhas da planilha

#Construindo um gr?fico de dispers?o 
library(ggplot2)
library(ggpubr)

ggscatter(dados, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

#PRESSUPOSTOS
#Linearidade: observar o gráfico de dispersão

#Normalidade para as duas variáveis (p>0,05)
# Shapiro-Wilk para mpg
shapiro.test(dados$mpg) # => p = 0.1229
ggqqplot(dados$mpg, ylab = "MPG")

# Shapiro-Wilk para wt
shapiro.test(dados$wt) # => p = 0.09
ggqqplot(dados$wt, ylab = "WT")

#nota: se a distribuição não for normal, recomenda-se usar Spearman ou Kendal (n pequeno)

#teste de correlação
res <- cor.test(dados$wt, dados$mpg, method = "pearson")
res



