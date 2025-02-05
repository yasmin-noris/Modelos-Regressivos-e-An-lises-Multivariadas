### Realizando a regressão linear no R: relações entre variáveis contínuas ###

#Criando dois conjuntos de dados 
idade<-c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,33,34,35,36,37,40,41,42,43)
altura<-c(110,105,120,135,110,115,135,103,134,126,160,124,165,158,160,170,181,175,165,165,159,178,165,168)

## Calcular o modelo para retirar os resíduos 
modelo<- lm (altura  ~ idade)
modelo #fornece o intercepto e a inclinação 

#Sumário dos resultados do modelo
summary(modelo) ## R2 é apresentado como “Adjusted R-squared

# Lineariedade 
plot(x = idade, y = altura, xlab = 'Idade', 
     ylab = 'Altura (cm)', pch = 19, main = 'Gráfico de Dispersão')
abline(modelo,lty=2)

#Teste para NORMALIDADE ( p > 0,05 dados normais)
shapiro.test(rstudent(modelo))   ##teste de shapiro wilk (normalidade)

# Homocedasticidade: os pontos devem se distribuir igualmente abaixo e acima da linha
plot(rstudent(modelo) ~ fitted(modelo), pch = 1, xlab="Valores Ajustados", ylab="Resíduos")
abline(h = 0, lty = 2)

dados<-data.frame(idade, altura)

library(ggplot2)
ggplot(dados, aes(idade, altura)) +
  geom_point() + 
  stat_smooth(method = lm)+
  theme_bw (base_size= 18) +
  labs (x= "Idade (anos)", y= "Altura (cm)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", label="Y = 105.870 + 1.71*x", x=15, y=200) 

## ou vc pode apresentar o R²

