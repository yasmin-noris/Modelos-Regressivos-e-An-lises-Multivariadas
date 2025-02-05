#####################Regress?o linear multipla
library(tidyverse)
library(datarium) #banco de dados
library(faraway)
library(car)
library(visreg)
#exemplo 1
data("marketing", package = "datarium") #carregando a planilha marketing 
head(marketing, 4) #visualizando as 4 primeiras linhas da planilha 

#construindo o modelo 
modelo <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(modelo)

modelo1  <- lm(sales ~ youtube + facebook, data = marketing,na.action = "na.fail")
summary(modelo1)

####pressuposto de normalidade 
#se > 0.05 ? dist. normal
shapiro.test(modelo1$residuals) #normalidade

#Linearidade e homocedasticidade
plot(fitted(modelo1),residuals(modelo1),xlab="ValoresAjustados",ylab="Residuos")
abline(h=0)

#multicolinearidade
vif(modelo)
vif(modelo1)
cor(marketing[,-4])
pairs(marketing[,-4])


#Corrigindo o problema dos pressupostos 
modelo2<- lm(sales ~ log(youtube+1) + log(facebook+1), data = marketing,na.action = "na.fail")
summary(modelo2)

shapiro.test(modelo2$residuals) #normalidade

#Linearidade e homocedasticidade
plot(fitted(modelo2),residuals(modelo2),xlab="ValoresAjustados",ylab="Res?duos")
abline(h=0)

#Conclusão: não podemos realizar regressões lineares multiplas...
# Usar um outro modelo com uma outra distribuicao (Ex. GLM, familia binomial negativa)

#exemplo 2
data(trees)
data("trees") #carregando a planilha trees 
head(trees) #visualizando as 4 primeiras linhas da planilha 
attach(trees)

#construindo o modelo 
modelo3 <- lm(Volume ~ Girth + Height, data=trees)
summary(modelo3)


####pressuposto de normalidade 
#se > 0.05 ? dist. normal
shapiro.test(modelo3$residuals) #normalidade

#Linearidade e homocedasticidade
plot(fitted(modelo3),residuals(modelo3),xlab="ValoresAjustados",ylab="Residuos")
abline(h=0)

#multicolinearidade
vif(modelo3)
cor(trees[,-3])
pairs(trees[,-3])

par(mfrow = c(1,2))
visreg(modelo3)



