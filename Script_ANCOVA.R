################ MODELOS REGRESSIVOS E ANÁLISES MULTIVARIADAS #####################
## Modelos regressivos II (GLM - Anova, Ancova) ##
## Ministrante: Taise M. Lopes ##


rm(list = ls())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ANCOVA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#### PASSO 1: Carregar os pacotes que ser?o usados ----

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                  
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)
if(!require(car)) install.packages("car")   
library(car)             
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(multcomp)) install.packages("multcomp") 
library(multcomp)
if(!require(emmeans)) install.packages("emmeans")
library(emmeans)

require(openxlsx)

#### PASSO 2: Carregar o banco de dados ----

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

setwd("C:/Users/taise/Arquivos/Cursos_Disciplinas_Ministrados/2025_Modelos regressivos e analises multivariadas_PEA")

dados <- read.csv2("dados/data_ancova.csv")

dados <- read.xlsx("dados/data_ancova_v2.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
View(dados)
head(dados) 


str(dados)                             # Visualizacao de um resumo dos dados


## Formatar as variaveis

dados$grau_instrucao = as.factor(dados$grau_instrucao)
dados$idade = as.numeric(dados$idade)
dados$salario = as.numeric(dados$salario)

# VD: Salario
# VI: Grau de instrucao
# Covariavel: Idade


#### PASSO 3: Verificar se ha efeito da VI sobre a covariavel (cov ~ VI) ----

# Pressuposto: "independencia entre a VI e a covariavel"
# Se rompido: nao ha outro modelo - eh um problema de delineamento

mod_cov <- aov(idade ~ grau_instrucao, data = dados)
summary(mod_cov)


mod_cov1 <- lm(idade ~ grau_instrucao, data = dados)
summary(mod_cov1)
Anova(mod_cov1)


#------------------------------------------------------#
#H0: A VI e a covariavel sao independentes (p>0.05)
#H1: A VI e a covariavel sao dependentes (p<0.05)
#-----------------------------------------------------#


#### PASSO 4: Verificar se a relacao entre a covariavel e a VD eh linear (VD ~ cov) ----

ggplot(data = dados, aes(x = idade, y = salario, group = grau_instrucao,
                         color = grau_instrucao)) +
  geom_point(size = 2) +
  xlab('Idade') +
  ylab('Salario') +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5)



#### PASSO 5: Verificar se o efeito da covariavel eh o mesmo para todos niveis da VI (VD ~ VI*cov) ----

# Pressuposto: "homogeneidade dos parametros de regressao"
# Compara as inclinacoes das retas para cada grupo da VI

mod_int <- aov(salario ~ grau_instrucao*idade, data = dados)
summary(mod_int)


mod_int2 <- lm(salario ~ grau_instrucao*idade, data = dados)
Anova(mod_int2, type = "III")
summary(mod_int2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#H0: Os parametros do modelo sao homogeneos (p > 0.05)
#H1: Os parametros do modelo nao sao homogeneos (p < 0.05)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


##Caso a interacao seja significativa, usar um modelo linear.
##Nao precisar ajustar o modelo. Voce vai o modelo com interacao.


#### PASSO 6: Verificar se ha homogeneidade de variancias (VD ~ VI) ----

leveneTest(salario ~ grau_instrucao, center = mean, data = dados)


#--------------------------------------------#
#H0: Os dados sao homogeneos (p >0.05)
#H1: Os dados nao sao homogeneos (p<0.05)
#-------------------------------------------#


#### PASSO 7: Ajustar o modelo de ANCOVA (VD ~ cov + VI) ----

mod_ANCOVA <- aov(salario ~ idade + grau_instrucao, data = dados)
summary(mod_ANCOVA)
mod_ANCOVA$coefficients


mod <- lm(salario ~ idade + grau_instrucao, data = dados)
Anova(mod, type = 'III')
summary(mod)


#### PASSO 8: Verificar a normalidade dos residuos ----

hist(dados$Salario)

shapiro.test(mod_ANCOVA$residuals)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#H0: Os dados apresentam distribuicao normal (p > 0.05)
#H1: Os dados nao apresentam distribuicao normal (p < 0.05)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#### PASSO 9: Verificar se ha homocedasticidade e outliers ----

boxplot(mod_ANCOVA$residuals)

par(mfrow=c(2,2))
plot(mod_ANCOVA)

leveneTest(mod_ANCOVA$residuals ~ dados$grau_instrucao)

par(mfrow = c(2,2))
plot(mod)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#H0: Os dados sao homogeneos (p > 0.05)
#H1: Os dados nao sao homogeneos (p < 0.05)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#### PASSO 10: Realizacao das comparacoes entre grupos ----

## Pelo pacote multcomp
posthoc <- glht(mod_ANCOVA, linfct = mcp(grau_instrucao = "Tukey"))
summary(posthoc)


#### PASSO 11: Obtencaoo das medias ajustadas ----

medias_ajustadas <- emmeans(mod_ANCOVA, ~ idade:grau_instrucao)
medias_ajustadas


####### Medias reais
dados %>% group_by(grau_instrucao) %>% 
  get_summary_stats(salario, type = "mean_sd")


#### Grafico do modelo ----

# Simular os dados dentro da amplitude dos dados originais
idade = seq(min(dados$idade), max(dados$idade), 1)
grau_instrucao <- rep(unique(dados$grau_instrucao), length(idade))
grau_instrucao <- grau_instrucao[order(grau_instrucao)]

# Cria um data.frame com os dados de idade e grau de instrucao simulados
dados_est <- data.frame(idade = rep(idade, 3), grau_instrucao = grau_instrucao)

# Estimar o salario utilizando o modelo criado
dados_est$salario <- predict(mod, newdata = dados_est)

# Plot com as retas dos dados estimados
ggplot(data = dados_est) +
  geom_line(aes(x = idade, y = salario, colour = grau_instrucao), lwd = 1.2) +
  scale_y_continuous(breaks = seq(0, max(dados$salario), by = 1))

# Plotar as retas e os dados originais
ggplot(data = dados_est, aes(x = idade, y = salario, colour = grau_instrucao)) +
  geom_line(lwd = 1.2) +
  geom_point(data = dados, aes(x = idade, y = salario, colour = grau_instrucao)) +
  scale_y_continuous(breaks = seq(0, max(dados$salario), by = 1))

