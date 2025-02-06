################ MODELOS REGRESSIVOS E ANÁLISES MULTIVARIADAS #####################
## Modelos regressivos II (GLM - Anova, Ancova) ##
## Ministrante: Taise M. Lopes ##


####~~~~~~~~~~~~~~~~~~~~~~~~~~ ANOVA bifatorial ~~~~~~~~~~~~~~~~~~~~~~~~~~~####

#### Passo 1: Carregar os pacotes que ser?o usados ----

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(car)) install.packages("car")   
library(car)
if(!require(vegan)) install.packages("vegan")   
library(vegan)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)                                
if(!require(DescTools)) install.packages("DescTools") 
library(DescTools)
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(multcomp)) install.packages("multcomp") 
library(multcomp)

#### Passo 2: Carregar o banco de dados ----

# Importante: selecionar o diret?rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

setwd("C:/Users/taise/Arquivos/Cursos_Disciplinas_Ministrados/2025_Modelos regressivos e analises multivariadas_PEA")

dados <- read.csv2('dados/data_anova.csv')    # Carregamento do arquivo csv
View(dados)                             # Visualizacao dos dados em janela separada
head(dados)


## Visualizacao de um resumo dos dados
str(dados)                


## Formatar os dados
dados$alcool <- factor(dados$alcool,
                       levels = c("nenhum",
                                  "2canecas",
                                  "4canecas"))

dados$alcool <- as.factor(dados$alcool)

dados$genero <- as.factor(dados$genero)

dados$memoria = as.numeric(dados$memoria)

#VD = Memoria
#VI = Genero e Alcool


#### Passo 3: Verificao dos pressupostos nos residuos ----

## Construcao do modelo
modelo <- aov(memoria ~ genero * alcool, dados) 

mod <- lm(memoria ~ genero * alcool, dados)


## Teste de normalidade para os residuos:
shapiro.test(modelo$residuals)

shapiro.test(mod$residuals)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#H0: Os residuos apresentam distribuicao normal (p>0.05)
#H1: Os residuos nao apresentam distribuicao normal (p<0.05)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


## Verificacao da homogeneidade de variancias - teste de Levene (pacote car)
dados$residuos <- modelo$residuals

leveneTest(residuos ~ genero * alcool, dados, center = mean)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #H0: Os dados sao homogeneos (p>0.05)
  #H1: Os dados nao sao homogeneos (p<0.05)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


## Verifica?o dos pressupostos graficamente (funcao lm)
par(mfrow = c(2,2))
plot(mod)


#### Passo 4: Realizacao dos modelos ----

modelo <- aov(memoria ~ genero * alcool, dados)
Anova(modelo, type = 'III') #summary do modelo - robusto para design desbalancedo      


mod <- lm(memoria ~ genero * alcool, dados)
Anova(mod, type = "III")

summary(mod)


#### Passo 5: Analise do post-hoc (Pacote DescTools e multcomp) ----

## Uso do TukeyHSD para modelo 'funcao aov'
PostHocTest(modelo, method = "hsd")


## Uso do Tukey para modelo 'funcao lm'

posthoc <- glht(mod, linfct = mcp(alcool = "Tukey")) #para modelos sem interacao significativa
summary(posthoc)


TP <- interaction(dados$genero, dados$alcool) #para modelos com interacao significativa

mod_2 <- lm(memoria ~ TP, dados) 

posthoc <- glht(mod_2, linfct = mcp(TP = "Tukey"))
summary(posthoc)


#### Passo 6: Grafico de interacao (Pacote ggplot2) ----

## Com generos com cores diferentes
ggplot(dados, aes(x = alcool, y = memoria, group = genero, color = genero)) +
  geom_line(stat = "summary", fun.data = "mean_se", linewidth = 0.6) +
  geom_point(stat = "summary", fun.data = "mean_se") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  scale_y_continuous(breaks = seq(min(dados$memoria), max(dados$memoria), by = 5))


## Com generos com linhas diferentes
ggplot(dados, aes(x = alcool, y = memoria, group = genero)) +
  geom_line(stat = "summary", fun.data="mean_se", size = 0.6, aes(linetype = genero)) +
  geom_point(stat = "summary", fun.data = "mean_se", size = 2, aes(shape = genero)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  scale_y_continuous(breaks = seq(min(dados$memoria), max(dados$memoria), by = 5))


#### Passo 7: Analise descritiva dos dados - Pacote rstatix ----
dados %>%
  group_by(genero, alcool) %>%
  get_summary_stats(memoria, type = "mean_sd")



