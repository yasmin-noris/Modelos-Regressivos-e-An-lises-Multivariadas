################ MODELOS REGRESSIVOS E ANÁLISES MULTIVARIADAS #####################
## Modelos regressivos II (GLM - Anova, Ancova) ##
## Ministrante: Taise M. Lopes ##


#~~~~~~~~~~~~~~~~~~~~~~ DISTRIBUICAO BINOMIAL NEGATIVA ~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#### Passo 1: Importar pacotes ----

if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(dplyr)) install.packages("dplyr")
library(dplyr) 
if(!require(car)) install.packages("car")   
library(car)
if(!require(MASS)) install.packages("MASS") 
library(MASS)
if(!require(multcomp)) install.packages("multcomp") 
library(multcomp)


#### Passo 2: Carregar banco de dados ----

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

getwd()

setwd("C:/Users/taise/Arquivos/Cursos_Disciplinas_Ministrados/2025_Modelos regressivos e analises multivariadas_PEA")


dados <- read.csv2("dados/data_nb.csv")
head(dados)

## Visualizar a estrutura dos dados
str(dados)

#VD = daysabs
#VI = prog e math

## Formatar os dados (quando formata n?o roda o ggplot)
dados$prog = factor(dados$prog, levels = c("general", "academic", "vocational"))
dados$daysabs = as.numeric(dados$daysabs)
dados$math = as.numeric(dados$math)


#### Passo 3: Descricao dos dados (pacote 'ggplot2' e 'dplyr') ----

summary(dados) 

ggplot(dados, aes(daysabs, fill = prog)) + 
  geom_histogram(binwidth = 1) + 
  facet_grid(prog ~ ., margins = TRUE, scales = "free") 



##media e variancia por grupo da variavel categorica

dados_sumarizados <- dados %>% 
  group_by(prog) %>% 
  summarise(media = mean(daysabs), 
            sd = sd(daysabs))

dados_sumarizados 

#variancia dentro de cada nivel maior que a media de cada nivel
#indicativo de superdispersao


#### Passo 4: verificao dos pressupostos do modelo (pacote 'MASS') ----

modelo1 <- glm(daysabs ~ math + prog, family = poisson, data = dados)
summary(modelo1)

#Overdispersion = > 1.2 ou <0.8
#Overdispersion = Residual deviance/df = 1774/310 = 5.72
#Dados de contagem com superdispersao = Distribuicao binomial negativa


modelo2 <- glm.nb(daysabs ~ math + prog, data = dados)
summary(modelo2)

Anova(modelo2, test = "LR", type = "III")

#Overdispersion = Residual deviance/df = 358.52/ 310 = 1.16


#### Passo 5: Realizacao do modelo (pacotes 'MASS' e 'car') ----

modelo2 <- glm.nb(daysabs ~ math + prog, data = dados)
summary(modelo2)

Anova(modelo2, test = "LR")


#### Passo 6: Comparacoes entre grupos (Pacote 'multcomp') ----

posthoc <- glht(modelo2, linfct = mcp(prog = "Tukey"))
summary(posthoc)


#### Passo 7: Plotar o grafico do modelo ----

## Calcular os dados preditos

newdata1 <- data.frame(math = mean(dados$math), prog = factor(1:3, levels = 1:3,                                                                           labels = levels(dados$prog)))
newdata1$phat <- predict(modelo2, newdata1, type = "response") # usando modelo para predicaoo dos valores
newdata1

newdata2 <- data.frame(math = rep(seq(from = min(dados$math), to = max(dados$math), length.out = 100), 3),
  prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
                  levels(dados$prog)))

newdata2 <- cbind(newdata2, predict(modelo2, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  DaysAbsent <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata2, aes(math, DaysAbsent)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
  geom_line(aes(colour = prog), size = 2) +
  labs(x = "Nota de matematica", y = "Dias ausentes (preditos)")


####~~~~~~~~~~~~~~~~~~~~~~ DISTRIBUICAO BINOMIAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

## Regressao logistica = Logit

#### PASSO 1: Importar pacotes ----

if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(car)) install.packages("car")   
library(car)
if(!require(aod)) install.packages("aod") 
library(aod)

#### Passo 2: Carregar banco de dados ----

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

getwd()

setwd("C:/Users/taise/Arquivos/Cursos_Disciplinas_Ministrados/2025_Modelos regressivos e analises multivariadas_PEA")


dados <- read.csv2("dados/data_binary.csv")
head(dados)

## Visualizar a estrutura dos dados
str(dados)

## Formatar os dados

dados$admit = as.numeric(dados$admit) #Variavel binaria
dados$gre = as.numeric(dados$gre)    #variavel continua
dados$gpa = as.numeric(dados$gpa)    #Variavel continua
dados$rank = as.factor(dados$rank)  #Variavel categorica - Prestigio da universidade varia de 1 a 4 (categorica)

#### Passo 3: Descricao dos dados (pacote 'dplyr') ----

summary(dados)

(dados_sumar_gre <- dados %>% group_by(rank) %>% 
                                summarise(Media = mean(gre), 
                                          sd = sd(gre)))
                               

(dados_sumar_gpa <- dados %>% group_by(rank) %>% 
                                  summarise(Media = mean(gpa), 
                                            sd = sd(gpa)))


#### Passo 4: Realizacao do modelo ----

mod_logit <- glm(admit ~ gre + gpa + rank, family = "binomial", data = dados)
summary(mod_logit)

Anova(mod_logit, test = "LR", type = "III")

plot(mod_logit)

shapiro.test(mod_logit$residuals)

#### Passo 5: Grafico do modelo ----

## Calcular os dados preditos

newdata1 <- with(dados, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

newdata1$rankP <- predict(mod_logit, newdata = newdata1, type = "response") # calcula os dados preditos a partir do modelo.

newdata2 <- with(dados, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))


newdata3 <- cbind(newdata2, predict(mod_logit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + 
                 geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) + 
                 geom_line(aes(colour = rank), size = 1)


#### Referencia:

# https://stats.idre.ucla.edu/other/dae/


