#'Programa de Pos Graduaçao em Ecologia de Ambientes Aquaticos Continentais (PEA)
#'AMB-40: Topicos especiais: Modelos regressivos e Analises multivariadas
#'Script pratico de RDA e pRDA
#'Beatriz Melissa Campos (melissacampos1@hotmail.com)

rm(list = ls()) #para limpar tudo

library(vegan)
library(adespatial)

data(mite) #dados biológicos
data(mite.env) #dados ambientais
data(mite.pcnm) #dados espaciais

#Transformar os dados biológicos
bio.hel <- decostand (mite,"hellinger")
bio.hel

#ambientais = dependendo dos dados ambientais estas devem ser transformados

#Seleção forward de VARIÁVEIS AMBIENTAIS usando ordistep
rda.envi <- rda(bio.hel ~ 1, data = mite.env)  # Modelo com intercepto
rda.env <- rda(bio.hel ~ ., data = mite.env)   # Modelo com todas as variáveis

selecao.env <- ordistep(rda.envi, scope = formula(rda.env), 
                        direction = "forward", permutations = 999)

# Modelo final com as variáveis selecionadas
rda.env.sel <- rda(bio.hel ~ WatrCont + Topo + SubsDens + Shrub + Substrate, data = mite.env)
vif.cca(rda.env.sel)  # Verificar colinearidade

#VARIÁVEIS ESPACIAIS
rda.spai<- rda (bio.hel~1, data=mite.pcnm) #Modelo com intercepto
rda.spa<-rda (bio.hel~., data=mite.pcnm)  #Modelo com todas as variáveis

seleçao.spa<-ordistep (rda.spai, data=mite.pcnm, 
                       scope= formula (rda.spa), direction="forward", 
                       perm.max=200)

rda.spa.sel<-rda(bio.hel~ V2 + V3 + V8 + V1 + V6 + V4 + V9 + V16 +
                   V7 + V20 + V11 + V10, data = mite.pcnm)
vif.cca (rda.spa.sel) #Verificando colinearidade 
#VIF = 1: Nenhuma colinearidade. Cada variável explicativa é 
#independente das outras variáveis no modelo


#Seleção de modelos pelo critério proposto por Blanchet, Legendre e Borcard (2008) 
rda.env <- rda(bio.hel ~ ., data = mite.env)
R2.env <- RsquareAdj(rda.env)$r.squared

# Verificar se os dados ambientais são numéricos
mite.env.numeric <- as.data.frame(lapply(mite.env, as.numeric))  # Converter para numérico
str(mite.env.numeric)  # Verificar a estrutura

# Realizar a seleção forward com forward.sel
selecao.envB <- forward.sel(bio.hel, mite.env.numeric, adjR2thresh = R2.env)
#forward.sel realiza a seleção forward de variáveis ambientais com base no R² ajustado.

# Exibir os resultados
selecao.envB

## Cálculo da pRDA
library(dplyr)

env <- mite.env %>% select(WatrCont, Topo, SubsDens, Shrub, Substrate)
env

spa <- mite.pcnm %>% select(V2, V3, V8, V1, V6, V4, V9, V16, V7, V20, V11, V10)
spa

rda.parcial<-varpart (bio.hel, env, spa) 
#varpart: Realiza a partição da variância para determinar quanto da variação nos
#dados biológicos é explicada por fatores ambientais, espaciais e pela interação 
#entre eles.
rda.parcial 

#O modelo completo (ambiente + espaço) explica 54.2% da variação nos dados 
#(R² ajustado = 0.54197), enquanto 45.8% da variação é residual (não explicada).

#Os fatores ambientais sozinhos explicam 43.7% da variação (R² ajustado = 0.43670).

#Os fatores espaciais sozinhos explicam 46.3% da variação (R² ajustado = 0.46286).

plot (rda.parcial) #diagrama de Venn


#Testar a significância das frações 

#fração [a]
rda.env.spa <-rda (bio.hel, env, spa) 
sig.a<-anova (rda.env.spa, pstep=999) 
sig.a

#fração [c]
rda.spa.env <-rda (bio.hel, spa, env) 
sig.c<-anova (rda.spa.env,pstep=999)
sig.c
