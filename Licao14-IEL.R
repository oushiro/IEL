###############Script para Licao 14####################
#####Estatistica Inferencial Regressao Logistica 1#####
#######################################################

#definir diretorio de trabalho
#setwd()

#carregar dados
dados <- read.csv("DadosRT.csv", header = T, sep = ",")

#inspecionar dados
str(dados)
View(dados)

#######Funcoes uteis#####
#essas funcoes nao existem na instalacao base do R nem em outros pacotes
# de Gries 2013
logit <- function(x) { # logit: maps the range (0,1) to the range -infinite to + infinite)
  log(x/(1-x)) # Gelman & Hill (2007:80)
}

ilogit <- function(x) { # inverse logit: maps the range (-∞,∞) to the range (0,1)
  1/(1+exp(-x)) # Gelman & Hill (2007:80)
}
##########################

#redefinir niveis da VD (retroflexo como segundo nivel)
levels(dados$VD)
dados$VD <- factor(dados$VD, levels=levels(dados$VD)[c(2,1)])
levels(dados$VD)

###Modelo (1) VD nominal ~ VI nominal/binaria
mod1 <- glm(VD ~ SEXO.GENERO, data = dados, family = binomial)
summary(mod1)


####Excursus... como calcular logodds###########
tab.sexo <- table(dados$SEXO.GENERO, dados$VD)
tab.sexo

#da tabela de frequencias acima
odds.F <- 3478 / 1087
odds.M <- 3137 / 1524
odds.F / odds.M #odds-ratio

log(odds.F / odds.M) #log-odds

#Ver Gries (2013:300) -- figura transformacoes odds, logodds, probabilidades

#transformar logodds em probabilidades: ilogit()
ilogit(-1.16304 + 0.4411073) # probabilidade para homens de usar retroflexo
ilogit(-1.16304) # probabilidade para mulheres de usar retroflexo
#comparar com...
prop.table(tab.sexo, 1)

######fim de excursus############################

# grafico de efeitos 
library(effects)
plot(allEffects(mod1), type = "response")

#**Valor de significancia para modelo como um todo
mod1$null.deviance
mod1$deviance
mod1$df.null
mod1$df.residual
pchisq(mod1$null.deviance - mod1$deviance, mod1$df.null - mod1$df.residual, lower.tail=F)#valor de significancia para modelo como um todo
###

confint(mod1) # intervalos de confianca das estimativas do modelo em logodds

#outro modelo logistico: funcao lrm() -- gera medidas estatisticas uteis
library(rms)
lrm(VD ~ SEXO.GENERO, data = dados) 

###Modelo (2) VD nominal ~ VI nominal mais de 2 fatores
mod2 <- glm(VD ~ FAIXA.ETARIA, data = dados, family = binomial)
summary(mod2)

plot(allEffects(mod2), type = "response")

confint(mod2) # intervalos de confianca das estimativas do modelo

lrm(VD ~ FAIXA.ETARIA, data=dados)


###Modelo (3) VD nominal ~ VD numerica

mod3 <- glm(VD ~ INDICE.SOCIO, data = dados, family = binomial)
summary(mod3)

plot(allEffects(mod3), type = "response")

confint(mod3)

#Calculo de probabilidades/lododds
1.53407 + (-0.81609 * 4.2) #falante com INDICE.SOCIO = 4.2
ilogit(-1.893508)

lrm(VD ~ INDICE.SOCIO, data=dados)


###Modelo (4) VD nominal ~ duas VIs nominais
mod4 <- glm(VD ~ FAIXA.ETARIA + REGIAO, data = dados, family = binomial)
summary(mod4)

plot(allEffects(mod4))

confint(mod4) 

lrm(VD ~ FAIXA.ETARIA + REGIAO, data=dados)

###Modelo (5) VD nominal ~ duas VIs com interacao
mod5 <- glm(VD ~ FAIXA.ETARIA * REGIAO, data = dados, family = binomial)
summary(mod5)

#Calculo de probabilidades/logodds
-1.31754 + 1.33476 # 1a faixa etária que mora na periferia.
-1.31754 -0.10887 + 1.33476 -0.69440 #2a faixa etária que mora na região periferia

plot(allEffects(mod5), type = "response")

confint(mod5) 

lrm(VD ~ FAIXA.ETARIA * REGIAO, data=dados)
