###############Script para Licao 15####################
#####Estatistica Inferencial Regressao Logistica 2#####
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

#redefinir niveis de CONT.FON.PREC
levels(dados$CONT.FON.PREC)
dados$CONT.FON.PREC <- factor(dados$CONT.FON.PREC, levels = levels(dados$CONT.FON.PREC)[c(5,4,2,3,1,6,7)]) 
levels(dados$CONT.FON.PREC) #nova ordem de fatores

#redefinir niveis de CONT.FON.SEG
levels(dados$CONT.FON.SEG) #
levels(dados$CONT.FON.SEG)<-list( #renomear fatores
  pausa = "#",
  labial = "b", 
  coronal = "d", 
  coronal = "dz", 
  labial = "f", 
  dorsal = "g", 
  dorsal = "h", 
  coronal = "j", 
  dorsal = "k", 
  coronal = "l", 
  labial = "m", 
  coronal = "n", 
  labial = "p", 
  coronal = "s", 
  coronal = "t", 
  coronal = "ts", 
  labial = "v",
  coronal = "x",
  coronal = "z" 
)
levels(dados$CONT.FON.SEG)
#novos valores aa esquerda, valores antigos aa direita

#tonicidade, posicaor, classe morfologica e cont.fon.seg nao totalmente ortogonais 
table(dados$CONT.FON.SEG, dados$POSICAO.R)
table(dados$CLASSE.MORFOLOGICA, dados$POSICAO.R)
table(dados$CLASSE.MORFOLOGICA, dados$TONICIDADE)

#modelo com essas variaveis para verificar multicolinearidade
mod <- glm(VD ~ TONICIDADE + POSICAO.R + CLASSE.MORFOLOGICA + CONT.FON.SEG, data = dados, family = binomial)
summary(mod)

#aplicacao da funcao vif() para avaliar multicolinearidade
car::vif(mod)


#####

#formulas que serao bastante usadas adiante
formula <- as.formula(VD ~ SEXO.GENERO + FAIXA.ETARIA * REGIAO + INDICE.SOCIO + CONT.FON.PREC + CONT.FON.SEG + TONICIDADE + POSICAO.R + CLASSE.MORFOLOGICA)

escopo <- as.formula(~ SEXO.GENERO + FAIXA.ETARIA * REGIAO + INDICE.SOCIO + CONT.FON.PREC + CONT.FON.SEG + TONICIDADE + POSICAO.R + CLASSE.MORFOLOGICA)


###Modelo de regressao logistica####
modelo <- glm(formula, data = dados, family = binomial)
summary(modelo)

library(rms)
lrm(formula, data = dados)

#step forward
m0 <- glm(VD ~ 1, data = dados, family = binomial)
m.fw <- step(m0, direction = "forward", scope = escopo)
m.fw


#step backward
m.bw <- step(modelo, direction = "backward")
m.bw

#step both
m.both <- step(m0, scope = escopo)
m.both

drop1(modelo, test="LR")

#testar overfitting
mod.lrm <- lrm (formula, data = dados, x = T, y = T)
#library(rms)
validate(mod.lrm, B = 200, bw = T)

############
#Testar se pressupostos sao atendidos

#a relacao entre o logit e as variaveis previsoras numericas eh linear
#modelo sem interacao para aplicar crPlot
modelo2 <- glm(VD ~ SEXO.GENERO + FAIXA.ETARIA + REGIAO + INDICE.SOCIO + CONT.FON.PREC + CONT.FON.SEG + TONICIDADE + POSICAO.R + CLASSE.MORFOLOGICA, data = dados, family = binomial)
summary(modelo2)

library(car)
crPlot(modelo2, variable = "INDICE.SOCIO")

#colinearidade entre variaveis previsoras do modelo
#library(car)
car::vif(modelo)

#independencia entre observacoes - nao ha! --> modelo de efeitos mistos
library(lme4)
library(lmerTest)

mod.glmer <- glmer(VD ~ SEXO.GENERO + FAIXA.ETARIA * REGIAO + INDICE.SOCIO + CONT.FON.PREC + CONT.FON.SEG + TONICIDADE + POSICAO.R + CLASSE.MORFOLOGICA + (1|INFORMANTE) + (1|ITEM.LEXICAL), data = dados, family = binomial)
summary(mod.glmer)

#visualizar resultados numericos em grafico de efeitos
library(effects)
plot(allEffects(mod.glmer), type = "response")
plot(allEffects(mod.glmer), type = "response", ask = T)
