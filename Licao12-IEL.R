#############Script para Licao 12###################
#####Estatistica Inferencial Regressao Linear 1#####
####################################################

#definir diretorio de trabalho
#setwd()

#carregar arquivo Pretonicas.csv
pretonicas <- read.csv("Pretonicas.csv", header = T, sep = ",")

#checar estrutura do dataframe
str(pretonicas)

#visualizar a planilha
View(pretonicas)

#criar subconjunto de dados da vogal /e/ pretonica
VOGAL.e <- subset(pretonicas, VOGAL %in% "e")

#excluir demais niveis da variavel VOGAL
VOGAL.e$VOGAL <- factor(as.character(VOGAL.e$VOGAL), exclude = c("a", "i", "o", "u"))
#reorganizar numeracao dos dados
rownames(VOGAL.e) <- seq(length=nrow(VOGAL.e))


#recodificacao de CONT.PREC
levels(VOGAL.e$CONT.PREC) <- list(
  dental.alveolar = c("t", "d", "n", "l"),
  labial = c("p", "b", "m", "f", "v"),
  palatal.sibilante = c("S", "Z", "L", "N", "s", "z"),
  velar = c("k", "g"),
  vibrante = c("r", "h", "R")
) #aa esquerda, novos valores; aa direita, valores antigos
levels(VOGAL.e$CONT.PREC)
table(VOGAL.e$CONT.PREC)

#recodificacao de CONT.FON.SEG
levels(VOGAL.e$CONT.SEG) <- list(
  dental.alveolar = c("t", "d", "n", "l"),
  labial = c("p", "b", "m", "f", "v"),
  palatal.sibilante = c("S", "Z", "L", "N", "s", "z"),
  velar = c("k", "g", "a"),
  vibrante = c("r", "h", "R")
) #aa esquerda, novos valores; aa direita, valores antigos
levels(VOGAL.e$CONT.SEG)
table(VOGAL.e$CONT.SEG)

###Modelos Lineares####
#1 VI binaria
mod1 <- lm(F1.NORM ~ AMOSTRA, data = VOGAL.e)
summary(mod1)

#visualizar distribuicao de dados de F1.NORM por amostra
boxplot(F1.NORM ~ AMOSTRA, data = VOGAL.e, ylim = c(600, 300), notch = T)

#excluir dados de F1.NORM acima de 500 Hz
VOGAL.e2 <- subset(VOGAL.e, F1.NORM < 500)

#refazer o modelo F1.NORM ~ AMOSTRA
mod2 <- lm(F1.NORM ~ AMOSTRA, data = VOGAL.e2)
summary(mod2)

#resultado de lm() com 1 VI binaria equivale a t.test()
t.test(F1.NORM ~ AMOSTRA, data = VOGAL.e2)

#visualizar resultados do modelo com grafico de efeitos
#funcoes effect() e allEffects() requerem pacote effects
library(effects)
plot(effect("AMOSTRA", mod2), ylim = c(450, 400), grid = T)


#########
#1 VI mais de 2 fatores
mod3 <- lm(F1.NORM ~ CONT.SEG, data = VOGAL.e2)
summary(mod3)

levels(VOGAL.e2$CONT.SEG)

#visualizar resultados do modelo com grafico de efeitos
plot(effect("CONT.SEG", mod3), ylim = c(450, 400), grid = T)

#e todos os demais contrastes possiveis?
TukeyHSD(aov(mod3)) #comparacao multipla


#########
#1 VI numerica
mod4 <- lm(F1.NORM ~ F1.SEG.NORM, data = VOGAL.e2)
summary(mod4)

#visualizar resultados do modelo com grafico de efeitos
plot(effect("F1.SEG.NORM", mod4), ylim = c(450, 400), grid = T)

#########
#2 VIs nominais
mod5 <- lm(F1.NORM ~ AMOSTRA + SEXO, data = VOGAL.e2)
summary(mod5)

#visualizar resultados do modelo com grafico de efeitos
plot(allEffects(mod5), ylim = c(450, 400), grid = TRUE)


######### testar interacao 2 entre variaveis ########
mod6 <- lm(F1.NORM ~ AMOSTRA * SEXO, data = VOGAL.e2)
summary(mod6)

#visualizar resultados do modelo com grafico de efeitos
plot(allEffects(mod6), ylim = c(450, 400), grid = TRUE)

