#############Script para Licao 13###################
#####Estatistica Inferencial Regressao Linear 2#####
####################################################


###Carregar dados####

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
#excluir dados F1.NORM > 500 Hz
VOGAL.e2 <- subset(VOGAL.e, F1.NORM < 500)
#reorganizar numeracao dos dados
rownames(VOGAL.e2) <- seq(length=nrow(VOGAL.e2))


#recodificacao de CONT.PREC
levels(VOGAL.e2$CONT.PREC) <- list(
  dental.alveolar = c("t", "d", "n", "l"),
  labial = c("p", "b", "m", "f", "v"),
  palatal.sibilante = c("S", "Z", "L", "N", "s", "z"),
  velar = c("k", "g"),
  vibrante = c("r", "h", "R")
) #aa esquerda, novos valores; aa direita, valores antigos
levels(VOGAL.e2$CONT.PREC)
table(VOGAL.e2$CONT.PREC)

#recodificacao de CONT.FON.SEG
levels(VOGAL.e2$CONT.SEG) <- list(
  dental.alveolar = c("t", "d", "n", "l"),
  labial = c("p", "b", "m", "f", "v"),
  palatal.sibilante = c("S", "Z", "L", "N", "s", "z"),
  velar = c("k", "g", "a"),
  vibrante = c("r", "h", "R")
) #aa esquerda, novos valores; aa direita, valores antigos
levels(VOGAL.e2$CONT.SEG)
table(VOGAL.e2$CONT.SEG)


###Modelos lineares multivariados####

###modelo com todas as variaveis previsoras
mod <- lm(F1.NORM ~ AMOSTRA + SEXO + F1.SEG.NORM + CONT.PREC + CONT.SEG, data = VOGAL.e2)
summary(mod)

#step forward
m0 <- lm(F1.NORM ~ 1, data = VOGAL.e2)
m.fw <- step(m0, direction = "forward", scope = ~ AMOSTRA + SEXO + F1.SEG.NORM + CONT.PREC + CONT.SEG)
m.fw

#step backward
m.bw <- step(mod, direction = "backward")
m.bw

#step both
m.both <- step(m0, scope = ~ AMOSTRA + SEXO + F1.SEG.NORM + CONT.PREC + CONT.SEG)
m.both

#drop1
drop1(mod, test = "F")


###novo modelo linear sem variavel SEXO
modelo <- lm(F1.NORM ~ F1.SEG.NORM + AMOSTRA + CONT.SEG + CONT.PREC, data = VOGAL.e2)
summary(modelo)


###Checagem de pressupostos####

###(a) relacao entre variavel resposta e variavel previsora numerica eh linear?
#carregar biblioteca `car` para funcoes crPlot() e vif()
library(car)
crPlot(modelo, var = "F1.SEG.NORM")

#retirar valores atipicos de F1.SEG.NORM
VOGAL.e3 <- subset(VOGAL.e2, F1.SEG.NORM < 500)

#novo modelo linear
modelo2 <- lm(F1.NORM ~ AMOSTRA + F1.SEG.NORM + CONT.PREC + CONT.SEG, data = VOGAL.e3)
summary(modelo2)

crPlot(modelo2, var = "F1.SEG.NORM")

###(b) ha multicolinearidade?
vif(modelo2)


###(c) residuos teem distribuicao normal?
shapiro.test(modelo2$residuals)

###(d) observacoes sao independentes? -- em dados linguisticos, quase nunca sao! --> MODELOS DE EFEITOS MISTOS

#carregar bibliotecas necessarias para todar modelos de efeitos mistos
library(lme4)
library(lmerTest)

#modelo linear de efeitos mistos
mod1.lmer <- lmer(F1.NORM ~ AMOSTRA + F1.SEG.NORM + CONT.PREC + CONT.SEG + (1|INFORMANTE) + (1|PALAVRA), data = VOGAL.e3)
summary(mod1.lmer)

#step backward
m.bw.lmer <- step(mod1.lmer, direction = "backward")
m.bw.lmer

