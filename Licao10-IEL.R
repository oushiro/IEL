######Script para Licao 10###########
###Estatistica Inferencial Teste-t###
#####################################

#definir diretorio de trabalho 
#setwd()

#carregar dados
pretonicas <- read.csv("Pretonicas.csv", header = T, sep = ",")

#inspecionar df
str(pretonicas)

#criar subconjuntos de dados
PBSP.e <- subset(pretonicas, AMOSTRA %in% "PBSP" & VOGAL %in% "e")
SP2010.e <- subset(pretonicas, AMOSTRA %in% "SP2010" & VOGAL %in% "e")
VOGAL.e <- subset(pretonicas, VOGAL %in% "e")

#visualizar os dados
#par() muda parametros graficos do R. Ver <http://rfunction.com/archives/1538>.
par(mfrow = c(2, 1)) 
hist(PBSP.e$F1.NORM, xlim = c(300, 600))
hist(SP2010.e$F1.NORM, xlim = c(300, 600))
par(mfrow = c(1, 1))

#calcular medias, desvio padrao e erro padrao
(mPBSP <- mean(PBSP.e$F1.NORM))
(sdPBSP <- sd(PBSP.e$F1.NORM))
(epPBSP <- sdPBSP/sqrt(length(PBSP.e$F1.NORM)))
(mSP2010 <- mean(SP2010.e$F1.NORM))
(sdSP2010 <- sd(SP2010.e$F1.NORM))
(epSP2010 <- sdSP2010/sqrt(length(SP2010.e$F1.NORM)))

#visualizar os dados
boxplot(F1.NORM ~ AMOSTRA, data = subset(pretonicas, VOGAL %in% "e"), ylim = c(600, 300), notch = T)

#teste de Shapiro para checar normalidade da distribuicao
shapiro.test(PBSP.e$F1.NORM)
shapiro.test(SP2010.e$F1.NORM)

#teste-t/teste de Wilcoxon

#uma amostra
t.test(PBSP.e$F1.NORM, mu = 440)
wilcox.test(SP2010.e$F1.NORM, mu = 410)

#calculo do valor-t
(mPBSP - 440) / (sdPBSP / sqrt(length(PBSP.e$F1.NORM)))#computar automaticamente

#teste unidirecional
t.test(PBSP.e$F1.NORM, mu = 440, alternative = "less")


#duas amostras
wilcox.test(F1.NORM ~ AMOSTRA, data = VOGAL.e, conf.int = T)
t.test(F1.NORM ~ AMOSTRA, data = VOGAL.e)

#diferenca entre medias
mPBSP - mSP2010


#duas amostras pareadas
sleep #conjunto de dados (instalacao base)

#criar vetores
g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]

#testar normalidade das distribuicoes
shapiro.test(g1)
shapiro.test(g2)

#teste-t (pareado)
t.test(g1, g2, paired = T)
t.test(g1, g2) # para comparar resultado

