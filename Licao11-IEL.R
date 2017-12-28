#############Script para Licao 11###################
###Estatistica Inferencial Correlacao e Regressao###
####################################################

#dois vetores numericos
idade<-c(1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 8, 9, 11, 12, 12)
altura<-c(60, 65, 97, 98, 100, 105, 107, 105, 119, 122, 125, 132, 142, 147, 153)

#inspecionar vetores
idade
altura

#visualizar distribuição
plot(idade, altura)

#teste de correlacao de Pearson
cor.test(idade, altura)
cor.test(altura, idade) #mesmo resultado

#teste de Shapiro para testar normalidade
shapiro.test(idade)
shapiro.test(altura)
#para dados que nao seguem dist. normal, method = "spearman"

#modelo linear
modelo <- lm(altura ~ idade)
modelo

#plotar linha de regressao em grafico de dispersao
abline(modelo)

#inspecionar modelo linear
str(modelo) 
#valores previstos
modelo$fitted.values 
#diferenca entre valores observados e valores previstos: residuos
modelo$residuals # equivale a altura - modelo$fitted.values
#visualizar residuos no grafico
segments(idade, modelo$fitted.values, idade, altura)

summary(modelo)
modelo$fitted.values 

#calculo do valor-t: esmativa / erro padrao
62.504 / 3.7691
7.5453 / 0.5135

#R^2 : r de Pearson ao quadrado
cor.test(idade, altura)$estimate
cor.test(idade, altura)$estimate ^ 2

###dados linguisticos
#definir diretorio de trabalho
#setwd()

#carregar dataframe
cov <- read.csv("Covariaveis.csv", header = T, sep= ",")

#inspecionar dataframe
str(cov)

#visualizar planilha
View(cov)

#visualizar grafico de dispersao
plot(cov$CV3PP, cov$CN)

#teste de Shapiro
shapiro.test(cov$CV3PP)
shapiro.test(cov$CN)

#teste de correlacao nao parametrico
cor.test(cov$CV3PP, cov$CN, method = "spearman")

#modelo linear
modelo1 <- lm(CN ~ CV3PP, data = cov)
#resultado do modelo linear
summary(modelo1)
#plotar linha de regressao
abline(modelo1)

#visualizar multiplas correlacoes
library(languageR)
pairscor.fnc( ~ cov$EN + cov$RT + cov$R0 + cov$CN + cov$CV3PP + cov$CV1PP, hist = T, smooth = T)



