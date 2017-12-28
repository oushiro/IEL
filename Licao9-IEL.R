####################Script para Licao 9#########################
###Estatistica Inferencial Testes de Proporcao e Qui-Quadrado###
################################################################

#Definir diretorio de trabalho
#setwd()

#carregar os dados
ds <- read.csv("LabovDS.csv", header = T, sep = ",")

#inspecionar df
str(ds)

#criar subconjunto de dados de r0 e r1
ds <- subset(ds, r %in% c("r0", "r1"))

#excluir "d" dos fatores de ds$r
ds$r <- factor(as.character(ds$r), exclude="d")

str(ds)

#fazer tabela de frequencias dos dados da VD
tab.r <- table(ds$r); tab.r

#fazer tabela de proporcoes
prop.r <- prop.table(tab.r); prop.r

#plotar tabela de proporcoes
barplot(prop.r, beside = T)

#teste de proporcoes sob hipotese nula de probabilidade = 0.5
teste1 <- prop.test(tab.r) #tem que ser feita sobre a tabela de frequencias, nao de proporcoes

#ver resultados do teste de proporcoes
teste1

#acima, probabilidade = 0.5; supondo que um estudo previo havia verificado uma proporcao de 0.7
prop.test(tab.r, p = 0.7) #hipotese bidirecional
prop.test(tab.r, p = 0.7, alternative = "less") #hipotese unidirecional
prop.test(tab.r, p = 0.7, alternative = "greater") #hipotese unidirecional

#comparacao entre dois grupos
tab.word <- table(ds$word, ds$r); tab.word
prop.word <- prop.table(tab.word, 1); prop.word

#plotar grafico de barras
barplot(prop.word[,1], beside = T)

#teste de qui-quadrado
x2.word <- chisq.test(tab.word); x2.word

#valores gerados pelo teste de qui-quadrado
str(x2.word)

#valores observados e valores esperados
O <- x2.word$observed; O
E <- x2.word$expected; E

#como computar valores esperados 'manualmente': (T-linha * T-coluna) / T-geral
addmargins(O)

(499 * 347) / 730
(499 * 383) / 730
(231 * 347) / 730
(231 * 383) / 730

#os valores esperados sao aqueles caso nao houvesse diferenca entre grupo 1 e grupo 2
E[1, 1] / 347
E[2, 1] / 383

#formula do calculo de X2
sum((O-E)^2/E)
#equivale ao calculo sem correcao:
chisq.test(tab.word, correct = F)

#graus de liberdade: (nrow - 1) * (ncol - 1)
(nrow(tab.word) - 1) * (ncol(tab.word) - 1)

#qui-quadrado para mais de dois grupos
tab.store <- table(ds$store, ds$r); tab.store
prop.store <- prop.table(tab.store, 1); prop.store

barplot(prop.store[,1], beside = T)

x2.store <- chisq.test(tab.store); x2.store

#onde esta a diferenca de fato?
O <- x2.store$observed
E <- x2.store$expected

#valor de qui-quadrado por celula: qto maior o X2, maior a diferenca entre o valor observado e o valor esperado
(O-E)^2/E 

#outra maneira de encontrar a diferenca: valores positivos são valores observados acima dos valores esperados, e valores negativos sao valores observados abaixo dos valores esperados
x2.store$residuals 

#visualizar diferenca no grafico
barplot(prop.store[,1], beside = T)
abline(h = .68, lty = 2) 

#teste de qui-quadrado entre dois niveis especificos
chisq.test(tab.store[2:3,])

#valor-p eh sensivel ao tamanho da amostra
tab.word / 20

chisq.test(tab.word / 20)
