##############Script para Lição 7###############
###Estatistica Descritiva Variaveis Numericas###
################################################

# Definir o diretório de trabalho
#setwd()

# Carregar dados
pretonicas <- read.csv("Pretonicas.csv", header = T, sep = ",")

# Inspecionar dados
str(pretonicas)

# Reorganizar os níveis da variável VOGAL de mais para menos anterior
pretonicas$VOGAL <- factor(pretonicas$VOGAL, levels=levels(pretonicas$VOGAL)[c(3, 2, 1, 4, 5)])

levels(pretonicas$VOGAL) # checar reorganização

######## Gráficos ########

###Gráfico de linhas dos espaços vocálicos####

# Obter coordenadas de x (F2) e y (F1)
F1 <- aggregate(VD ~ VI + VI, 
                data = conjdados, 
                medidaestatistica)
F2 <- aggregate(VD ~ VI + VI, 
                data = conjdados, 
                medidaestatistica)

# Definir vogais
vogais <- c("i", "e", "a", "o", "u")

# Plotar vogais PBSP
plot(F2[linhas, coluna], 
     F1[linhas, coluna], 
     ylim = c(limitemax, limitemin), 
     xlim = c(limitemax, limitemin), 
     ylab = "",
     xlab = "",
     type = "", 
     pch = vogais,
     col = "")

# Adicionais vogais SP2010
lines(F2[linhas, coluna],
      F1[linhas, coluna],
      type = "", 
      pch = vogais,
      col = "")

# Inserir legenda
legend("",
       legend = c("", ""),
       text.col = c("", ""),
       bty = "",
       cex = )

# Inserir título
title("Médias de F1/F2 nas amostras PBSP e SP2010")


###Histograma####

# Criar subconjunto de dados da vogal /e/ na amostra PBSP
pretonicas.PBSP.e <- subset(conjdados, variavel %in% "variante" & variavel %in% "variante")

# Plotar histograma
hist(vetornumerico, 
     xlim = c(, ),
     ylim = c(, ),
     breaks = ,
     xlab = "Vogal pretônica /e/",
     ylab = "Frequência",
     main = "Distribuição de F1 normalizado da vogal pretônica /e/ \n na amostra PBSP",
     col = "")

# Inserir linhas verticais com indicação da média e da mediana
media <- mean(pretonicas.PBSP.e$F1.NORM)
mediana <- median(pretonicas.PBSP.e$F1.NORM)

abline(v = c(media, mediana), 
       col = c("red", "blue"))


###Boxplot####

# Definir cores dos boxplots
cores <- c("", "", "", "", "")
cores2 <-rep(cores, each=2)

# Plotar boxplot
boxplot(VD ~ VI + VI, 
        data = conjuntodedados, 
        ylim = c(, ), 
        xlab = "", 
        ylab = "", 
        main = "Distribuição dos valores de F1 normalizados \n nas amostras PBSP e SP2010", 
        col = cores2,
        notch = )

