#############Script das Lições 4 e 5#############
####Estatistica Descritiva Variaveis Nominais####
#################################################

# O hashtag é o caractere de comentário. Tudo é escrito depois dele é ignorado pelo R. Use-o para fazer anotações que achar necessárias.

#Definir diretório de trabalho: preencha a função com o caminho do diretório e apague o # para rodar a linha 8. Linhas de comando em scripts são enviadas ao Console por meio de CTRL + L.
#setwd()

#Carregar dados
ds <- read.csv("LabovDS.csv", header = T, sep = ",")

#Checar dados
str(ds)

#Fazer tabelas
table(ds$r)

#Definir novo conjunto de dados
ds2 <- subset(ds, r %in% c("r0", "r1"))
ds2$r <- factor(as.character(ds2$r), exclude="d")

#distribuição de dados de /r/
# ; separa duas linhas de comando na mesma linha
tab.r <- table(ds2$r); tab.r
tab.store <- table(ds2$store, ds2$r)
tab.emphasis <- table(ds2$emphasis, ds2$r)
tab.word <- table(ds2$word, ds2$r)

prop.store <- prop.table(tab.store, 1) * 100

####Gráfico de barras####
?barplot

#Na função barplot abaixo, os argumentos foram separados em linhas para facilitar a visualização. Veja que cada argumento termina com uma vírgula, exceto o último. Outros argumentos não usados foram aqui incluídos para possíveis usos futuros. Para usá-los, descomente a linha respectiva apagando o # (e tome cuidado com as vírgulas!).
barplot(prop.store[3:1, 1],
        beside = T,
        main = "Proporção de apagamento de /r/ pós vocálico \n em três lojas de departamento em Nova Iorque",
        names.arg = c("Saks", "Macy's", "S. Klein"),
        ylim = c(0, 100),
        # xlim= c(0, 100),
        xlab = "Lojas de Departamento",
        ylab = "Proporção de apagamento de /r/",
        col = c("hotpink2", "goldenrod1", "green1")
        # cex.axis = 1, 
        # cex.names = 1,
        # legend.text = F,
        # horiz = F
)

#Vários dos argumentos de barplot têm funções próprias.P.ex., a legenda pode ser especificada dentro ou fora da função barplot. Se fora:  
cores <- c("hotpink2", "goldenrod1", "green1")

legend("topleft", #Posicao: "topright", "bottomleft", "bottomright"
       legend = c("Saks","Macy's", "S. Klein"), 
       cex=.8, 
       bty="n", 
       fill=cores)

#Neste caso, a legenda é claramente redundante, mas vc pode querer incluí-la em outros gráficos. 

####Gráfico de linhas####
?plot

plot(prop.store[3:1, 1], 
     type = "o", 
     pch = 19, 
     lty = 3,
     axes = F, 
     xlab = "Lojas de Departamento", 
     ylab = "Proporção de apagamento de /r/", 
     col = "black")

axis(1, at = 1:3, lab = c("Saks", "Macy's", "S. Klein"))
axis(2, las=2)
box()
title("Proporção de apagamento de /r/ pós-vocálico \n em três lojas de departamento em Nova Iorque")


# Ver pagina http://www.statmethods.net/advgraphs/parameters.html para explicações sobre vários outros parâmetros gráficos. Interessa não só a primeira página (Graphical Parameters), mas também todas as demais: Axes and Text, Combining Plots etc.



####Outro modo de exportar figuras####

#definir arquivo a ser exportado
png("figura.png",
    width = 480,
    height = 480,
    units = "px",
    res=300) #a resolução pode ser melhorada aqui

#plotar figura
barplot(prop.store[3:1, 1],
        beside = T,
        main = "Proporção de apagamento de /r/ pós vocálico \n em três lojas de departamento em Nova Iorque",
        names.arg = c("Saks", "Macy's", "S. Klein's"),
        ylim = c(0, 100),
        xlab = "Lojas de Departamento",
        ylab = "Proporção de apagamento de /r/",
        col = c("hotpink2", "goldenrod1", "green1")
)

cores <- c("hotpink2","goldenrod1", "green1")

legend("topleft", #Posicao: "topright", "bottomleft", "bottomright"
       legend = c("Saks","Macy's", "S. Klein's"), 
       cex=.8, 
       bty="n", 
       fill=cores)

#fechar o dispositivo de exportação de figuras
dev.off()

####(fim de outro modo de exportar figuras)###


