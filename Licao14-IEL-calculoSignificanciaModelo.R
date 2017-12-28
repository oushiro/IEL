##Como calcular Valor de significancia para modelo logistico como um todo

#visualizar dados do modelo
mod1$null.deviance
mod1$deviance
mod1$df.null
mod1$df.residual

#funcao pchisq faz teste de qui-quadrado e calculo de valor-p a partir dos parametros acima
pchisq(mod1$null.deviance - mod1$deviance, mod1$df.null - mod1$df.residual, lower.tail=F)