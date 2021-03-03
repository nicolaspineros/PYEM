#Matriz de Varianzas y Covarianzas ACCIONES
## INTEGRANTES: Luz Vanessa Rozo, Valentina soto, Nicol Montañez, Nicolas Piñeros

head(ACCIONES)
attach(ACCIONES)

medias_ACCIONES=colMeans(ACCIONES)# vector de medias

round(medias_ACCIONES,3)

mvarcov_ACCIONES=cov(ACCIONES)

round(mvarcov_ACCIONES,3)

matriz_11=cbind(log(X1,exp(1)),log(X2,exp(1)),log(X3,exp(1)));matriz_11

round(cov(matriz_11),2)


par(mfrow=c(1,2))

hist(X1,xlab="X1",ylab="frecuencia",main="Histograma para X1")

hist(X2,xlab="X2",ylab="frecuencia",main="Histograma para X2",col="pink")

is.matrix(matriz_11)

mvarcov_ACC=cov(ACCIONES);mvarcov_ACC

mvarcov_ACC=cov(matriz_11)

mvarcov_ACC

#Varianza Generalizada 
vargen_mvarcov_ACC = det(mvarcov_ACC); vargen_mvarcov_ACC

#Varianza Promedio
varpro_mvarcov_ACC = mean(diag(mvarcov_ACC)); varpro_mvarcov_ACC

#Varianza Total
varto_mvarcov_ACC = sum(diag(mvarcov_ACC)); varto_mvarcov_ACC
