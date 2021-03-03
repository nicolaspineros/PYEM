library(moments)
library(stats)
library(ggplot2)
library(dplyr)
install.packages("ppcor")
library(ppcor)
attach(medifis)
#attach(ACCIONES)

#medias_ACCIONES=colMeans(ACCIONES)# vector de medias

##round(medias_ACCIONES,3)

#mvarcov_ACCIONES=cov(ACCIONES)

#round(mvarcov_ACCIONES,3)

#matriz_11=cbind(log(X1,exp(1)),log(X2,exp(1)),log(X3,exp(1)))

#round(cov(matriz_11),2)


#par(mfrow=c(1,2))

#hist(X1,xlab="X1",ylab="frecuencia",main="Histograma para X1")

#hist(X2,xlab="X2",ylab="frecuencia",main="Histograma para X2",col="pink")

#is.matrix(matriz_11)

#mvarcov_ACC=cov(ACCIONES)

#mvarcov_ACC=cov(matriz_11)

#coefasim_acc=c(skewness(X1),skewness(X2),skewness(X3))

#curt_acc=c(kurtosis(X1),kurtosis(X2),kurtosis(X3))

###############################3

plot(est,lbr, main = "Gráfico de Dispersión", xlab = "estatura",ylab = "longitud brazo")

cor( medifis[6],medifis[3])

plot(lbr,est, main = "Gráfico de Dispersión", xlab = "lbr",ylab = "estatura")

cor( medifis[8],medifis[6])#tabla original

plot(lbr,dcr, main = "Gráfico de Dispersión", xlab = "Longitud de brazo",ylab = "dist craneal")

############################MEDIDAS GLOBALES DE VARIABILIDAD
#Varianza varianza total, varianza media y varianza generalizada

#Varianza total


#Suma de las varianzas de cada una de las variables, es decir, la traza de la matriz de covarianza

#matriz_sp=medifis[3:6],peso=NULL

medifis$peso=NULL

cov_medsp=cov(medifis[3:8])

Vartotal_medsp = sum(diag(cov_medsp))

diag(cov_medsp)

Vartotal_medsp/6

#Varianza promedio

prom_medsp=mean(diag(cov_medsp))

#Varianza media

#Promedio de las varianzas

var_media=mean(diag(mvarcov_medifis))

#La varianza generaliza es el determinante de la matriz de covarianzas de la matriz de datos

##Varianza Generalizada##

vargen_mdsp=det(cov_medsp)

################################################################
######### Distancias ###############

#Distancias euclideas

# distancias entre individuos

# distancias entre variables

round(dist(medifis[3:9], method = "euclidean", diag = FALSE, upper = FALSE,p=7),3)

round(dist(t(medifis[3:9])),3)# distancias entre columnas

###################################################
####Distancia de Mahalanobis#####

###Estandarización de matriz de datos

estndr_medifis = as.data.frame(scale(medifis[2:8]))####estandarización matriz

cov(estndr_medifis)

cor(estndr_medifis)

#######Se observa que cuando la matriz de datos se encuentra estandarizada:

#1. Para la matriz de datos estandariza, su matriz de covarianzas es igual a la correlación

#2. La matriz de correlación de la matriz de datos original y la matriz de datos estandarizado son iguales


#####################################
#distancia DE Mahalanobis
#############

dmahalanobis_medif= sqrt(mahalanobis(medifis[3],colMeans(medifis[3]),cov(medifis[3])))# dist maha para la est

dmahalanobis_medif[order(dmahalanobis_medif,decreasing = TRUE)]

boxplot(dmahalanobis_medif, ylab = "Distancias de Mahalanobis")


####################################
estandar = as.data.frame(scale(medifis[3:9]))

cov(estandar)

cor(estandar)

dmahalanobis= sqrt(mahalanobis(medifis[6],colMeans(medifis[6]),cov(medifis[6])))

dmahalanobis[order(dmahalanobis,decreasing = TRUE)]

head(dmahalanobis)

boxplot(dmahalanobis, ylab = "Distancias de Mahalanobis")

#Mostramos un gráfico de cajas para detectar si existen valores atípicos

#####REGRESION MULTIPLE
#####Regresió lineal
rls=lm(lpie~est)# Creyendo que lpie depende de est
summary(rls)

######################MODELO DE REGRESIÓN LINEAL MÚLTIPLE#########
#Vamos a generar el modelo con todos los predictores
modelo.lineal = lm(est ~ ., medifis[3:9])
summary(modelo.lineal)

library(dplyr)
library(MASS)
pairs(medifis[3:9])#matriz con graficos de dispérsion con todas las vvariables de medifis
round(cor(medifis[3:9]),3)
require(psych)   
multi.hist(medifis[3:9], dcol = c("blue", "red"), 
           dlty = c("dotted", "solid"), main = "" )#múltiples histogramas
require(GGally)
ggpairs(medifis[3:9], lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")
cor(medifis[3:9])

#####Correlaciones parciales
#la inversa de matriz de varianzas y cov
inv_mvarcov_medifis=solve(mvarcov_medifis)
diag1=diag(mvarcov_medifis)
diag2=diag(inv_mvarcov_medifis)
diag1%*%t(diag2)
diag2%*%diag1
# inv_mvarcov
####################3
install.packages("ppcor")
library(ppcor)
attach(medifis)
matriz=cbind(est, lpie, lbr, aes, dcr, lrt) 
is.matrix(matriz)
corpar_medifispcor(matriz, method = "pearson")
round(pcor(matriz, method = "pearson")$estimate,2)
round(pcor(matriz, method = "pearson")$p.value,3)

######################



#############3
