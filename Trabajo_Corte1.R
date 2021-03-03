# Trabajo PYEM Primer Corte 
## Integrantes: Vanessa Rozo, Nicol Montañez, Nicolas Piñeros

library(moments)
library(stats)
library(ggplot2)
library(dplyr)
install.packages("ppcor")
library(ppcor)

head(DataBase)
attach(DataBase)

names(DataBase[2:6])#nombre de las variables cuantitativas

medias = round(apply(DataBase[2:6],2,mean),1);medias

mediana = round(apply(DataBase[2:6],2,median),1);mediana

D_estand = round(apply(DataBase[2:6],2,sd),1);D_estand

Curtosis = round(apply(DataBase[2:6],2,kurtosis),1);Curtosis

Coef_asim = round(apply(DataBase[2:6],2,skewness),1);Coef_asim

Coef_var = round(D_estand/medias,1);Coef_var

#enunciados medidas columna 1
medidas=c("Medias", "Mediana","Desv. Estándar","Coef. Asimetría","Coef. Curtosis","Coef.Variación")

pibm = c((mean(PIB)),median(PIB),sd(PIB),skewness(PIB),kurtosis(PIB),abs(sd(PIB)/mean(PIB)));pibm

expo = c((mean(`Exportaciones comerciales`)),median(`Exportaciones comerciales`),sd(`Exportaciones comerciales`),skewness(`Exportaciones comerciales`),kurtosis(`Exportaciones comerciales`),abs(sd(`Exportaciones comerciales`)/mean(`Exportaciones comerciales`)));expo

impo = c((mean(`Importaciones Comerciales`)),median(`Importaciones Comerciales`),sd(`Importaciones Comerciales`),skewness(`Importaciones Comerciales`),kurtosis(`Importaciones Comerciales`),abs(sd(`Importaciones Comerciales`)/mean(`Importaciones Comerciales`)));impo

reservas = c((mean(`Total de reservas menos oro`)),median(`Total de reservas menos oro`),sd(`Total de reservas menos oro`),skewness(`Total de reservas menos oro`),kurtosis(`Total de reservas menos oro`),abs(sd(`Total de reservas menos oro`)/mean(`Total de reservas menos oro`)));reservas

cuenta_corriente = c((mean(`Balance de cuenta corriente`)),median(`Balance de cuenta corriente`),sd(`Balance de cuenta corriente`),skewness(`Balance de cuenta corriente`),kurtosis(`Balance de cuenta corriente`),abs(sd(`Balance de cuenta corriente`)/mean(`Balance de cuenta corriente`)));cuenta_corriente

#tabla calculo de las medidas para DataBase
dataframe1=data.frame(medidas,pibm,expo,impo,reservas,cuenta_corriente);dataframe1

#ENCONTRANDO EL VECTOR DE MEDIAS

medias_DataBase=colMeans(DataBase[2:6]);medias_DataBase

var_DataBase = round(var(DataBase[2:6]),2)

vtr_medias = data.frame(medias_DataBase);vtr_medias

#MATRIZ DE VARIANZAS Y COVARIANZAS

mvarcov_DataBase=cov(DataBase[,2:6]);mvarcov_DataBase

dataframe4 = data.frame(mvarcov_DataBase);dataframe4

#RANGO DE UNA MATRIZ

rang_DataBase=qr(mvarcov_DataBase)$rank; rang_DataBase



#Tabla Analisis Descriptivo
## medianas, medas, meda/mediana

medidas=c("Medianas","Medas","meda/mediana")
PIBt=c(median(PIB),median(round(abs(PIB-mean(PIB)),2)),median(round(abs(PIB-mean(PIB))/(median(PIB)),2)));PIBt

expot=c(median(`Exportaciones comerciales`),median(round(abs(`Exportaciones comerciales`-mean(`Exportaciones comerciales`)),2)),median(round(abs(`Exportaciones comerciales`-mean(`Exportaciones comerciales`))/(median(`Exportaciones comerciales`)),2)));expot

impot=c(median(`Importaciones Comerciales`),median(round(abs(`Importaciones Comerciales`-mean(`Importaciones Comerciales`)),2)),median(round(abs(`Importaciones Comerciales`-mean(`Importaciones Comerciales`))/(median(`Importaciones Comerciales`)),2)));impot

rest=c(median(`Total de reservas menos oro`),median(round(abs(`Total de reservas menos oro`-mean(`Total de reservas menos oro`)),2)),median(round(abs(`Total de reservas menos oro`-mean(`Total de reservas menos oro`))/(median(`Total de reservas menos oro`)),2)));rest

ccorrientet=c(median(`Balance de cuenta corriente`),median(round(abs(`Balance de cuenta corriente`-mean(`Balance de cuenta corriente`)),2)),median(round(abs(`Balance de cuenta corriente`-mean(`Balance de cuenta corriente`))/(median(`Balance de cuenta corriente`)),2)));ccorrientet

dataframe2=data.frame(medidas,PIBt,expot,impot,rest,ccorrientet);dataframe2



#Varianza total
#Suma de las varianzas de cada una de las variables, es decir, la traza de la matriz de covarianza

Vartotal = sum(diag(mvarcov_DataBase));Vartotal

#Varianza media
#Promedio de las varianzas
var_media=mean(diag(mvarcov_DataBase));var_media

#La varianza generaliza es el determinante de la matriz de covarianzas de la matriz de datos
##Varianza Generalizada##
VarGeneralizada = det(mvarcov_DataBase);VarGeneralizada# Mejor medida de la variabilidad global


#Valores y vectores propios de la matriz de varianzas y cova

eigen(mvarcov_DataBase)
valprop_DataBase=eigen(mvarcov_DataBase)$values;valprop_DataBase
vecprop_DataBase=eigen(mvarcov_DataBase)$vectors;vecprop_DataBase



#MATRIZ DE CORRELACIONES


medias_DataBase=colMeans((DataBase[2:6]));medias_DataBase# vector de medias

round(medias_DataBase,3)

mvarcov_DataBase=cov(DataBase[2:6]);mvarcov_DataBase

round(mvarcov_DataBase[2:6],3)

matriz_11=cbind(log(PIB,exp(1)),log(`Exportaciones comerciales`,exp(1)),log(`Importaciones Comerciales`,exp(1)),log(`Total de reservas menos oro`,exp(1)),log(`Balance de cuenta corriente`,exp(1)));matriz_11

round(cov(matriz_11),2)


par(mfrow=c(2,3))

hist(PIB,xlab="PIB",ylab="frecuencia",main="Histograma para PIB",col="red")
hist(`Exportaciones comerciales`,xlab="Exportaciones comerciales",ylab="frecuencia",main="Histograma para Exportaciones comerciales",col="pink")
hist(`Importaciones Comerciales`,xlab="Importaciones Comerciales",ylab="frecuencia",main="Histograma para Importaciones comerciales",col="green")
hist(`Total de reservas menos oro`,xlab="Total de reservas menos oro",ylab="frecuencia",main="Histograma para Total de reservas menos oro",col="blue")
hist(`Balance de cuenta corriente`,xlab="Balance de cuenta corriente",ylab="frecuencia",main="Histograma para Balance de cuenta corriente",col="gray")

is.matrix(matriz_11)

mvarcov_DataBase=cov(DataBase[2:6]);mvarcov_DataBase


mvarcov_DataBase=cov(matriz_11)

mvarcov_DataBase


#Grafica de las variables 


plot((DataBase[2:6]))

plot(DataBase)

mcorr_DataBase=cor(DataBase[2:6]);mcorr_DataBase

#Análisis de la matriz de correlación

order(mcorr_DataBase, decreasing = FALSE)

mcorr_DataBase=as.vector(mcorr_DataBase)[order(mcorr_DataBase)]#matriz de correlación ordenada de menor a mayor
mcorr_DataBase
#Las dos variables con menor correlacion positiva

#La menor correlación se puede observar entre Balance de cuenta corriente  y Total de reservas menos oro

#Gráfica de Balance de cuenta corriente  y Total de reservas menos oro

plot( `Total de reservas menos oro`,`Balance de cuenta corriente`, main = "Gráfico de Dispersión",col=4)

#Las dos variables con mayor correlacion positiva

#La mayor correlación se puede observar entre  Importaciones Comerciales y Exportaciones comerciales 

#Gráfica de Importaciones Comerciales y Exportaciones comerciales 

plot(`Importaciones Comerciales`,`Exportaciones comerciales` , main = "Gráfico de Dispersión", col = 3)


#Graficas de las variables medifis y sus correlaciones

library(GGally)
ggpairs(DataBase[2:6], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")





plot(PIB,`Exportaciones comerciales`, main = "PIB-Exportaciones comerciales", xlab = "PIB",ylab = "Exportaciones comerciales")

cor( DataBase[3],DataBase[2])

plot(PIB,`Importaciones Comerciales`, main = "PIB-Importaciones Comerciales", xlab = "PIB",ylab = "Importaciones comerciales")

cor( DataBase[4],DataBase[2])

plot(PIB,`Total de reservas menos oro`, main = "PIB-Total de reservas menos oro", xlab = "PIB",ylab = "Total de reservas menos oro")

cor( DataBase[5],DataBase[2])

plot(PIB,`Balance de cuenta corriente`, main = "PIB-Balance de cuenta corriente", xlab = "PIB",ylab = "Balance de cuenta corriente")

cor( DataBase[6],DataBase[2])

plot(`Importaciones Comerciales`,`Exportaciones comerciales`, main = "Importaciones Comerciales-Exportaciones comerciales", xlab = "Importaciones Comerciales",ylab = "Exportaciones comerciales")

cor( DataBase[4],DataBase[3])

plot(`Total de reservas menos oro`,`Balance de cuenta corriente`, main = "Total de reservas menos oro-Balance de cuenta corriente", xlab = "Total de reservas menos oro",ylab = "Balance de cuenta corriente")

cor( DataBase[6],DataBase[5])

#HISTOGRAMAS

hist(PIB,freq= FALSE,col="red")
lines(density(PIB),col="blue",main="Histograma de est")
hist(`Exportaciones comerciales`,freq = FALSE,col="blue")
hist(`Importaciones Comerciales`,freq = FALSE,col = "gray")
hist(`Total de reservas menos oro`,freq = FALSE,col="yellow")
hist(`Balance de cuenta corriente`,freq = FALSE,col="green")
multi.hist(x = DataBase[2:6], dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")


boxplot(PIB, xlab="PIB", ylab = "dólares")
boxplot(`Exportaciones comerciales`, xlab="Exportaciones comerciales", ylab = "dólares")
boxplot(`Importaciones Comerciales`, xlab="Importaciones comerciales", ylab = "dólares")
boxplot(`Total de reservas menos oro`, xlab="Total de reservas menos oro", ylab = "dólares")
boxplot(`Balance de cuenta corriente`, xlab="Balance de cuenta corriente", ylab = "dólares")

#Distancias Euclideas

round(dist(DataBase[2:6], method = "euclidean", diag = FALSE, upper = FALSE,p=6),2)

round(dist(t(DataBase[2:6])),2)

#Distancia de Mahalanobis

##Estandarización de matriz de datos

estndr_DataBase = as.data.frame(scale(DataBase[2:6]))

cov(estndr_DataBase)

cor(estndr_DataBase)

#distancia DE Mahalanobis

dmahalanobis_DataBase = sqrt(mahalanobis(DataBase[2],colMeans(DataBase[2]),cov(DataBase[2])))# dist maha para la est

dmahalanobis_DataBase[order(dmahalanobis_DataBase,decreasing = TRUE)]

boxplot(dmahalanobis_DataBase, ylab = "Distancias de Mahalanobis")



View(DataBase)

pairs(DataBase[2:6])

cor(DataBase[2:6])

regresion = lm(PIB ~ `Exportaciones comerciales`, data = DataBase); regresion

summary(regresion)

# Y= -2.689e+11+1.104e+01x

plot(DataBase[2:6]$`Exportaciones comerciales`, DataBase[2:6]$PIB, xlab='Exportaciones comerciales', ylab='PIB')

abline(regresion,col=130)

confint(regresion)

confint(regresion, level = 0.90)

anova(regresion)

residuos = rstandard(regresion)

valores.ajustados = fitted(regresion)

plot(valores.ajustados, residuos)

qqnorm(residuos,col="red")

qqline(residuos,col=4)

dataframe3 = data.frame(anova(regresion));dataframe3

