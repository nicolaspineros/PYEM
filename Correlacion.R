ibrary(moments)
library(stats)
library(ggplot2)
library(dplyr)
library(psych)
install.packages("ppcor")
library(ppcor)
attach(medifis)
View(medifis)

#MATRIZ DE CORRELACIONES

#Grafica de las variables 

plot((medifis[2:8]))

plot(ACCIONES)

mcorr_medifis=cor(medifis[2:8])


#Análisis de la matriz de correlación

order(mcorr_medifis, decreasing = FALSE)

mcord_medifis=as.vector(mcorr_medifis)[order(mcorr_medifis)]#matriz de correlación ordenada de menor a mayor

#Se observa que los MENORES valores corresponden a la correlación entre

#dcr y lbr con r=0.4749471 y la correlación entre lpie y dcr con r= 0.5478035,

#indicando que existe una pobre relación directa entre las variables indicadas.

#Asimismo el mayor valor positivo (diferente de 1) es est y lpie con r= 0.9278572 indicando que existe una relación positiva entre dichas variables

#Gráfica de dcr y plot

plot(dcr,lbr, main = "Gráfico de Dispersión",col=4)

#Las dos variables con mayor correlacion positiva

#La mayor correlación se puede observar entre est y lpie

#Gráfica de est y lpie

plot(est,lpie, main = "Gráfico de Dispersión", col = 10)


cor( medifis[5],medifis[3]) # correlación entre var 5 y var 3 (lbr y peso)

plot()

#Graficas de las variables medifis y sus correlaciones

library(GGally)
ggpairs(medifis[2:8], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

##Dependencia directa entre pares: Correlaciones parciales

#La dependencia directa entre dos variables controlando el efecto de las restantes se mide por
#el coeÞciente de correlación parcial. 

#


pcor(medifis,method = "pearson")
