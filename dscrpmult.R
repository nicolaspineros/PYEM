
#library(moments)
#library(stats)
#library(ggplot2)

head(medifis)
attach(medifis)
#tabla 3.2
#ANÁLIS DESCRIPTIVO DE LAS MEDIDAS FÍSICAS DE MEDIFIS



names(medifis[2:8])#nombre de las variables cuantitativas

medias = round(apply(medifis[2:8],2,mean),1);medias

mediana = round(apply(medifis[2:8],2,median),1);mediana

D_estand = round(apply(medifis[2:8],2,sd),1);D_estand

Curtosis = round(apply(medifis[2:8],2,kurtosis),1);Curtosis

Coef_asim = round(apply(medifis[2:8],2,skewness),1);Coef_asim

Coef_var = round(D_estand/medias,1);Coef_var


medidas=c("Medias", "Mediana","Desv. Estándar","Coef. Asimetría","Coef. Curtosis","Coef.Variación")

desv_medifis=c(sd(est),sd(peso),sd(lpie),sd(lbr),sd(aes),sd(dcr),sd(lrt));desv_medifis#vector de desviaciones estándar

coefvar_medifis=c(abs(sd(est)/mean(est)),abs(sd(peso)/mean(peso)),abs(sd(lpie)/mean(lpie)),abs(sd(lbr)/mean(lbr)),abs(sd(aes)/mean(aes)),abs(sd(dcr)/mean(dcr)),abs(sd(lrt)/mean(lrt)));coefvar_medifis

mediana_medifis=c(median(est),median(peso),median(lpie),median(lbr),median(aes),median(dcr),median(lrt));mediana_medifis

kurtosis(medifis)[2:8]

estat=c((mean(est)),median(est),sd(est),skewness(est),kurtosis(est),abs(sd(est)/mean(est)));estat

Pes=c(mean(peso),median(peso),sd(peso),skewness(peso),kurtosis(peso),abs(sd(peso)/mean(peso)));Pes

lopie=c(mean(lpie),median(lpie),sd(lpie),skewness(lpie),kurtosis(lpie),abs(sd(lpie)/mean(lpie)));lopie

lobr=c(mean(lbr),median(lbr),sd(lbr),skewness(lbr),kurtosis(lbr),abs(sd(lbr)/mean(lbr)));lobr

anes=c(mean(aes),median(aes),sd(aes),skewness(aes),kurtosis(aes),abs(sd(aes)/mean(aes)));anes

dmcr=c(mean(dcr),median(dcr),sd(dcr),skewness(dcr),kurtosis(dcr),abs(sd(dcr)/mean(dcr)));dmcr

lnrt=c(mean(lrt),median(lrt),sd(lrt),skewness(lrt),kurtosis(lrt),abs(sd(lrt)/mean(lrt)));lnrt

dataframe1=data.frame(medidas,estat,Pes,lopie,lobr,anes,dmcr,lnrt);dataframe1


##################################
#d.est=c(sd(est),sd(peso),sd(lpie),sd(lbr),sd(aes),sd(dcr),sd(lrt))
#coef_asim=c(skewness(est),skewness(peso),skewness(lpie),sd(lbr),skewness(aes),skewness(dcr),skewness(lrt))
#coef_kurt=c(kurtosis(est),kurtosis(peso),kurtosis(lpie),kurtosis(lbr),kurtosis(aes),kurtosis(dcr),kurtosis(lrt))
#coef_asim=skewness(medifis)[2:8]
#coef_kurt=kurtosis(medifis)[2:8]
###############################################
#####################################DATA FRAMES###############


 

####################################
####MEDIDAS DE CENTRALIZACIÓN: VECTOR DE MEDIAS
#VECTOR DE MEDIAS

##########################
#ENCONTRANDO EL VECTOR DE MEDIAS

medias_medifis=colMeans(medifis[2:8])

vtr_medias=data.frame(var_medifis,medias_medifis)

#######################################
#MATRIZ DE VARIANZAS Y COVARIANZAS


mvarcov_medifis=cov(medifis[,2:8])

#RANGO DE UNA MATRIZ

rang_medifis=qr(mvarcov_medifis)$rank

################################
#TAREA 1 ELABORAR TABLA 3.3
desvabs_est=abs(est-mean(est))
medaest_medifis=median(abs(est-median(est)))#meda de las est


#VARIANZA TOTAL
######################################################
#Varianza varianza total y varianza media

#Varianza total
#Suma de las varianzas de cada una de las variables, es decir, la traza de la matriz de covarianza

Vartotal = sum(diag(mvarcov_medifis))

#Varianza media
#Promedio de las varianzas
var_media=mean(diag(mvarcov_medifis))

#La varianza generaliza es el determinante de la matriz de covarianzas de la matriz de datos
##Varianza Generalizada##
VarGeneralizada = det(mvarcov_medifis)# Mejor medida de la variabilidad global

#Valores y vectores propios de la matriz de varianzas y cova

eigen(mvarcov_medifis)
valprop_medifis=eigen(mvarcov_medifis)$values
vecprop_medifis=eigen(mvarcov_medifis)$vectors

#MATRIZ DE CORRELACIONES

mcorr_medifis=cor(medifis[,3:9])
#Análisis de la matriz de correlación

order(mcorr_medifis, decreasing = FALSE)

mcord_medifis=as.vector(mcorr_medifis)[order(mcorr_medifis)]#matriz de correlación ordenada de menor a mayor
#Se observa que los MENORES valores corresponden a la correlación entre
#dcr y lbr con r=0.4749471 y la correlación entre lpie y dcr con r= 0.5478035,
#indicando que existe una pobre relación directa entre las variables indicadas.
#Asimismo el mayor valor positivo (diferente de 1) es est y lpie con r= 0.9278572 indicando que existe una relación positiva entre dichas variables

cor( medifis[5],medifis[3])

#Las dos variables con mayor correlacion positiva

#VALORES Y VECTORES PROPIOS
##Las variables con menor correlación positiva
cor( medifis[8],medifis[6])#tabla original

####################################################
#Gráficos
plot(dcr,lbr, main = "Gráfico de Dispersión")
plot(est,lpie,main = "Gráfico de Dispersión")
par(mfrow=c(3,3))
hist(est,freq = FALSE,col="red")
lines(density(est),col="blue",main="Histograma de est")
hist(peso,freq = FALSE,col="blue")
hist(lpie,freq = FALSE,col = "gray")
hist(lbr,freq = FALSE,col="yellow")
hist(aes,freq = FALSE,col="green")
hist(dcr,freq = FALSE,col="orange")
hist(lrt,freq = FALSE,col="pink",ylab = "long rodilla a tobillo")
lines(density(est),col="red",main="Histograma de est")

####los histogramas #####
multi.hist(x = medifis[3:9], dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

#Variabilidad y distancias 


#Análisis gráfico para detectar los valores atípicos de cada variable
boxplot(est, xlab="estatura", ylab = "Número de estudiantes")
