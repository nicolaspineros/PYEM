# Taller 2 
## Integrantes: Vanessa Rozo, Nicolas Piñeros, Nicol Montañez, Valentina Arrieta
#Tabla 3.3 analisis descriptivo 

head(medifis)
attach(medifis)

names(medifis[2:8])#nombre de las variables cuantitativas

medias = round(apply(medifis[2:8],2,mean),1);medias

mediana = round(apply(medifis[2:8],2,median),1);mediana


medidas=c("Medianas","Medas","meda/mediana")
estat=c(median(est),(round(abs(est-mean(est)),2)),(round(abs(est-mean(est))/(median(est)),2)));estat

Pes=c(median(peso),(round(abs(est-mean(peso)),2)),(round(abs(est-mean(peso))/(median(peso)),2)));Pes

lopie=c(median(lpie),(round(abs(est-mean(lpie)),2)),(round(abs(est-mean(lpie))/(median(lpie)),2)));lopie

lobr=c(median(lbr),(round(abs(est-mean(lbr)),2)),(round(abs(est-mean(lbr))/(median(lbr)),2)));lobr

anes=c(median(aes),(round(abs(est-mean(aes)),2)),(round(abs(est-mean(aes))/(median(aes)),2)));anes

dmcr=c(median(dcr),(round(abs(est-mean(dcr)),2)),(round(abs(est-mean(dcr))/(median(dcr)),2)));dmcr

lnrt=c(median(lrt),(round(abs(est-mean(lrt)),2)),(round(abs(est-mean(lrt))/(median(lrt)),2)));lnrt

dataframe1=data.frame(medidas,estat,Pes,lopie,lobr,anes,dmcr,lnrt);dataframe1
