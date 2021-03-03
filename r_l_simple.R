
#REGRESIÓN LINEAL SIMPLE



grasas=reg_lin_simp;grasas

View(grasas)

#Con el fin de conocer las relaciones existentes entre 
#cada par de variables podemos representar una matriz de diagramas de dispersión.

pairs(grasas)

#Al parecer existe una relación lineal bastante clara entre la edad y las grasas,
#pero no entre los otros dos pares de variables. 
#Por otra parte el fichero contiene un dato atípico.

#Para cuantificar el grado de relación lineal, calculamos la matriz de coeficientes 
#de correlación:

cor(grasas)

regresion = lm(grasas ~ edad, data = grasas); regresion#~

summary(regresion)

#y=102.575+5.321x

plot(grasas$edad, grasas$grasas, xlab='Edad', ylab='Grasas')

abline(regresion,col=130)

nuevas.edades = data.frame(edad = seq(30, 50));nuevas.edades

predict(regresion, nuevas.edades)

confint(regresion)

confint(regresion, level = 0.90)

nuevas.edades = data.frame(edad = seq(20, 60))

# Grafico de dispersion y recta

plot(grasas$edad, grasas$grasas, xlab='Edad', ylab='Grasas')

abline(regresion,col="red")

# Intervalos de confianza de la respuesta media:

# ic es una matriz con tres columnas: la primera es la prediccion, las otras dos son los extremos del intervalo

ic = predict(regresion, nuevas.edades, interval = 'confidence')

lines(nuevas.edades$edad, ic[, 2], lty = 2,col=4)

lines(nuevas.edades$edad, ic[, 3], lty = 2,col=23)

# Intervalos de prediccion

ic = predict(regresion, nuevas.edades, interval = 'prediction')

lines(nuevas.edades$edad, ic[, 2], lty = 2, col = 'red')

lines(nuevas.edades$edad, ic[, 3], lty = 2, col = 'red')

anova(regresion)

residuos = rstandard(regresion)

valores.ajustados = fitted(regresion)

plot(valores.ajustados, residuos)

qqnorm(residuos,col="red")

qqline(residuos,col=4)
