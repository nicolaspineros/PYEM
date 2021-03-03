
library(dplyr)

View(state.x77)

?state.x77

head(state.x77)

str(state.x77)

datos = as.data.frame(state.x77)

datos = rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)
# las variables con palabaras separadas deben ir entre comillas

View(datos)

#1.Analizar la relación entre variables

#estudiar la relación que existe entre variables

#Esta información es crítica a la hora de identificar cuáles pueden ser 
#los mejores predictores para el modelo, qué variables presentan relaciones 
#de tipo no lineal (por lo que no pueden ser incluidas) y para identificar
#colinialidad entre predictores. A modo complementario
#es recomendable representar la distribución de cada variable mediante histogramas.

round(cor(x = datos, method = "pearson"), 3)

library(psych)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#Del análisis preliminar se pueden extraer las siguientes conclusiones:
  
#  Las variables que tienen una mayor relación lineal con la esperanza de vida son:
#asesinatos (r= -0.78), analfabetismo (r= -0.59) y universitarios (r= 0.58).
#Asesinatos y analfabetismo están medianamente correlacionados (r = 0.7) 
#por lo que posiblemente no sea útil introducir ambos predictores en el modelo.
#Las variables habitantes, área y densidad poblacional muestran
#una distribución exponencial,
#una transformación logarítmica posiblemente haría más normal su distribución.

######

#2.Generar el modelo

datos = mutate(.data = datos, densidad_pobl = habitantes * 1000 / area)

modelo = lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos +
               universitarios + heladas + area + densidad_pobl, data = datos )
summary(modelo)

#El modelo con todas las variables introducidas como predictores tiene un R^2 alto
#(0.7501)
#es capaz de explicar el 75,01% de la variabilidad observada en la esperanza de vida.

#################
#3.Selección de los mejores predictores

#En este caso se van a emplear la estrategia de stepwise mixto.
#El valor matemático empleado para determinar la calidad del modelo va a ser Akaike(AIC).

step(object = modelo, direction = "both", trace = 1)

#El mejor modelo resultante del proceso de selección ha sido:

modelo = (lm(formula = esp_vida ~ habitantes + asesinatos + universitarios +
                heladas, data = datos))
summary(modelo)

#Es recomendable mostrar el intervalo de confianza para cada uno 
#de los coeficientes parciales de regresión:

confint(lm(formula = esp_vida ~ habitantes + asesinatos + universitarios +
             heladas, data = datos))

#######################################

#Cada una de las pendientes de un modelo de regresión lineal múltiple

#(coeficientes parciales de regresión de los predictores) 

#se define del siguiente modo: 

#Si el resto de variables se mantienen constantes, 

#por cada unidad que aumenta el predictor en cuestión, 

#la variable (Y) varía en promedio tantas unidades como indica la pendiente.

#Para este ejemplo, por cada unidad que aumenta el predictor universitarios,

#la esperanza de vida aumenta en promedio 0.04658 unidades, 

#manteniéndose constantes el resto de predictores.

#################################################

#4.Validación de condiciones para la regresión múltiple lineal

#Relación lineal entre los predictores numéricos y la variable respuesta:

#Esta condición se puede validar bien mediante diagramas de dispersión

#entre la variable dependiente y cada uno de los predictores

#o con diagramas de dispersión entre cada uno de los predictores y los residuos del modelo.

#Si la relación es lineal, los residuos deben de distribuirse aleatoriamente

#en torno a 0 con una variabilidad constante a lo largo del eje X.

library(ggplot2)

library(gridExtra)

plot1 = ggplot(data = datos, aes(habitantes, modelo$residuals)) +
  geom_point(col=19) + geom_smooth(color = 4) + geom_hline(yintercept = 0) +
  theme_bw()

plot2 = ggplot(data = datos, aes(asesinatos, modelo$residuals)) +
  geom_point(col=20) + geom_smooth(color = 2) + geom_hline(yintercept = 0) +
  theme_bw()

plot3 = ggplot(data = datos, aes(universitarios, modelo$residuals)) +
  geom_point(col = 21) + geom_smooth(color = 3) + geom_hline(yintercept = 0) +
  theme_bw()

plot4 = ggplot(data = datos, aes(heladas, modelo$residuals)) +
  geom_point(col=24) + geom_smooth(color = 5) + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2, plot3, plot4)

#Se cumple la linealidad para todos los predictores

#Distribución normal de los residuos:

qqnorm(modelo$residuals, col= 17, xlab = "Cuantiles teóricos", ylab="Cuantiles de muestra")

qqline(modelo$residuals, col=6)

?qqnorm

shapiro.test(modelo$residuals) # p-value > 0.1, no se rechaza la h0 de normailidad

#Tanto el análisis gráfico como es test de hipótesis confirman la normalidad.

#Variabilidad constante de los residuos (homocedasticidad):

#Al representar los residuos frente a los valores ajustados por el modelo,

#los primeros se tienen que distribuir de forma aleatoria en torno a cero,

#manteniendo aproximadamente la misma variabilidad a lo largo del eje X.

#Si se observa algún patrón específico,

#por ejemplo forma cónica o mayor dispersión en los extremos, 

#significa que la variabilidad es dependiente del valor ajustado

#y por lo tanto no hay homocedasticidad.

ggplot(data = datos, aes(modelo$fitted.values,modelo$residuals),) +
  geom_point(col=10) +
  geom_smooth(color = 5, se = F) +
  geom_hline(yintercept = 0) +
  theme_bw()

library(lmtest)

bptest(modelo)

#No hay evidencias de falta de homocedasticidad.

#No multicolinialidad:

  
##  Matriz de correlación entre predictores.

library(corrplot)
corrplot(cor(dplyr::select(datos, habitantes, asesinatos,universitarios,heladas)),
         method = "number", tl.col = "black")

#Análisis de Inflación de Varianza (VIF):

library(car)

vif(modelo)

#No hay predictores que muestren una correlación lineal muy alta ni inflación de varianza.

#Autocorrelación:

dwt(modelo, alternative = "two.sided")

#No hay evidencia de autocorrelación

#6.Conclusión

#El modelo lineal múltiple

#es capaz de explicar el 73.6% de la variabilidad observada en la esperanza de vida

#(R2: 0.736, R2-Adjusted: 0.7126). 

#El test F muestra que es significativo (p-value: 1.696e-12). 

#Se satisfacen todas las condiciones para este tipo de regresión múltiple.

