
#library(dplyr)

View(state.x77)

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

#library(psych)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

#library(GGally)
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

step(object = modelo, direction = "both", trace = 1)

#El mejor modelo resultante del proceso de selección ha sido:

modelo = (lm(formula = esp_vida ~ habitantes + asesinatos + universitarios +
                heladas, data = datos))
summary(modelo)
