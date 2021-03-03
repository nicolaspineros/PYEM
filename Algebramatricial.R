
#Matrices, determinantes y valores y vectores propios

Mc = -matrix(0,2,2) #Mc es la matriz cero

Mc

E1<-matrix(1:9,3,3)

E1

E2<-matrix(1:9,3,3,byrow=TRUE) # la elabora por fila

E2

E3=matrix(1:10,3,3) #Presenta advertencia

E3

zz=c(1,2,3,4,5)

zz

ww=c("frio", "caliente", "Juana", "Carlos")

ww

E4=matrix(c("hola", "adios"),2,2)

E4

E5=matrix(c("hola", "adios"),2,2,byrow=TRUE)

E5

vec = sample(1:20,20,replace = TRUE)

vec

E6= matrix(vec, nrow = 4, ncol = 5, byrow = FALSE)# la ordena por columna

E6

#Accediendo a elementos de una matriz

E6[2,1]    # Renglón 2 columna 1]

E6[4,5]

E6[1,]     # Toda la fila 1 

E6[,4]     # la columna 4 

# Cambiar el orden de una matrix mediante t()

E6

trnE6=t(E6)# matriz transpuesta de E6

trnE6

trnE6[5,4]

#Operaciones matematicas con matrices

2*E6 #Escalar por matriz

E6

E6^2

E6*E6

mat1= matrix(sample(1:9, 9, replace = TRUE), nrow = 3, ncol = 3, byrow = FALSE)

mat1

mat2= matrix(sample(1:9, 9, replace = TRUE), nrow = 3, ncol = 3, byrow = FALSE)

mat2

mat3= mat1 + mat2 # Siuma de matrices

mat3

mat4= mat1 * mat2

mat4

E6

colMeans(E6)    # Promedio por columna

c1= mean(c(E6[1,1], E6[2,1], E6[3,1], E6[4,1])) #Media de col 1

c1

rowMeans(E6) # Media por renglón

r1=mean(c(E6[1,1], E6[1,2], E6[1,3], E6[1,4], E6[1,5]))

r1

#Agregando una nueva columna a E6

E6

cbind(E6, c(10, 9, 14, 19)) # Solo muestra la nueva columna en E6

E6 <- cbind(E6, c(10, 9, 14, 19)) # Agrega definitivamente la nueva columna

#Agregando nuevo renglón a E6

E6

rbind(E6, c(12, 8, 6, 18, 9, -1)) # Solo muestra nuevo Ren

#Agregando las medias de renglones y columnas a E6
# Nueva columna con media de renglones

E6 = cbind(E6, rowMeans(E6)) # Agrega vector de medias de E6 por fila

E6

E6[1,7]

E6 <- rbind(E6, colMeans(E6)) # Agrega vector de medias de E6 por columna

E6

E6[5,1]

#Función apply con matrices

E6a=matrix(c(3,4,5,6,8,4,1,-1,0), nrow=3, ncol= 3)

E6a

apply(E6a, 1, mean)# media por fila (1)

apply(E6a, 2, mean)# media por columna (2)

# Agregando con rbind y cbind nuevo renglón y nueva columna con apply

E6a <- cbind(E6a, apply(E6a, 1, mean))

E6a

E6a <- rbind(E6a, apply(E6a, 2, mean))

E6a

#Encontrando la inversa de una matriz

Mf <- matrix(c(1,1,0,-1,3,-1,1,-2,1), nrow=3, ncol=3)

invMF=solve(Mf)

#R. La inversa de la matriz Mf es igual a c(1,-1,-1,0,1,1,-1,3,4)

#b. Inversa para la matriz l

Ml <- matrix(c(0,-1,2,6,-1,1,0,-1,1,-1,1,3,-1,-1,1,4), nrow=4, ncol=4)

invMl=solve(Ml)

#R. La inversa de la matriz Ml es igual a c(1,-1,-1,-1,3,-2,-4,-2,-4,5,7,2,2,-2,-3,-1)
# Matrices

A=matrix(c(6,3,3,4), nrow=2, ncol=2)

A

B=matrix(c(6,8,7,9), nrow=2, ncol=2)

B

C=matrix(c(2,1,1,2), nrow=2, ncol=2)

C

# Operaciones entre matrices

A+B

A*B

A%*%B

A2<-A%*%A     # Para calcular el cuadrado de la matriz A.

A2

#Entonces si se desea la matriz A al cubo, se tendría que usar dos veces (3-1) el comando %*%, la sintaxis sería así.

A3<-A%*%A%*%A   # Para calcular la matriz cubica de A.

A3

#Pero como se señalo este procedemiento sería ineficiente 
#si se desea un exponente alto, por ejemplo 10.
#Para solucionar esto, instalaremos el paquete "Biodem".

install.packages("Biodem")

library(Biodem)

#Una vez cargado usamos el comando mtx.exp(). Que tiene la siguiente sintaxis.
#mtx.exp(X, n) donde X es la matriz que se desea elevar a la potencia, y n es el orden del exponente. Si deseamos calcular la matriz A al cuadrado,
#se tendría que realizar la siguiente sintaxis.

AA<-mtx.exp(A,2)  # La matriz A elevado al cuadrado.

# Comprobemos si es igual al hallado con el comando %*%.

AA

AA==A2

#Ahora, si deseamos elevar la matriz al exponente 10.

A10<-mtx.exp(A,10)   # La matriz A elevado a la 10.

A10

#Conceptos importantes de una matriz

ivc=solve(C) #Inversa de C

I=solve(C)%*%C

C%*%solve(C)

det(C) #Determinante de C

round(ivc,2)

A

det(A)

eigen(A)

diag(A)

traza=sum(diag(A))

#Norma

x=matrix(c( -0.8112422, -0.5847103), nrow=2, ncol=1)

sqrt((-0.8112422)^2+(-0.5847103)^2)


# Calcular los vectores y valores propios 

A

eigen(A)

lambda1=eigen(A)$values[1]

lambda2=eigen(A)$values[2]

lambda1

lambda2

vecpropio1=eigen(A)$vector[,1]

vecpropio2=eigen(A)$vector[,2]

vecpropio1

vecpropio2


# A\lambda=\lambdau

A%*%vecpropio1   

lambda1*vecpropio1

A%*%vecpropio2

lambda2*vecpropio2








###Taller de Estadística - Algebra matricial 
###Matrices

###Ejercicio 1.1

#Crear las matrices

A <- matrix(c(1,-1,2,-1), nrow=2, ncol=2)
B <- matrix(c(1,0,1,1), nrow=2, ncol=2)
C <- matrix(c(1,0,3,0,1,1,0,0,2), nrow=3, ncol=3)
D <- matrix(c(1,0,3,-2,1,-1), nrow=3, ncol=2)

#Calcular si es posible, los siguientes productos:

#a. AB
A%*%B
#R. Si es posible

#b. BA
B%*%A
#R. Si es posible

#c. AC
#R. No es posible, el número de columnas de la matriz A (2), difiere del número de filas
#en la matriz C (3)

#d. CA
#R. No es posible, el número de columnas de la matriz C (3), difiere del número número

#e. AD
#R. No es posible, el número de columnas de la matriz A (2), difiere del número de filas
#en la matriz D (3)

#f. DA
D%*%A
#R. Si es posible

#g. CD
C%*%D
#R. Si es posible

#h. DC
#R. No es posible, el número de columnas de la matriz D (2), difiere del número de filas
#en la matriz C (3)

#i. AB diferente BA
A%*%B
B%*%A
#R. Si, el producto de la matriz A*B genera diferentes resultados que B*A



###Ejercicio 1.4 con base en la matriz del ejercicio 1.1 
#Si A=(aij) es una matriz cuadrada de orden n, entonces se define la traza de A, que
#notaremos Tr (A)

# Definir la traza de A y B

TrazaA <- sum(diag(A))
TrazaB <- sum(diag(B))

#Probar que si A y B son matrices cuadradas de orden n, entonces:

#a. Tr (A-B) = Tr(A) - Tr(B)

#Crear matriz A-B
A-B
AmenosB <- matrix(c(0,-1,1,-2), nrow=2, ncol=2)
TrazaAmenosB <- sum(diag(AmenosB))
TrazaA-TrazaB

#R. La Tr (A-B) es igual a la Tr(A) - Tr(B) porque:
#Tr (A-B) = -2
#Tr (A) - Tr(B) = -2


#b. Tr(A*B) = Tr (B*A)
#Crear matriz A*B y crear matriz B*A
A%*%B
AxB <- matrix(c(1,-1,3,-2), nrow=2, ncol=2)
B%*%A
BxA <- matrix(c(0,-1,1,-1), nrow=2, ncol=2)

TrazaAxB <- sum(diag(AxB))
TrazaBxA <- sum(diag(BxA))

#R. La Tr(A*B) es igual a la Tr(B*A)
#Tr(A*B) = -1
#Tr(B*A) = -1


#c. A*B - B*A diferente In, siendo In la matriz unidad de orden n
#Asumiendo que In es la identica de la matriz A o de la matriz B, se responde la pregunta
 
(A%*%B)-(B%*%A)
solve(A)%*%A
solve(B)%*%B

#R. A*B - B*A es diferente de In, tanto para la matriz A como para la matriz B 


###Ejercicio 1.5 Determinantes

#a. Calcular el determinante para la matriz que denominarÃ© E

E <- matrix(c(1,0,1,-1,1,-2,1,-1,3), nrow=3, ncol=3)
det(E)

#R. Determinante de la matriz E es igual a 1

###Ejercicio 1.9 Calcular inversa de dos matrices del ejercicio 1.9

#a. Inversa para la matriz f
#Crear matriz MF
####################################################
Mf <- matrix(c(1,1,0,-1,3,-1,1,-2,1), nrow=3, ncol=3)
solve(Mf)

#R. La inversa de la matriz Mf es igual a c(1,-1,-1,0,1,1,-1,3,4)

#b. Inversa para la matriz l

Ml <- matrix(c(0,-1,2,6,-1,1,0,-1,1,-1,1,3,-1,-1,1,4), nrow=4, ncol=4)
solve(Ml)

#R. La inversa de la matriz Ml es igual a c(1,-1,-1,-1,3,-2,-4,-2,-4,5,7,2,2,-2,-3,-1)
# Matrices

A=matrix(c(6,3,3,4), nrow=2, ncol=2)
A

B=matrix(c(6,8,7,9), nrow=2, ncol=2)
B

C=matrix(c(2,1,1,2), nrow=2, ncol=2)
C

# Operaciones entre matrices
A+B
A*B
A%*%B
solve(C)
solve(C)%*%C
C%*%solve(C)
det(C)
round(solve(C),2)
A
det(A)
eigen(A)
traza=sum(diag(A))

#Norma
x=matrix(c( -0.8112422, -0.5847103), nrow=2, ncol=1)
sqrt((-0.8112422)^2+(-0.5847103)^2)


# Calcular los vectores y valores propios 
A
eigen(A)
lambda1=eigen(A)$values[1]
lambda2=eigen(A)$values[2]
lambda1
lambda2
vecpropio1=eigen(A)$vector[,1]
vecpropio2=eigen(A)$vector[,2]
vecpropio1
vecpropio2
var(A)


# A\lambda=\lambdau
A%*%vecpropio1   
lambda1*vecpropio1

A%*%vecpropio2
lambda2*vecpropio2
####################################

#
# Para graficar el primer vector (vector m):
H<-matrix(c(3,8,10,2), nrow = 2)
H
plot(3,8, xlim=c(0,14), ylim=c(0,12), 
    
     xlab="Eje x", ylab="Eje y", lwd=3,
     col="blue", bty="n",
     main = "Gráfica de la Determinante de la matriz H")
axis(side = 1, 0:14)
xis(side = 2, 0:12)
arrows(0,0,3,8,col="blue")
abline(h = pretty(0:12, 12), v = pretty(0:14, 14), col = "lightgray")
# Para graficar el segundo vector (vector p):
points(10,2,lwd=3,col="green")
arrows(0,0,10,2,col="green")
# Para graficar el vector resultante de la adición de los 2 vectores (a=m+p)
points(13,10,lwd=3,col="black")
arrows(0,0,13,10,col="black")
# Agregando las líneas imaginarias par formar el paralelogramo que representa la adición de 2 vectores.
arrows(3,8,13,10,col="blue",lty=1)
arrows(10,2,13,10,col="green",lty=3)  
# Graficando el área.
polygon(c(0,3,13,10),c(0,8,10,2),
        col = rgb(0, 206/255, 209/255, 0.5),
        density = c(20, 40), angle = c(-45, 45))   
text(6.3,5.3, "DETERMINANTE\n DE LA \n MATRIZ H", col = "brown")
