###Taller de Estadística - Algebra matricial 
###Matrices

###Ejercicio 1.1

#Crear las matrices

A = matrix(c(6,3,1,8), nrow=2, ncol=2);A
B = matrix(c(2,0,2,7), nrow=2, ncol=2);B
C = matrix(c(5,9,-3,0,6,7,0,0,2), nrow=3, ncol=3);C
D = matrix(c(4,0,9,-2,3,-1), nrow=3, ncol=2);D

#Calcular si es posible, los siguientes productos:

#a. AB
A%*%B
#R. Si es posible

#b. BA
B%*%A
#R. Si es posible

#c. AC
A%*%C
#R. No es posible, el número de columnas de la matriz A (2), difiere del número de filas
#en la matriz C (3)

#d. CA
c%*%A
#R. No es posible, el número de columnas de la matriz C (3), difiere del número número

#e. AD
A%*%D
#R. No es posible, el número de columnas de la matriz A (2), difiere del número de filas
#en la matriz D (3)

#f. DA
D%*%A
#R. Si es posible

#g. CD
C%*%D
#R. Si es posible

#h. DC
D%*%C
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

TrazaA = sum(diag(A));TrazaA
TrazaB = sum(diag(B));TrazaB

#Probar que si A y B son matrices cuadradas de orden n, entonces:

#a. Tr (A-B) = Tr(A) - Tr(B)

#Crear matriz A-B
A-B
AmenosB = matrix(c(4,3,-1,1), nrow=2, ncol=2);AmenosB
TrazaAmenosB = sum(diag(AmenosB));TrazaAmenosB
TrazaA-TrazaB

#R. La Tr (A-B) es igual a la Tr(A) - Tr(B) porque:
#Tr (A-B) = 5
#Tr (A) - Tr(B) = 5


#b. Tr(A*B) = Tr (B*A)
#Crear matriz A*B y crear matriz B*A
A%*%B
AxB = matrix(c(12,6,19,62), nrow=2, ncol=2)
B%*%A
BxA = matrix(c(18,21,18,56), nrow=2, ncol=2)

TrazaAxB = sum(diag(AxB));TrazaAxB
TrazaBxA = sum(diag(BxA));TrazaBxA

#R. La Tr(A*B) es igual a la Tr(B*A)
#Tr(A*B) = 74
#Tr(B*A) = 74

#c. A*B - B*A diferente In, siendo In la matriz unidad de orden n
#Asumiendo que In es la identica de la matriz A o de la matriz B, se responde la pregunta

(A%*%B)-(B%*%A)
solve(A)%*%A
solve(B)%*%B

#R. A*B - B*A es diferente de In, tanto para la matriz A como para la matriz B 

###Ejercicio 1.5 Determinantes

#a. Calcular el determinante para la matriz que denominarÃ© E

E = matrix(c(2,4,0,-7,8,-2,3,-5,10), nrow=3, ncol=3);E
det(E)

#R. Determinante de la matriz E es igual a 396

###Ejercicio 1.9 Calcular inversa de dos matrices
#a. Inversa para la matriz 
#Crear matriz MF
Mf = matrix(c(11,-6,0,-1,10,-5,4,-1,8), nrow=3, ncol=3);Mf
solve(Mf)

#R. La inversa de la matriz Mf es igual a c()

#b. Inversa para la matriz 

Ml = matrix(c(7,-28,13,6,-10,7,0,-2,18,-19,15,39,-14,-12,1,6), nrow=4, ncol=4);Ml
solve(Ml)

#R. La inversa de la matriz Ml es igual a c()

# Matrices

A=matrix(c(4,2,5,1), nrow=2, ncol=2);A

B=matrix(c(6,4,9,2), nrow=2, ncol=2);B

C=matrix(c(5,1,4,9), nrow=2, ncol=2);C

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
traza

#Norma
x=matrix(c( 0.9284767, 0.3713907), nrow=2, ncol=1);x
sqrt((0.9284767)^2+(0.3713907)^2)

# Calcular los vectores y valores propios 
A
eigen(A)
lambda1=eigen(A)$values[1];lambda1
lambda2=eigen(A)$values[2];lambda2

vecpropio1=eigen(A)$vector[,1];vecpropio1
vecpropio2=eigen(A)$vector[,2];vecpropio2

var(A)

# A\lambda=\lambdau
A%*%vecpropio1   
lambda1*vecpropio1

A%*%vecpropio2
lambda2*vecpropio2
