# ---------------- Universidad Javeriana
# ---------------- Facultad de Ciencias 
# -------------- Deparamento de Matemáticas
# ------------ Tomas Charria

# ------------ Lectura de datos
library(tidyverse)
library(readxl)
datos <- read_excel("C:/Users/ciencias507/Downloads/P1_Mult/datasetP1.xlsx") # NO ALTERAR!


# ------------ DESARROLLO ------------#

#1. descripcion de un conjunto multivariado ( )
#entender que el mes no es un numero sino un factor por que no tiene sentido tartarlo como numero 
#- indicadores
#- graficos
#conversion a factores de si es privado o no y tambien la clasificacion de tecnologica que tenga
datos$Var5<- factor(datos$Var5)
datos$Var6<- factor(datos$Var6)

mean(table(datos$Var5))
#descriptivas acerca de los datos solo con summary
summary(datos)

#Graficos univariados
par(mfrow=c(2,3))
boxplot(datos$Var1, main="Consumo energ´etico del quir´ofano")
boxplot(datos$Var2,main="Velocidad promedio de procedimientos")
boxplot(datos$Var3,main="Tiempo promedio por cirug´ıa, en minutos.")
boxplot(datos$Var4,main="N´umero de complicaciones por cirug´ıa realizada.")
barplot(table(datos$Var5), main="Tipo de hospital")
barplot(table(datos$Var6), main="Clasificacionn de tecnologia")

par(mfro=c(2,2))
hist(datos$Var1, main="Consumo energ´etico del quir´ofano")
hist(datos$Var2,main="Velocidad promedio de procedimientos")
hist(datos$Var3,main="Tiempo promedio por cirug´ıa, en minutos.")
hist(datos$Var4,main="N´umero de complicaciones por cirug´ıa realizada.")

#Con summary y lo graficos podemos encontrar que el consumo una descripcion de cada variable 
# Como que el consumo energetico tienen media de 25.72, la velocidad promedio es de 1.985 el tiempo promedio por cirugia es 
# de 86.8 minutos que el numero de complicaciones en media por circujia es de 1.068 y ver que 
# hay 67 hopitales privados y 50 publicos, tambien que hay 38 baja tecnologia, 47 con media tecnologia y 32 con alta tecnologia
#Ahora con las graficas podemos ir detallando cada una de las variables, empezando por el consummo de energia del quirofano
# se nos muestra una grafica bastante simeptrica distribuyendose en valores iguales desde la mitad el final del primer cuantil como al final del cuarto,
#en la velocidad promedio de procedimientos tambien encontramos que se distribuyen de una forma simetrica teniendo cada cuatril aproximadamente 1 unidad
#en el tiempo promedio por ciruguia encontramos que no es tan simetrica ya que un 50 porciento de los datos van a estar entre 
# aproximadamente 20 minutos y 96 minutos mientras que el otro 50 porciento va estar entre 96 y 144 dejando ver como se comporta la variable
#en el numero de complicaciones vemos que un 50 porciento esta entre 0 y 1 pero hay casos de hasta 5 complicaciones

# revision noramlidad multivariada, 

# ------ Uso del recurso MVN completo
require(MVN)
mvn_analisis <- mvn(data = datos[,1:4], mvn_test = "mardia",#se utiliza SW ya que el numero de variables numericas es menor a 10  y hay 117 observaciones
                    univariate_test = "SW", show_new_data = TRUE)
mvn_analisis$multivariate_normality
mvn_analisis$univariate_normality

# --- Opción desde la definición
X<-as.matrix(datos[,1:4])
Xbarra<-colMeans(X)
S<-cov(X)
dm<-mahalanobis(X,Xbarra,S)
# ----- Posibles outliers multivariados ----- #

cutoff <- qchisq(0.98, df = ncol(datos[,2:4])) # Usando un nivel de confianza del 98%
outliers <- which(dm > cutoff);outliers

cuantiles<-qchisq(ppoints(length(dm)),df=4)
qq <-qqplot(cuantiles,dm,
            col = ifelse(sort(dm) > cutoff, "red", "black"), #colorea outlier
            pch = 20)
abline(0,1,col = "blue")

#graficos normlidad univariada

par(mfrow=c(2,2))
qqnorm(datos$Var1, main=expression(x[1]), pch = 20)
qqline(datos$Var1, col = "blue")
qqnorm(datos$Var2, main=expression(x[2]), pch = 20)
qqline(datos$Var2, col = "blue")
qqnorm(datos$Var3, main=expression(x[3]), pch = 20)
qqline(datos$Var3, col = "blue")
qqnorm(datos$Var4, main=expression(x[4]), pch = 20)
qqline(datos$Var4, col = "blue")



#Lo primero que se hace para saber si siguen la normlaidad de cada una de las variables utilizar MVN con una prueba SW ya que el numero de variables numericas es menor 10 y hay 117 observaciones
#Con esto nos muestra que las primeras dos variables son normales y que las otras 2 no son. Pero para entender esto un poco mas. Generamos los posibles outliers 
#multivariados los cuales nos dan los registris 31 47 52 55 82. Tambien se representan en su grafica la cual nosmuestra como estas cinco obseravaciones estan desviandose del abline  del qqplot
#Pero para saber exactamente lso outlieres de aquellas varianles que nos son normales y entenderlas mejor se hace un qqplot univariado.
#Con los graficos observamos la normalidad de la variable 1 y 2 mejor pero entendemos como la variable 3 no es normal ya que se genera una forma de "U" inversa por lo que nos deja ver la no normalidad


#Graficos Bivariados 
pairs(datos[, c(1:4)], pch=20, main="Matrix de bivariados")
library(corrplot)
corr<- cor(datos[,c(1:4)])
corrplot(corr, method ="number", rl.col="black")

#Caso donde si hay Multivariado Normal o Tamaño suficiente grande Teoria Asintotica, 
require(psych)
cortest.bartlett(cor(X), n=dim(X)[1])
p_values <- matrix(0, ncol = ncol(X), nrow = ncol(X),
                   dimnames = list(colnames(X), colnames(X)))

for (i in 1:ncol(X)) {
  for (j in 1:ncol(X)) {
    if (i < j) {
      p_values[i, j] <- cor.test(X[,i], X[,j], method = "spearman")$p.value
    }
  }
};p_values


#Lo que nos muestra el grafico de correlaciones es que entre la variables 1, 2 y 3 existen  correlaciones ya sean positivas 
# o negativas, como la variable 1 con la 2 de -0.75, la 1 con la 3 de 0.8 y la 2 con la 3 de -0.7. 
#La variable 4 no tiene fuertes relaciones con nungna de las otras tres variables
#PAra igaulmente verificar este hallazgo se genera una prueba de barlett ya que tenemos un tamaño suficiente de obseravciones para poder utilizar la teoria asitotica
#Con esto confirmamos que si hay correlaciones pero para entenderlas hay mejor encontramos los p values de cada una de las relaciones
#mostrando como las primeras 3 varibles se encuentras en con mucha relacion


#3. (1.0) Seg´un le corresponda dentro del script, responda solo UNA de las siguientes preguntas:
sample(pregunta,1)     # NO ALTERAR!
#  a) ¿Hay evidencia de diferencia en las m´etricas entre hospitales p´ublicos y privados?
#  b) ¿Hay diferencias significativas en las m´etricas con respecto al nivel de tecnolog´ıa?

#Opcion A
#Tenemos dos poblaciones independientes las cuales son los hopitales privados y los publicos para esto primero toca saber si hay varianzas iguales o no 
# ----- Dos Muestras independientes (VARIANZA HOMOGENEOS)

if (datos$Var5==1){
    datos1<- datos$  
}

X11 <- c(5.7,8.9,6.2,5.8,6.8,6.2)
X11 <- c(5.7,8.9,6.2,5.8,6.8,6.2)
X12 <- c(2.1,1.9,1.98,1.92,2,2.01)
X21 <- c(4.4,7.5,5.4,4.6,5.9)
X22 <- c(1.8,1.75,1.78,1.89,1.9)

A <- data.frame(X11,X12); nA = dim(A)[1]
B <- data.frame(X21,X22); nB = dim(B)[1]

A
B

xbarrA<-colMeans(A);xbarrA
xbarrB<-colMeans(B);xbarrB

xbarrA
xbarrB

#PRUEBA DE VARIANZA
# Valores
p <- 2; q <- 2 #p = Num Var q = Num Grupos
n1 <- 12; n2 <- 10; #Tamaño muestra de cada Grupo
N <- n1+n2
v1 <- n1-1; v2 <- n2-1;  
v <- N-q
SA <- cov(A);SA
SB <- cov(B);SB
Sp<-(1/v)*(v1*SA + v2*SB)

# Estadistico de prueba
Lambda3 <- v*log(det(Sp))-(v1*log(det(SA)))-(v2*log(det(SB)))
b <- (1/v1 + 1/v2 - 1/v)
rho <- 1-(2*p^2 + 3*p - 1)/(6*(p+1)*(q-1))*b
varphi <- rho*Lambda3; varphi
# Valor Critico
vc<-qchisq(0.05,(1/2)*p*(p+1)*(q-1),lower.tail = F);vc 
# --- Fin de prueba de Varianza
#SE COMPROBO QUE LAS VARIANZAS SON IGUALES

Sp <- (nA-1)*SA/(nA+nB-2) + (nB-1)*SB/(nA+nB-2);Sp

#si el prueba es mayor a critico entonces la varianza si son diferentes








