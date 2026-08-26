# ---------------- Universidad Javeriana
# ---------------- Facultad de Ciencias 
# -------------- Deparamento de Matemáticas
# -------------- Tomas Charria

# --------- Lectura de datos
data("mtcars")

# --------- Identificacion Pregunta a responder
numero <- 3       # Escriba el numero asignado
set.seed(31415)  # NO ALTERAR
lista <- c(rep(1,8),rep(2,7))
pregunta <- sample(lista)[numero]
pregunta # ESTA ES SU PREGUNTA

# ------------ DESARROLLO ------------#
#Pregunta 1

#a) (1.0) Verifique si el ACP es una herramienta ´util en este caso.
library(FactoMineR)
library(factoextra)
library(tibble)
head(mtcars)
X <- mtcars
diag(cov(mtcars))
colnames(mtcars) <- c("mpg", "cyl", "disp",
                      "hp", "drat", "wt", "qsec",
                      "vs","am","gear","carb")
#Lo primero que se hace es ver la diagonal de las covarianzas esto para poder 
#saber si se utiliza la matriz de covarianzas o la de correlaciones
#Como podemos ver en la diagonal hay uno valor muy pequeño ne disp por lo que 
#podria afectar las otras variables por lo que se va a utilizar la matriz de 
#Correlaciones
require(MVN)
mvn <- mvn(X, mvn_test="mardia", univariate_test="SW")
mvn$multivariate_normality
mvn$univariate_normality
#Según la prueba de Mardia, no hay normaldiad multivariada en el dataset 
#por su asimetría. Además, la prueba de Shapiro Wilk identificó que  laa 
#variable mpg, drat, wt, qsec tiene distribución normal
library(corrplot)
library(psych)
summary(X)
cov(X)
cor <- cor(X)
corrplot(cor,method ="circle",sig.level = 0.05,type="upper")
cortest.bartlett(cor, n=nrow(X))

#Para ya responder la pregunta podemos empezar con el plot el cual nos deja ver
#que las correlaciones son altas, y que hay muchas entonces toca comprender sus 
#relaciones mejor .Ademas tenemos 11 variables, por lo cual conviene reducir 
#dimensionalidad para poder describir los datos. Esto junto que con prueba de 
#esfericidad de Bartlett, se puede ver claramente que hay suficiente evidencia 
#para rechazar la hipotesis nula que indica que la matriz de correlación del dataset 
#es equivalente a la matriz de identidad . Con estas dos conclusiones nos damos 
#cuenta que si vale la pena hacer el ACP para poder comprender mejor las 
#relaciones entre variables ypoder rescribirlas mejor. 

#b) (0.5) ¿Cuantas componentes recomienda analizar? (justifique)
#Para definir el número de componentes que se analizar, se utilizara el Criterio
#de Varianza Acumulada donde consiste en seleccionar los componentes donde sus 
#varianzas acumulativas son Sufcientes (>70)
pca1 <- PCA(X,graph=F)
summary(pca1)

#Como podemos ver con el primer y segundo componente ya son suficiente para 
#analizar ya que su varianza acumulada es mayor que 70 siendo esta de 84.172
#Por lo que lo componentes 1 y 2 se recomiendan seleccionar para el analisis

#c) (1.0) ¿Qu´e variables contribuyen m´as a cada componente elegida para el 
#an´alisis? ¿Es posible nombrar las componentes para la interpretaci´on?

#Para buscar cuales variables son las que mayor contribuyen a cada componennte 
#elegido, se mirara el valor "contrib" del objeto PCA para cada variable.

pca1$var$contrib
fviz_contrib(pca1,choice="var",axes=1)
fviz_contrib(pca1,choice = "var",axes=2)

#Como podemos ver en el componente 1 no existe una variable la cual se pueda 
#responsabilizar de todo el comportamiento ya que existen 3 variables con 
#al rededor de 13% las cuales son mpg con 13.1, cyl con 13.9 y disp con 13.5
#igualmente muy seguido con un 11.9% esta wt y hp con 10.8 % por lo que para 
#representar el primer componente toca tener una agrupacion de estas, almismo 
#tiempo por se tantas variables se dificulta poner el nombre de esta componente.
#Para el segundo componente encontramos 2 variables con la mayor contribucion
#qsec con 21.4 % y gear con 21.3% seguuidas por menor contribucion pero igual 
#alta am con 18.4% y carb con 17.1% por lo que para representar el segundo 
#componente con estas cuatro variables ya se estaria expresando en 
#aproxiamdamente en un 78 % de la componente 2.

#d) (1.0) ¿Qu´e puede decir de la relaci´on entre las variables originales? 
#Comente sobre el comportamiento de al menos 2 individuos que le llamen la 
#atencion.
colnames(mtcars) <- c("mpg", "cyl", "disp",
                     "hp", "drat", "wt", "qsec",
                     "vs","am","gear","carb")
fviz_pca_biplot(pca1,repel=T)

#Con el biplot podemos ver que es muy dificil establecer unas pocas variables a 
#cada dimension. Pero podemos ver correlaciones directas como cyl, disp y wt ya 
#que su angulo es pequeño y apuntan hacia la misma direccion, de igual manera
#gear y am y podria pensarse qye draft por las mismas razones.pero existen 
#Pero respecto a estos dos grupos podemos definirlos como inversos ya que 
#Tienen un gran angulo y miran en direcciones casi opuestas por lo que si lo 
#podemos definir. De igual manera hay relaciones inversas entre carb y qsec y 
#hp con vs.Hablando de los individuos primero tenemos que hablar de las 
#variables de las caracteristicas de motor ya que estas se relacionan siendo 
#disp, cyl , hp ya que podemos ver como un gran conujunto de carros es del mismo
#tipo, tambien podemos compararlas al desempeño del vehiculo con las variables 
#qsec y mpg viendo como mpg es la mas inversa y qsec un poco menos relacionada 
#y esto lo podemos llevar a nuestro caso y concluir que mientras subamos
#el cilindraje y la potencia de caballos de fuerza vamos a tener un menor tiempo
#en recorrer una milla. Ya con esto podemos ver casos como el toyota corolla el 
#cual podria pensarse que es uno de los mayores carros de millas por galon ofrece
#muy informativo para la gente que este interesada en buscar carros con buen 
#Ahorro o irse al otro extremo con un carro como el Camaro z28 el cual 
#tiene gran hp grandes cantidades de cilindros por lo que va a ser un carro 
#bastante rapido. En conclusion teniendo una grafica asi uno puede comprender 
#los defectos y ventajas de cada carro y puede hacer una eleccion acorde a eso.









     
     