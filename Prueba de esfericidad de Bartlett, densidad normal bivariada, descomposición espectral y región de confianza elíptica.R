airquality
View(airquality)

#
summary(airquality$Ozone)
require(ggplot2)
hist(airquality$Ozone,xlab = "Nivel de ozono en partes por bilĺon",
     ylab = "Frecuencia", breaks = 20, main="Ozone", ylim = c(0,30),xlim = c(0,250))

summary(airquality$Wind)
hist(airquality$Wind,xlab = "Velocidad del viento en mph",
     ylab = "Frecuencia", breaks = 20, main="Wind", ylim = c(0,30),xlim = c(0,25))

summary(airquality$Temp)
hist(airquality$Temp,xlab = "Temperatura en grados Fahrenheit",
     ylab = "Frecuencia", breaks = 20, main="Temp", ylim = c(0,30),xlim = c(50,100))

#
R<-cor(X, use = "complete.obs")
R
require(corrplot)
corrplot(R,type="lower")

#

ggplot(airquality, aes(x = factor(Month), y = Temp)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Temperatura agrupado por Mes",
    x = "Mes",
    y = "Temperatura") 


#Punto 2.d
#UNIVARIADO
X2<-airquality[,1:6]
R<-cor(X2, use = "complete.obs")
R
require(corrplot)
corrplot(R,type="lower")

pairs(X2[, 1:4], pch = 0)

#MULTIVARIADO
#Prueba Esfericidad
X3 <- na.omit(X2)
require(psych)
cortest.bartlett(cor(X3), n=30)

#PUNTO 4.a
H <- data.frame(
  Hospital = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  x1 = c(120, 150, 95, 180, 110, 165, 130, 140, 100, 175),  
  x2 = c(450, 520, 380, 600, 430, 570, 490, 510, 400, 590), 
  x3 = c(320, 410, 280, 500, 350, 470, 390, 420, 310, 495)  
)
X123 <- H[,2:4]
pairs(X123, pch=21)
plot(H[,2], H[,3], xlab = "x1", ylab = "x2")

#PUNTO 4.b

Xbarra<-apply(H[,2:3], 2, mean)
Xbarra
S<-cov(H[,2:3])
S
R<-cor(H[,2:3])
R

#PUNTO 9.a
library(mvtnorm)
mu<-c(1,-1)
s<-matrix(c(3,1,1,2), nrow = 2, ncol = 2, byrow = TRUE)
a1 = a2 <- seq(-4, 6, length.out = 100)

densidad <- function(a1, a2) dmvnorm(cbind(a1,a2),mu, s)
f<-outer(a1,a2, FUN="densidad")

contour(a1,a2,f, drawlabels=TRUE, nlevels=10,
        xlab=expression(x[1]),
        ylab=expression(x[2]), main = "Contorno"
)

#PUNTO 9.b
autov<-eigen(s) 
autov$values
autov$vectors

alpha<-0.10
threshold<-qchisq(1-alpha, df=2)

contour(a1,a2,f,levels=dmvnorm(t(mu),mu,s)*exp(-threshold/2), drawlabels=TRUE,
        xlab=expression(x[1]),
        ylab=expression(x[2]), main = "Contorno"
        )
