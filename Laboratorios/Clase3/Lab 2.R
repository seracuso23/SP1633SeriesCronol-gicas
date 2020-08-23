
# Laboratorio 2 -----------------------------------------------------------


library(ggplot2)
library(forecast)
library(fpp2)

# Descomposición de series ------------------------------------------------


# 1. Descomposición aditiva -----------------------------------------------


contrayentes<-read.csv("contrayentes.csv",sep=";")
y<-ts(contrayentes$serie,start=c(1978,1),frequency=12)
aditivo<-decompose(y,type="additive")
names(aditivo)
aditivo$x #serie original
aditivo$seasonal #índices estacionales normalizados 
aditivo$trend #tendencia
aditivo$random #residuo
aditivo$figure #índices estacionales normalizados

plot(aditivo)
autoplot(aditivo)
#range.bars=TRUE Logical indicating if each plot should have a bar at its right side representing
#relative size. If NULL, automatic selection takes place.
autoplot(aditivo, range.bars=FALSE)

tend <- aditivo$trend
tend.estac <- aditivo$trend+aditivo$seasonal
serie.aj.estac<-aditivo$x-aditivo$seasonal

autoplot(cbind(aditivo$x,tend,tend.estac,serie.aj.estac),col=1:4, size = 1.2)+
  xlab("tiempo")+ylab("contrayentes")+
  ggtitle("Serie contrayentes")+
  scale_color_manual(labels = c("serie original","tend", "tendencia+estac","serie.aj.estac"), 
                     values = c(1, 2, 3, 4))


# 2. Descomposición multiplicativa ----------------------------------------

turistas<-read.csv("turistas.csv",sep=";")
y<-ts(turistas$turistas,start=c(1991,1),frequency=12)
multiplicativo<-decompose(y,type="multiplicative")
names(multiplicativo)
multiplicativo$x #serie original
multiplicativo$seasonal #índices estacionales normalizados 
multiplicativo$trend #tendencia
multiplicativo$random #residuo
multiplicativo$figure #índices estacionales normalizados
serie.aj.estac<-y/multiplicativo$seasonal


plot(multiplicativo)
autoplot(multiplicativo)

tend <- multiplicativo$trend
tend.estac <- multiplicativo$trend * multiplicativo$seasonal
serie.aj.estac<-multiplicativo$x/multiplicativo$seasonal

autoplot(cbind(multiplicativo$x,tend,tend.estac,serie.aj.estac),col=1:4, size = 1.2)+
  xlab("tiempo")+ylab("contrayentes")+
  ggtitle("Serie contrayentes")+
  scale_color_manual(labels = c("serie original","tend", "tendencia+estac","serie.aj.estac"), 
                     values = c(1, 2, 3, 4))




# Descomposición STL ---------------------------------------------------------------------

y.stl1<-stl(y,s.window,t.window=20, s.window="periodic", robust=TRUE)
#robust: regresión robusta.
plot(y.stl1)

y.stl2<-stl(y,t.window=5, s.window="periodic", robust=TRUE)
#robust: regresión robusta.
plot(y.stl2)


# Pronóstico con STL ------------------------------------------------------

y
y.train<-window(y,start=c(1991,1),end=c(1999,12))
y.test<-window(y,start=c(2000,1),end=c(2000,12))
y.stl3<-stl(y.train,t.window=20, s.window="periodic", robust=TRUE)

?forecast.stl
pronostico<-forecast(y.stl3,h=12)
#default: suavizamiento exponencial

names(pronostico)
plot(pronostico)
points(y.test,type="l",col=2)

accuracy(pronostico)
accuracy(pronostico,y.test)

#tomar en cuenta la diferencia entre los residuos de la descomposición y el error de pronóstico.
e.train<-y.train-fitted(pronostico)
(MAE.train<-sum(abs(e.train))/length(e.train))
(RMSE<-sqrt(sum((e.train)^2)/length(e.train)))


n<-12
e<-pronostico$mean-y.test
(MAE<-sum(abs(e))/n)
sqrt(MSE<-sum(e^2)/n)
(MAPE<-sum(abs(100*e/y.test))/n)

