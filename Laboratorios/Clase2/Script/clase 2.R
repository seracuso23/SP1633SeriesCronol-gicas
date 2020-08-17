# II. Análisis exploratorio de series de tiempo. Introducción a R.


# 0. librerías ---------------------------------------------------------------

library(ggfortify)
library(forecast)
library(fpp2)
library(data.table)
library(TTR)
library(xts)


# 1. Leer datos ----------------------------------------------------------

data("AirPassengers")
AirPassengers
class(AirPassengers)
AP <- as.numeric(AirPassengers)
class(AP)

str(AirPassengers)
str(AP)

AP.ts <- ts(AP, start = c(1949, 1), frequency = 12)
str(pass.ts)
frequency(AP.ts) #la frecuencia de la serie
cycle(AP.ts) #verificar el ciclo de cada observación

ts.plot(AP.ts)

plot(AP)
plot(AP.ts)

autoplot(AP.ts) + labs(x ="t", y = "pasajeros (miles)", title=" Pasajeros (1949-1961)") 


decomposeAP <- decompose(AP.ts,"multiplicative")
autoplot(decomposeAP)


#¿qué notamos en este gráfico? tendencia, ciclos, estacionalidad.

# 2. Efecto estacional ----------------------------------------------------

boxplot(AP.ts~cycle(AP.ts),xlab="mes", ylab = "pasajeros (miles)")

ggseasonplot(AP.ts, year.labels=FALSE, continuous=TRUE)

ggseasonplot(AP.ts, year.labels=FALSE, continuous=TRUE, polar = TRUE)

#¿qué notamos en estos gráficos?


# 3. Producción de cemento (cuatrimetre) ------------------------------------------------

?fpp2::qcement
cemento<-fpp2::qcement
str(cemento)
head(cemento)

autoplot(cemento)

ggseasonplot(cemento, year.labels=FALSE, continuous=TRUE)

ggsubseriesplot(cemento)

#¿qué notamos en estos gráficos?


# 4. venta de medicamento anti-diabético (mensual) ------------------------

?fpp2::a10
medicamento<-fpp2::a10

autoplot(medicamento)

ggseasonplot(medicamento, year.labels=FALSE, continuous=TRUE)

ggseasonplot(medicamento, year.labels=FALSE, continuous=TRUE, polar = TRUE)

ggsubseriesplot(medicamento)



# 5. Producción de cerveza en Australia ---------------------------------------------------------

?fpp2::ausbeer
cerveza<-fpp2::ausbeer

autoplot(cerveza)

ggseasonplot(cerveza, year.labels=FALSE, continuous=TRUE)

ggsubseriesplot(cerveza)

#funcion de autocorrelacion
acf(cerveza)
ggAcf(cerveza)
acf(ausbeer, plot = FALSE)

#lagplot
?gglagplot  #
gglagplot(cerveza,lags=16)
gglagplot(cerveza,lags=16,do.lines=FALSE)

h=1

gglagplot(cerveza,lags=h,do.lines=FALSE)

cerveza.shift<-shift(cerveza,n=h,type="lag")
cycle(cerveza)

plot(cerveza~cerveza.shift,xlim=c(200,600),ylim=c(200,600),
     xy.labels=FALSE,col=cycle(cerveza),pch=20)



# 6. Muertes por accidente en EU 1973-1978 --------------------------------

?USAccDeaths
autoplot(USAccDeaths)
ggseasonplot(USAccDeaths, year.labels=FALSE, continuous=TRUE)
gglagplot(USAccDeaths,lags=16)


# 7. Series multivariadas -----------------------------------------------------------

?fpp2::arrivals
arrivals<-fpp2::arrivals
str(arrivals)

arrivals

autoplot(arrivals)
autoplot(arrivals, facets = TRUE)

autoplot(arrivals, facets = TRUE) +
  geom_smooth() +
  labs("Llegadas internacionales a Australia",
       y = "llegadas (miles)",
       x = NULL)


# 8. Promedio diario industrial Dow Jone (20 de abril, 2006 a 20 d --------


djia = getYahooData("^DJI", start=20060420, end=20160420, freq="daily")
plot(djia$Close)

#plot(djia$Close[1:200])
#plot(djia$Close[1:100])

# 8. Ruido blanco y las medias móviles -------------------------------------------------------------

w = rnorm(500,0,1) # 500 N(0,1) variates
v = filter(w, sides=2, filter=rep(1/3,3)) # moving average
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, ylim=c(-3,3), main="moving average")

#las funciones de autocorrelación
acf(w)
acf(na.omit(v))

# 9. Señal+ruido ----------------------------------------------------------

cs = 2*cos(2*pi*1:500/50 + .6*pi); w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))

         
         