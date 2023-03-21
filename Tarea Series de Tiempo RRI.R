rm(list=ls())

# Cargar la base de datos de Google Mobility Report para México
datos <- read_csv("ITESO - Data Science/Semestre 3 - Series de Tiempo/Tarea Mobility Report/2022_MX_Region_Mobility_Report_2.csv")

# Seleccionar la entidad "Jalisco" y eliminar filas con valores faltantes
jalisco <- subset(datos, sub_region_1 == "Jalisco")

# Convertir la columna "date" a formato de fecha de R
jalisco$date <- as.Date(jalisco$date)

# Graficar la serie de movilidad para "Jalisco"
plot(jalisco$date, jalisco$grocery_and_pharmacy_percent_change_from_baseline, 
     type = "l", xlab = "Fecha", ylab = "Cambio porcentual en movilidad",
     main = "Movilidad en tiendas de alimentos y farmacias en Jalisco")

# Función de autocorrelación
acf(jalisco$grocery_and_pharmacy_percent_change_from_baseline, lag.max = 30, 
    main = "Función de autocorrelación")

# Función de autocorrelación parcial
pacf(jalisco$grocery_and_pharmacy_percent_change_from_baseline, lag.max = 30, 
     main = "Función de autocorrelación parcial")

#SIMULACIONES


set.seed(123)

# Simulación para n = 100
set.seed(113)
n <- 100
ar_parametros <- c(-0.2, 0.4, -0.1)
ma_parametros <- 0
arima_sim_100 <- arima.sim(model = list(ar = ar_parametros, ma = ma_parametros), n = n)


#Graficando la serie
plot(arima_sim_100, type = "l", main = "Serie AR(3) para n = 100")

#Función de autocorrelación
acf(arima_sim_100, main = "Función de autocorrelación para n = 100")

#Función de autocorrelación parcial
pacf(arima_sim_100, main = "Función de autocorrelación parcial para n = 100")

#Confirmar con la función auto.arima
library(forecast)
auto.arima(arima_sim_100)

#Regresión
arima_100 <- arima(arima_sim_100, order = c(3, 0, 0))
arima_100
arima_100$coef


#SIMULACION PARA 500
set.seed(113)
n <- 500
ar_parametros <- c(-0.2, 0.4, -0.1)
ma_parametos <- 0
arima_sim_500 <- arima.sim(model = list(ar = ar_parametros, ma = ma_parametos), n = n)

#Graficando la serie
plot(arima_sim_500, type = "l", main = "Serie AR(3) para n = 500")

#Función de autocorrelación
acf(arima_sim_500, main = "Función de autocorrelación para n = 500")

#Función de autocorrelación parcial
pacf(arima_sim_500, main = "Función de autocorrelación parcial para n = 500")

#Confirmar con la función auto.arima
library(forecast)
auto.arima(arima_sim_500)

#Regresión
arima_500 <- arima(arima_sim_500, order = c(3, 0, 0))
arima_500
arima_500$coef

#SIMULACION PARA 1000
set.seed(113)
n <- 1000
ar_parametros <- c(-0.2, 0.4, -0.1)
ma_parametos <- 0
arima_sim_1000 <- arima.sim(model = list(ar = ar_parametros, ma = ma_parametos), n = n)

#Graficando la serie
plot(arima_sim_1000, type = "l", main = "Serie AR(3) para n = 1000")

#Función de autocorrelación
acf(arima_sim_1000, main = "Función de autocorrelación para n = 1000")

#Función de autocorrelación parcial
pacf(arima_sim_1000, main = "Función de autocorrelación parcial para n = 1000")

#Confirmar con la función auto.arima
library(forecast)
auto.arima(arima_sim_1000)

#Regresión
arima_1000 <- arima(arima_sim_1000, order = c(3, 0, 0))
arima_1000
arima_1000$coef
