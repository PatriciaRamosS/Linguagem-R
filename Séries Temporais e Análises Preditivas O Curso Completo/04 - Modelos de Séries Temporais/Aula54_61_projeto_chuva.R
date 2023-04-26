###########################
###    PROJETO CHUVA    ###
###########################

# CARREGAR PACOTES
library(dplyr)

# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Cursos_Udemy/series_temporais_R")

# ABRIR ARQUIVO
chuva_sp <- read.csv('chuva_tratado.csv', sep = ";")
View(chuva_sp)

str(chuva_sp)

chuva_sp2 <- as.vector(t(chuva_sp))
print(chuva_sp2)

##### Cria??o da s?rie #####

serie <- ts(chuva_sp2, start = c(1985,1), end = c(2020,12), frequency=12)
print(serie)
plot(serie)





##### M?dia M?vel #####
library("forecast")
media <- ma(serie, order = 6, centre = TRUE)
plot(media)
print(media)
plot(serie, xlab = "Tempo (meses)", col = "blue")
lines(media, col="red")



### DECOMPOSI??O
decomposicao <- decompose(serie)
plot(decomposicao, col = "brown")

plot(decomposicao$trend)
plot(decomposicao$seasonal)
plot(decomposicao$random)
plot(window(decomposicao$trend, start=2011, end=2013))

#efeito sazonal por ano
ggseasonplot(window(serie, start=c(2011), end=2016))



### SUAVIZA??O
serie2 <- tsclean(serie)

# Compara??o
plot(serie)
lines(serie2, col="red")


### NORMALIDADE
qqnorm(serie2)
qqline(serie2)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(serie2)
print(serie2)

### TRANSFORMAÇÃO
serie3 <- sign(serie2)*abs(serie2)^(1/3)
print(serie3)

hist(serie2)
hist(serie3)

qqnorm(serie3)
qqline(serie3)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie3)



#### ESTACIONARIDADE
library("urca")
# Teste pp (Philips-Perron)

# Ho = ? estacion?ria: p > 0.05
# Ha = n?o ? estacion?ria: p <= 0.05
estacionaridade <- ur.pp(serie3)
summary(estacionaridade)

ndiffs(serie3)

serie4 <- diff(serie3)
estacionaridade = ur.pp(serie4)
summary(estacionaridade)

serie5 <- diff(serie4)
estacionaridade <- ur.pp(serie5)
summary(estacionaridade)




plot(serie3)

# Teste KPSS (Kwiatkowski-Phillips-Schmidt-Shin)

# Ho = n?o ? estacion?rio: teste estat?stico > valor cr?tico
# Ha = ? estacion?rio:  teste estat?stico < valor cr?tico
kpss <- ur.kpss(serie3)
summary(kpss)

# Teste df (Dickey Fuller)

# Ho = n?o ? estacion?rio: teste estat?stico > valor cr?tico
# Ha = ? estacion?rio:  teste estat?stico < valor cr?tico
df <- ur.df(serie3)
summary(df)


#### AUTOCORRELA??O
acf(serie3)
pacf(serie3)
tsdisplay(serie3)

# Teste de Autocorrela??o (Ljung-Box)
# Ho = n?o ? autocorrelacionado: p > 0.05
# Ha = ? autocorrelacionado: p <= 0.05
Box.test(serie3, type = "Ljung-Box")





?arima




#### MODELO AR

# modelo arima:(p,d,q)
# modelo ar: (p,0,0)
acf(serie3)
modelo_ar <- arima(serie3, order = c(10,0,0))
summary(modelo_ar)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_ar)

plot(resid(modelo_ar))

qqnorm(resid(modelo_ar))
qqline(resid(modelo_ar))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_ar))

acf(resid(modelo_ar))
pacf(resid(modelo_ar))

plot(serie3)
lines(serie3-modelo_ar$resid, col= "red")

previsao <- forecast(modelo_ar,h=36)
plot(previsao)
lines(serie3-modelo_ar$resid, col= "red")

print(previsao)

prev_escala <- as.data.frame(previsao)^3
View(prev_escala)







#### MODELO MA
# modelo arima:(p,d,q)
# modelo ma: (0,0,q)
modelo_ma <- arima(serie3, order = c(0,0,8))
summary(modelo_ma)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_ma)

plot(resid(modelo_ma))

qqnorm(resid(modelo_ma))
qqline(resid(modelo_ma))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_ma))

acf(resid(modelo_ma))
pacf(resid(modelo_ma))

plot(serie3)
lines(serie3-modelo_ma$resid, col= "red")

previsao2 <- forecast(modelo_ma,h=24)
plot(previsao2)
lines(serie3-modelo_ma$resid, col= "red")

print(previsao2)

prev_escala2 <- as.data.frame(previsao2)^3
View(prev_escala2)








#### MODELO ARMA
# modelo arima:(p,d,q)
# modelo ma: (p,0,q)

modelo_arma <- arima(serie3, order = c(10,0,8))
summary(modelo_arma)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_arma)

plot(resid(modelo_arma))

qqnorm(resid(modelo_arma))
qqline(resid(modelo_arma))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_arma))

acf(resid(modelo_arma))
pacf(resid(modelo_arma))

plot(serie3)
lines(serie3-modelo_arma$resid, col= "red")

previsao3 <- forecast(modelo_arma,h=24)
plot(previsao3)
lines(serie3-modelo_arma$resid, col= "red")

print(previsao3)

prev_escala3 <- as.data.frame(previsao3)^3
View(prev_escala3)







#### MODELO ARIMA

modelo_arima <- arima(serie3, order = c(4,1,4))
summary(modelo_arima)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_arima)

plot(resid(modelo_arima))

qqnorm(resid(modelo_arima))
qqline(resid(modelo_arima))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_arima))

acf(resid(modelo_arima))
pacf(resid(modelo_arima))

plot(serie3)
lines(serie3-modelo_arima$resid, col= "red")

previsao4 <- forecast(modelo_arima,h=24)
plot(previsao4)
lines(serie3-modelo_arima$resid, col= "red")

print(previsao4)

prev_escala4 <- as.data.frame(previsao4)^3
View(prev_escala4)







#### MODELO SARIMA: (p,d,q)(P,D,Q)

modelo_sarima <- arima(serie3, order = c(4,0,4), seasonal = c(1,0,1))
summary(modelo_sarima)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_sarima)

plot(resid(modelo_sarima))

qqnorm(resid(modelo_sarima))
qqline(resid(modelo_sarima))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_sarima))

acf(resid(modelo_sarima))
pacf(resid(modelo_sarima))

plot(serie3)
lines(serie3-modelo_sarima$resid, col= "red")

previsao5 <- forecast(modelo_sarima,h=24)
plot(previsao5)
lines(serie3-modelo_sarima$resid, col= "red")

print(previsao5)

prev_escala5 <- as.data.frame(previsao5)^3
View(prev_escala5)







### AUTOARIMA
?auto.arima
modelo_auto <- auto.arima(serie3, trace = T, stepwise = F, approximation = F,
                          max.p = 5, max.q = 5, max.P = 2, max.Q = 2)

# Trace: apresenta no console a lista dos modelos.
# stepwise: sele??o gradual(processo mais r?pido, por?m menos minucioso)
# approximation: sele??o do melhor modelo por aproxima??o
#           (indicado para s?ries muito longas, diminui tempo computacional)
# Drift do modelo ? um par?metro que representa a tend?ncia temporal num passeio aleat?rio.
# Interessante dobrar as ordens m?ximas: max.p = 10, max.q = 10, max.P = 4, max.Q = 4
summary(modelo_auto)


# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_auto)

plot(resid(modelo_auto))

qqnorm(resid(modelo_auto))
qqline(resid(modelo_auto))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_auto))

acf(resid(modelo_auto))
pacf(resid(modelo_auto))

plot(serie3)
lines(serie3-modelo_auto$resid, col= "red")

previsao6 <- forecast(modelo_auto,h=24)
plot(previsao6)
lines(serie3-modelo_auto$resid, col= "red")

prev_escala6 <- as.data.frame(previsao6)^3
View(prev_escala6)


