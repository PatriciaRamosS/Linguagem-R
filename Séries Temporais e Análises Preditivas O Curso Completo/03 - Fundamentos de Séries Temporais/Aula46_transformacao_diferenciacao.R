############################################
###    Transformação e Diferenciação     ###
############################################


# S?rie mensal
set.seed(6)
dados <- rnorm(72, 10, 1)
serie1 <- ts(dados,start = c(2015,1), end=c(2020,12), frequency=12)
print(serie1)
plot(serie1)
summary(serie1)

# NORMALIDADE

# Gr?fico
qqnorm(serie1)
qqline(serie1)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie1)
hist(serie1)


# Transforma??o por log (Diminuir vari?ncia e melhorar normalidade)
serie2 <- log(serie1)
print(serie2)
summary(serie2)


# NORMALIDADE
hist(serie2)

# Gr?fico
qqnorm(serie2)
qqline(serie2)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(serie2)


# Transformação por raiz cúbica (quando possui dados com valor zero ou negativos)
serie3 <- sign(serie1)*abs(serie1)^(1/3)
print(serie3)


# NORMALIDADE
hist(serie3)


# Gráfico
qqnorm(serie3)
qqline(serie3)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie3)


# Comparação com o original
plot(serie3)
lines(suavizacao, col="red")
# Estacionaridade

library(forecast)
decomposicao <- decompose(serie2)
autoplot(decomposicao)
?autoplot
plot(decompose(serie2), col="blue")

# Teste pp (Philips-Perron)

# Ho = ? estacion?ria: p > 0.05
# Ha = n?o ? estacion?ria: p <= 0.05
library("urca")
pp <- ur.pp(serie2)
summary(pp)

# Indicação de quantas diferenciações devem ser feitas
ndiffs(serie2)





# Número de passageiros aéreos entre 1949 a 1960

passageiros <- AirPassengers
plot(passageiros)
print (passageiros)

# NORMALIDADE

# Gráfico
qqnorm(passageiros)
qqline(passageiros)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(passageiros)
hist(passageiros)


# Transforma??o (Diminuir vari?ncia e melhorar normalidade)
passageiros2 <- log(passageiros)
hist(passageiros2)
shapiro.test(passageiros2)


passageiros3 <- sign(passageiros)*abs(passageiros)^(1/3)
hist(passageiros3)
shapiro.test(passageiros3)


qqnorm(passageiros2)
qqline(passageiros2)

qqnorm(passageiros3)
qqline(passageiros3)



# Estacionaridade

library(forecast)
decomposicao <- decompose(passageiros3)
autoplot(decomposicao)

# Indica??o de quantas diferencia??es devem ser feitas
ndiffs(passageiros3)

# Teste pp (Philips-Perron)

# Ho = ? estacion?rio: p > 0.05
# Ha = n?o ? estacion?rio: p <= 0.05
pp <- ur.pp(passageiros3)
summary(pp)

# Primeira diferencia??o
dif_passageiros <- diff(passageiros3)
dif_passageiros


# Teste pp (Philips-Perron)

# Ho = ? estacion?ria: p > 0.05
# Ha = n?o ? estacion?ria: p <= 0.05
pp <- ur.pp(dif_passageiros)
summary(pp)


# Segunda diferencia??o
dif_passageiros2 <- diff(dif_passageiros)

pp <- ur.pp(dif_passageiros2)
summary(pp)

