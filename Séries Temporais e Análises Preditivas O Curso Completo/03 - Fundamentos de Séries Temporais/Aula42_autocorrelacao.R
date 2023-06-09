############################
###    Autocorrela��o    ###
############################


set.seed(8)
dados = rnorm(72)
serie = ts(dados,start = c(2015,1), end=c(2020,12), frequency=12)
print(serie)
plot(serie)
acf(serie)
pacf(serie)

# Teste de Autocorrela��o (Ljung-Box)

# Ho = n�o � autocorrelacionado: p > 0.05
# Ha = � autocorrelacionado: p <= 0.05
Box.test (serie, type = "Ljung")





# Manchas Solares
?sunspots
manchas_solares <- sunspots
manchas_solares
plot(manchas_solares)

# Estacionaridade

# Teste pp (Phillips-Perron)

# Ho = � estacion�rio: p > 0.05
# Ha = n�o � estacion�rio: p <= 0.05
library("urca")
pp <- ur.pp(manchas_solares)
summary(pp)


# Autocorrela��o

# Correlograma
acf(manchas_solares)
pacf(manchas_solares)

# Teste de Autocorrela��o (Ljung-Box)
# Ho = n�o � autocorrelacionado: p > 0.05
# Ha = � autocorrelacionado: p <= 0.05
Box.test(manchas_solares, type = "Ljung-Box")







