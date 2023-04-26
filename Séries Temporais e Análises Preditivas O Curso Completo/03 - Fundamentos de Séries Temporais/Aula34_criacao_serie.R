#############################################################################
###    CRIAÇÃO DE UMA SÉRIE TEMPORAL ALEATÓRIA COM DISTRIBUIÇÃO NORMAL    ###
#############################################################################

### Série anual

set.seed(10)
dados1 <- rnorm(41)
print(dados1)
serie1 <- ts(dados1, start = c(1980), end=c(2020), frequency=1)
print(serie1)
plot(serie1)

# NORMALIDADE

# Gráfico
qqnorm(serie1)
qqline(serie1)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(serie1)



# Série mensal
set.seed(6)
dados2 <- rnorm(72)
print(dados2)
serie2 <- ts(dados2,start = c(2015,1), end=c(2020,12), frequency=12)
print(serie2)
plot(serie2)

# NORMALIDADE

#Gráfico
qqnorm(serie2)
qqline(serie2)

#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05
shapiro.test(serie2)



# Série diária
set.seed(12)
dados3 <- rnorm(730)
serie3 <- ts(dados3, start = c(2019,1,1), frequency=365.25)
print(serie3)
plot(serie3)


# NORMALIDADE

#Gráfico
qqnorm(dados3)
qqline(dados3)

#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05
shapiro.test(dados3)



# Série trimestral
set.seed(20)
dados4 <- rnorm(164)
serie4 <- ts(dados4, start = 1980, end=2020, frequency=4)
print(serie4)
plot(serie4)


# NORMALIDADE

#Gráfico
qqnorm(serie4)
qqline(serie4)

#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05
shapiro.test(serie4)


