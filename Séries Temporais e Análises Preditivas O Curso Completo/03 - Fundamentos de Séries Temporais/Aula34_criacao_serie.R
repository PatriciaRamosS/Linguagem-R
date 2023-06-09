#############################################################################
###    CRIA��O DE UMA S�RIE TEMPORAL ALEAT�RIA COM DISTRIBUI��O NORMAL    ###
#############################################################################

### S�rie anual

set.seed(10)
dados1 <- rnorm(41)
print(dados1)
serie1 <- ts(dados1, start = c(1980), end=c(2020), frequency=1)
print(serie1)
plot(serie1)

# NORMALIDADE

# Gr�fico
qqnorm(serie1)
qqline(serie1)

# Ho = distribui��o normal : p > 0.05
# Ha = distribui��o != normal : p <= 0.05
shapiro.test(serie1)



# S�rie mensal
set.seed(6)
dados2 <- rnorm(72)
print(dados2)
serie2 <- ts(dados2,start = c(2015,1), end=c(2020,12), frequency=12)
print(serie2)
plot(serie2)

# NORMALIDADE

#Gr�fico
qqnorm(serie2)
qqline(serie2)

#Ho = distribui��o normal : p > 0.05
#Ha = distribui��o != normal : p <= 0.05
shapiro.test(serie2)



# S�rie di�ria
set.seed(12)
dados3 <- rnorm(730)
serie3 <- ts(dados3, start = c(2019,1,1), frequency=365.25)
print(serie3)
plot(serie3)


# NORMALIDADE

#Gr�fico
qqnorm(dados3)
qqline(dados3)

#Ho = distribui��o normal : p > 0.05
#Ha = distribui��o != normal : p <= 0.05
shapiro.test(dados3)



# S�rie trimestral
set.seed(20)
dados4 <- rnorm(164)
serie4 <- ts(dados4, start = 1980, end=2020, frequency=4)
print(serie4)
plot(serie4)


# NORMALIDADE

#Gr�fico
qqnorm(serie4)
qqline(serie4)

#Ho = distribui��o normal : p > 0.05
#Ha = distribui��o != normal : p <= 0.05
shapiro.test(serie4)


