################################
###    PROJETO M?DIA M?VEL   ###
################################

# BAIXAR PACOTE
install.packages("dplyr") 

# CARREGAR PACOTES
library(dplyr)

# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Cursos_Udemy/series_temporais_R/dados-covid-sp-master/data")

# ABRIR ARQUIVO
covid_sp <- read.csv('dados_covid_sp.csv', sep = ";")
View(covid_sp)

covid_sp <- read.csv2('dados_covid_sp.csv', sep = ";", encoding="UTF-8")
View(covid_sp)


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(covid_sp, function(x) sum(is.na(x)))
sapply(covid_sp, function(x) sum(is.nan(x)))


#Substituir valores missing
install.packages("tidyr")
library(tidyr)

### Função mutate_all temporariamente com erro:
### covid_sp2 <- covid_sp %>% mutate_all(replace_na, 0)


### Opção:
covid_sp2 <- replace(x = covid_sp,list = is.na(covid_sp), values=0)
sapply(covid_sp2, function(x) sum(is.na(x)))



#VERIFICA??O DA TIPAGEM DOS ATRIBUTOS (Variáveis)
# EXISTEM 7 TIPOS BÁSICOS:
# character (caracteres)
# integer (números inteiros)
# numeric (n?meros reais)
# logical (falso ou verdadeiro)
# complex (n?meros complexos)
# factor (fator: Sequ?ncia de valores definidos por n?veis)
# date (data)
str(covid_sp2)
# OU
glimpse(covid_sp2)

# Transforma??o da tipagem de atributos
covid_sp2$datahora <- as.Date(covid_sp2$datahora, format ='%Y-%m-%d')
glimpse(covid_sp2)


# An?lise da cidade de S?o Paulo
media_covid <- covid_sp2 %>% filter(nome_munic=="S?o Paulo")
View(media_covid)





##### M?dia M?vel casos novos #####

plot(media_covid$casos_mm7d, col="red", type = "l")

serie1 = ts(media_covid$casos_novos, start = c(2020,2,25), frequency=365.25)
print(serie1)
plot(serie1)

library("forecast")
media1 <- ma(serie1, order = 7, centre = TRUE)
plot(media1)

par(mfrow=c(2,1))
plot(media_covid$casos_mm7d, col="red", type = "l")
plot(media1)


par(mfrow=c(1,1))
plot(serie1, xlab = "Tempo (meses)")
lines(media1, col="red")







##### M?dia M?vel ?bitos novos #####

plot(media_covid$obitos_mm7d, col="red", type = "l")

serie2 = ts(media_covid$obitos_novos, start = c(2020,2,25), frequency=365.25)
print(serie2)
plot(serie2)

media2 <- ma(serie2, order = 7, centre = TRUE)
plot(media2)

par(mfrow=c(2,1))
plot(media_covid$obitos_mm7d, col="red", type = "l")
plot(media2)


par(mfrow=c(1,1))
plot(serie2)
lines(media2, col="red")




