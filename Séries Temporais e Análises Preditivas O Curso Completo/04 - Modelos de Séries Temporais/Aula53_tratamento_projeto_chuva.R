#################################################
###    TRATAMENTO DOS DADOS: PROJETO CHUVA    ###
#################################################

#### INFORMAÇÕES DO DATASET

# http://www.hidrologia.daee.sp.gov.br/
# PREFIXO: 	E3-262	
# NOME DO POSTO: 	GUARAU	
# MUNICÍPIO: 	SAO PAULO	
# CURSO D'ÁGUA: 	CABUCU DE BAIXO,R/(ITAGUACU,COR)	
# LATITUDE: 	23°26'33"	
# LONGITUDE: 	46°39'01"	
# DADOS DE 1985 A 2021



# CARREGAR PACOTE
library(dplyr)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Cursos_Udemy/series_temporais_R")

# ABRIR ARQUIVO
chuva <- read.csv('chuva_mensal.csv', sep = ";", encoding = "UTF-8")
View(chuva)

str(chuva)

chuva$Janeiro <- as.numeric(chuva$Janeiro)
chuva$Fevereiro <- as.numeric(chuva$Fevereiro)
chuva$Março <- as.numeric(chuva$Março)
chuva$Maio <- as.numeric(chuva$Maio)
chuva$Junho <- as.numeric(chuva$Junho)
chuva$Julho <- as.numeric(chuva$Julho)
chuva$Agosto <- as.numeric(chuva$Agosto)
chuva$Setembro <- as.numeric(chuva$Setembro)
chuva$Outubro <- as.numeric(chuva$Outubro)
chuva$Novembro <- as.numeric(chuva$Novembro)
chuva$Dezembro <- as.numeric(chuva$Dezembro)


# Renomeando variáveis (colunas)
chuva <- rename(chuva, ano = X.U.FEFF.Ano)
View(chuva)


str(chuva)


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(chuva, function(x) sum(is.na(x)))
sapply(chuva, function(x) sum(is.nan(x)))


#Substituir valores missing

chuva$Janeiro[is.na(chuva$Janeiro)] <- mean(chuva$Janeiro[which(chuva$Janeiro!='NA')])
chuva$Fevereiro[is.na(chuva$Fevereiro)] <- mean(chuva$Fevereiro[which(chuva$Fevereiro!='NA')])
chuva$Março[is.na(chuva$Março)] <- mean(chuva$Março[which(chuva$Março!='NA')])
chuva$Maio[is.na(chuva$Maio)] <- mean(chuva$Maio[which(chuva$Maio!='NA')])
chuva$Junho[is.na(chuva$Junho)] <- mean(chuva$Junho[which(chuva$Junho!='NA')])
chuva$Julho[is.na(chuva$Julho)] <- mean(chuva$Julho[which(chuva$Julho!='NA')])
chuva$Agosto[is.na(chuva$Agosto)] <- mean(chuva$Agosto[which(chuva$Agosto!='NA')])
chuva$Setembro[is.na(chuva$Setembro)] <- mean(chuva$Setembro[which(chuva$Setembro!='NA')])
chuva$Outubro[is.na(chuva$Outubro)] <- mean(chuva$Outubro[which(chuva$Outubro!='NA')])
chuva$Novembro[is.na(chuva$Novembro)] <- mean(chuva$Novembro[which(chuva$Novembro!='NA')])
chuva$Dezembro[is.na(chuva$Dezembro)] <- mean(chuva$Dezembro[which(chuva$Dezembro!='NA')])

sapply(chuva, function(x) sum(is.na(x)))
str(chuva)

#Excluir Linhas
chuva2 <- chuva %>% filter(ano!=2021)
chuva2 <- chuva2 %>% filter(ano!="Media")
View(chuva2)

# EXCLUIR UMA COLUNA
chuva2 <- subset(chuva2, select = -c(ano))

# Exportação de arquivos
write.table(chuva2, file ="chuva_tratado.csv", sep = ";")


