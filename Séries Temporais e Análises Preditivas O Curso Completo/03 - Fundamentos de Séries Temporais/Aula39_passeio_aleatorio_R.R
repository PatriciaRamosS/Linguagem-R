#############################################
###    CRIA��O DE UM PASSEIO ALEAT�RIO    ###
#############################################


## Gera��o de 41 n�meros aleat�rios entre 0 e 100
dados1 <- runif(41, min=0, max=100);
s1 <- ts(dados1, start = 1980, end=2020, frequency=1)
plot(s1)



## Gera��o de uma amostra aleat�ria de 41 n�meros, no intervalo de 0 a 100
## e com reposicao
dados2 <- sample(0:100, 41, replace=TRUE)
serie2 = ts(dados2, start = c(1980), end=c(2020), frequency=1)
plot(serie2)



passeio <- numeric()
passeio[1] <- rnorm(1,0,1) # cria um valor aleat�rio com m�dia zero e desvio padr�o 1
for(t in 2:2000){
  passeio[t]=passeio[t-1]+rnorm(1,0,1)
}
plot.ts(passeio, main="Passeio Aleat�rio",col="red")

?rnorm

