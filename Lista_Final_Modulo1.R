# Instalando os pacotes necessários
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("palmerpenguins")

# Lendo os pacotes necessários
library(dplyr)
library(ggplot2)
library(palmerpenguins)

# Documentação da base de dados
?penguins

# Organizando a base de dados
dados = na.omit(penguins) # Removendo os NAs (valores faltantes)
dados$year = factor(dados$year) # Transformando a variável year em fator

### Questão 1

# Opção 1:
dim(dados) # número de linhas e colunas

# Opção 2:
nrow(dados) # número de linhas
ncol(dados) # número de linhas

### Questão 2

# Opção 1:
dados$body_mass_g
min(dados$body_mass_g)
mean(dados$body_mass_g)
max(dados$body_mass_g)

# Opção 2:
attach(dados)
min(body_mass_g)
mean(body_mass_g)
max(body_mass_g)

# Opção 3:
peso_em_gramas = dados$body_mass_g
min(peso_em_gramas)
mean(peso_em_gramas)
max(peso_em_gramas)

### Questão 3

## letra a

names(dados) # para ver os nomes das variáveis

# Opção 1:
dados1 = rename(dados,
                especies = species,
                ilha = island,
                comp_bico = bill_length_mm,
                prof_bico = bill_depth_mm,
                tam_nadadeira = flipper_length_mm,
                peso = body_mass_g,
                sexo = sex,
                ano = year)

# Opção 2:
dados1 = select(dados,
                especies = species,
                ilha = island,
                comp_bico = bill_length_mm,
                prof_bico = bill_depth_mm,
                tam_nadadeira = flipper_length_mm,
                peso = body_mass_g,
                sexo = sex,
                ano = year)

## letra b

dados2 = arrange(dados1, peso)

## letra c

# Opção 1:
dados3 = filter(dados2, especies != "Gentoo")

# Opção 2:
dados3 = filter(dados2, especies == "Adelie" | especies == "Chinstrap")

# letra d

dados_final = mutate(dados3, razao = comp_bico/prof_bico)

### Questão 4

str(dados_final)
# fatores: especies, ilha, sexo, ano
# numéricas: comp_bico, prof_bico, tam_nadadeira, pego_g, razao

## letra a

ggplot(dados_final, aes(x = especies)) +
  geom_bar(fill = "dodgerblue") +
  xlab("Espécies") +
  ylab("Número de Observações")

ggplot(dados_final, aes(x = ilha)) +
  geom_bar(fill = "dodgerblue") +
  xlab("Ilha") +
  ylab("Número de Observações")

ggplot(dados_final, aes(x = sexo)) +
  geom_bar(fill = "dodgerblue") +
  xlab("Sexo") +
  ylab("Número de Observações")

ggplot(dados_final, aes(x = ano)) +
  geom_bar(fill = "dodgerblue") +
  xlab("Ano") +
  ylab("Número de Observações")

## letra b

ggplot(dados_final, aes(x = comp_bico)) +
  geom_histogram(fill = "dodgerblue", binwidth = 5) +
  xlab("Comprimento do Bico") +
  ylab("Número de Observações")

ggplot(dados_final, aes(x = prof_bico)) +
  geom_histogram(fill = "dodgerblue", binwidth = 1) +
  xlab("Profundidade do Bico") +
  ylab("Número de Observações")

ggplot(dados_final, aes(x = tam_nadadeira)) +
  geom_histogram(fill = "dodgerblue", binwidth = 10) +
  xlab("Tamanho da Nadadeira") +
  ylab("Número de Observações")

ggplot(dados_final, aes(x = peso)) +
  geom_histogram(fill = "dodgerblue", binwidth = 250) +
  xlab("Peso (em gramas)") +
  ylab("Número de Observações")

ggplot(dados_final, aes(x = razao)) +
  geom_histogram(fill = "dodgerblue", binwidth = 0.2) +
  xlab("Comprimento/Profundidade") +
  ylab("Número de Observações")

### Questão 5

## letra a

ggplot(dados_final, aes(x = especies, y = peso)) +
  geom_boxplot(fill = "dodgerblue") +
  xlab("Espécies") +
  ylab("Peso (em gramas)")

ggplot(dados_final, aes(x = ilha, y = peso)) +
  geom_boxplot(fill = "dodgerblue") +
  xlab("Ilha") +
  ylab("Peso (em gramas)")

ggplot(dados_final, aes(x = sexo, y = peso)) +
  geom_boxplot(fill = "dodgerblue") +
  xlab("Sexo") +
  ylab("Peso (em gramas)")

ggplot(dados_final, aes(x = ano, y = peso)) +
  geom_boxplot(fill = "dodgerblue") +
  xlab("Ano") +
  ylab("Peso (em gramas)")

## letra b

ggplot(dados_final, aes(x = comp_bico, y = peso)) +
  geom_point(color = "dodgerblue") +
  xlab("Comprimento do Bico") +
  ylab("Peso (em gramas)")

ggplot(dados_final, aes(x = prof_bico, y = peso)) +
  geom_point(color = "dodgerblue") +
  xlab("Profundidade do Bico") +
  ylab("Peso (em gramas)")

ggplot(dados_final, aes(x = tam_nadadeira, y = peso)) +
  geom_point(color = "dodgerblue") +
  xlab("Tamanho da Nadadeira") +
  ylab("Peso (em gramas)")

ggplot(dados_final, aes(x = razao, y = peso)) +
  geom_point(color = "dodgerblue") +
  xlab("Comprimento/Profundidade") +
  ylab("Peso (em gramas)")

## letra c

# Sexo, Comprimento do Bico, Profundidade do Bico e Tamanho da Nadadeira.

### Questão 6

attach(dados_final)

## letra a

# vamos fazer todos os modelos com apenas uma variável:
ajuste_e = lm(peso ~ especies)
summary(ajuste_e)

ajuste_i = lm(peso ~ ilha)
summary(ajuste_i)

ajuste_s = lm(peso ~ sexo)
summary(ajuste_s) # variável importante!

ajuste_a = lm(peso ~ ano)
summary(ajuste_a)

ajuste_c = lm(peso ~ comp_bico)
summary(ajuste_c) # variável importante!

ajuste_p = lm(peso ~ prof_bico)
summary(ajuste_p) # variável importante!

ajuste_t = lm(peso ~ tam_nadadeira)
summary(ajuste_t) # variável importante!

ajuste_r = lm(peso ~ razao)
summary(ajuste_r)

# vamos fazer uma regressão múltipla:

ajuste_scpt = lm(peso ~ sexo + comp_bico + prof_bico + tam_nadadeira)
summary(ajuste_scpt)

ajuste_spt = lm(peso ~ sexo + prof_bico + tam_nadadeira)
summary(ajuste_spt)

## letra b

AIC(ajuste_e,
    ajuste_i,
    ajuste_s,
    ajuste_a,
    ajuste_c,
    ajuste_p,
    ajuste_t,
    ajuste_r,
    ajuste_scpt,
    ajuste_spt)

# O melhor modelo foi o com sexo, profundidade do bico e tamanho da nadadeira!

## letra c

summary(ajuste_spt)

# Todas as variáveis desse modelo tiveram p-valor menor que 0.05!

## letra d

# 53.66% da variável peso é explicada pelas variáveis sexo, profundidade do bico e tamanho da nadadeira!
