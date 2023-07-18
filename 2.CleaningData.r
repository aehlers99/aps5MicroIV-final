# Trabalho Final - Grupo 10
# Antonio Ehlers, Bruno Weber, Mel Alencastro

# Parte 2 - Limpeza dos Dados --------------------------------------------------

# Bibliotecas

library(tidyverse)
library(ggplot2)

# Importando os dados do arquivo csv

library(readr)

locale <- readr::locale(decimal_mark = ",", grouping_mark = ".")

data <- read_csv("newDf.csv", locale = locale)
dataAux <- data
data <- dataAux
View(head(data,10))
dim(data)

# Deletando a coluna TPMORTEECO

data <- data %>% select(-c(TPMORTEOCO, CIRCOBITO, ASSISTMED, RACACOR))

# Convertendo DTOBITO para formato de data

data$DTOBITO <- as.Date(data$DTOBITO, format = "%d/%m/%Y")

##Separate data by year month and day
#
#data$year <- format(data$DTOBITO, "%Y")
#data$month <- format(data$DTOBITO, "%m")
#data$day <- format(data$DTOBITO, "%d")
#
##Filtering data date
#
## Filter out rows with year == 2018 and month < 12, day 27
#data <- data[!(data$year == "2019" & (data$month >= "10") & (data$day >= "27") ), ]
##data <- data %>% filter((data$year == "2019" & (data$month >= "10") & (data$day >= "27") ))
#View(head(data,10))
## Filter out rows with year == 2020 and month > 12, day = 22
#data <- data[!(data$year == "2020" & (data$month <= "2" ) & (data$day <= "22")), ]

# Deletando todos TIPOBITO != Nao Fetal

data <- data %>% filter(TIPOBITO == "N\xe3o Fetal")

# Deletando idade < 15

data <- data %>% filter(IDADEanos >= 15)

# Removendo * de todas as linhas da coluna LINHAA

data$LINHAA <- gsub("\\*", "", data$LINHAA)
data$LINHAB <- gsub("\\*", "", data$LINHAB)
data$LINHAC <- gsub("\\*", "", data$LINHAC)
data$LINHAD <- gsub("\\*", "", data$LINHAD)
data$LINHAII <- gsub("\\*", "", data$LINHAII)

# Separando uma string em letras e numeros, adicionando uma nova coluna

data <- data %>% separate(LINHAA, c("LINHAA", "LINHAA2"), sep = "(?<=[A-Za-z])(?=[0-9])")
data <- data %>% separate(LINHAB, c("LINHAB", "LINHAB2"), sep = "(?<=[A-Za-z])(?=[0-9])")
data <- data %>% separate(LINHAC, c("LINHAC", "LINHAC2"), sep = "(?<=[A-Za-z])(?=[0-9])")
data <- data %>% separate(LINHAD, c("LINHAD", "LINHAD2"), sep = "(?<=[A-Za-z])(?=[0-9])")
data <- data %>% separate(LINHAII, c("LINHAII", "LINHAII2"), sep = "(?<=[A-Za-z])(?=[0-9])")

# Removendo o terceiro caractere da LINHA

dataAux <- data
data <- dataAux

# Deletando todas as linhas de LINHA2 que sao diferentes de X

data <- data %>% filter(LINHAA2 == "X" & LINHAB2 == "X" & LINHAC2 == "X" & LINHAD2 == "X" & LINHAII2 == "X")
View(head(data,10))

data$LINHAA2 <- substr(data$LINHAA2, 1, nchar(data$LINHAA2)-1)
data$LINHAB2 <- substr(data$LINHAB2, 1, nchar(data$LINHAB2)-1)
data$LINHAC2 <- substr(data$LINHAC2, 1, nchar(data$LINHAC2)-1)
data$LINHAD2 <- substr(data$LINHAD2, 1, nchar(data$LINHAD2)-1)
data$LINHAII2 <- substr(data$LINHAII2, 1, nchar(data$LINHAII2)-1)
View(head(data,10))

# Filtrando as datas corretas

data$DTOBITO <- as.Date(data$DTOBITO, format = "%d/%m/%Y")

data$year <- format(data$DTOBITO, "%Y")
data$month <- format(data$DTOBITO, "%m")
data$day <- format(data$DTOBITO, "%d")

# Deletando todos os anos diferentes de 2019 e 2020

data <- data[(data$year == "2019" | data$year == "2020"), ]

# Transformando month e day para valores numericos

data$month <- as.numeric(data$month)
data$day <- as.numeric(data$day)

# Selecionando meses (10, 11, 12) de 2019 e (1,2) de 2020

data <- data[(data$year == "2019" & (data$month >= "10")) | (data$year == "2020" & (data$month <= "2")), ]
dataT <- data[(data$year == "2020" & (data$month <= "2")), ]

# Salvando em csv

write.csv(data, "D:\\dataClean.csv")
