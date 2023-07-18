# Trabalho Final - Grupo 10
# Antonio Ehlers, Bruno Weber, Mel Alencastro

# Parte 3 - Manipulacao --------------------------------------------------------

# Bibliotecas
library(tidyverse)
library(ggplot2)
library(knitr)

# Importando dados de um arquivo csv
library(readr)

locale <- readr::locale(decimal_mark = ",", grouping_mark = ".")

data <- read_csv("D:/newDf_v2.1.csv", locale=locale)

dataAux <- data

# Convertendo LINHAA que nao e "X" para NA

View(head(data,10))
data$LINHAA <- ifelse(data$LINHAA != "X", NA, data$LINHAA)
data$LINHAB <- ifelse(data$LINHAB != "X", NA, data$LINHAB)
data$LINHAC <- ifelse(data$LINHAC != "X", NA, data$LINHAC)
data$LINHAD <- ifelse(data$LINHAD != "X", NA, data$LINHAD)
data$LINHAII <- ifelse(data$LINHAII != "X", NA, data$LINHAII)

View(head(data, 10))
dim(data)

# Deletando todas as linhas em que pelo menos LINHAA/LINHAB/LINHAC/LINHAD/LINHAII nao e NA

data <- data[!is.na(data$LINHAA) | !is.na(data$LINHAB) | !is.na(data$LINHAC) | !is.na(data$LINHAD) | !is.na(data$LINHAII), ]
dim(data)

# Removendo o terceiro numero da linha auxiliar LINHAA

data$LINHAA2 <- substr(data$LINHAA2, 1, 2) %>% as.integer()
data$LINHAB2 <- substr(data$LINHAB2, 1, 2) %>% as.integer()
data$LINHAC2 <- substr(data$LINHAC2, 1, 2) %>% as.integer()
data$LINHAD2 <- substr(data$LINHAD2, 1, 2) %>% as.integer()
data$LINHAII2 <- substr(data$LINHAII2, 1, 2) %>% as.integer()

# Checando se muncod e NA

data <- data[!is.na(data$CODMUNOCOR), ]

# Convertendo toda linhas que e ]64, 84[ para NA

dataT <- data %>% mutate(
    LINHAA2 = ifelse(LINHAA2 > 63 & LINHAA2 < 85, NA, LINHAA2),
    LINHAB2 = ifelse(LINHAB2 > 63 & LINHAB2 < 85, NA, LINHAB2),
    LINHAC2 = ifelse(LINHAC2 > 63 & LINHAC2 < 85, NA, LINHAC2),
    LINHAD2 = ifelse(LINHAD2 > 63 & LINHAD2 < 85, NA, LINHAD2),
    LINHAII2 = ifelse(LINHAII2 > 63 & LINHAII2 < 85, NA, LINHAII2))

# Deletando todas as linhas em que pelo menos LINHAA2/LINHAB2/LINHAC2/LINHAD2/LINHAII2 nao e NA

dataT <- dataT[!is.na(dataT$LINHAA2) | !is.na(dataT$LINHAB2) | !is.na(dataT$LINHAC2) | !is.na(dataT$LINHAD2) | !is.na(dataT$LINHAII2), ]

dim(data)

# Deletando todas as colunas sem causa de morte

dataAux <- data

data <- data %>% select(-c(LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, LINHAA2, LINHAB2, LINHAC2, LINHAD2, LINHAII2))

# Deletando colunas criadas pelos merges

data <- data %>% select(-c(...1, ...2))

# Gerando um id aleatorio para cada linha (caso seja necessario merge algo futuramente)

data$id <- sample(1:1000000, nrow(data), replace = FALSE)

length(unique(data$id))

# Salvando em csv

write.csv(data, "C:\\Users\\anton\\Desktop\\5o\\micro4\\Final\\newDf_v3.csv")
