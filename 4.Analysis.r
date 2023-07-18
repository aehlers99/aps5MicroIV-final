# Trabalho Final - Grupo 10
# Antonio Ehlers, Bruno Weber, Mel Alencastro

# Parte 4 - Analise Descritiva -------------------------------------------------

# Bibliotecas ------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(stats)
library(TSstudio)

# Base de Dados ----------------------------------------------------------------
data <- read.csv("C:/Users/Lenovo/Desktop/Insper/5º SEM/Micro IV/Trabalho Final/PF_Final_M4/dataClean.csv", header = TRUE)
data <- data.frame(X = data$X[data$DTOBITO < "2020-02-21"],
                   TIPOBITO = data$TIPOBITO[data$DTOBITO < "2020-02-21"],
                   DTOBITO = data$DTOBITO[data$DTOBITO < "2020-02-21"],
                   CODMUNNATU = data$CODMUNNATU[data$DTOBITO < "2020-02-21"],
                   IDADEanos = data$IDADEanos[data$DTOBITO < "2020-02-21"],
                   SEXO = data$SEXO[data$DTOBITO < "2020-02-21"],
                   ESC2010 = data$ESC2010[data$DTOBITO < "2020-02-21"],
                   CODMUNOCOR = data$CODMUNOCOR[data$DTOBITO < "2020-02-21"],
                   id = data$id[data$DTOBITO < "2020-02-21"],
                   year = data$year[data$DTOBITO < "2020-02-21"],
                   month = data$month[data$DTOBITO < "2020-02-21"],
                   day = data$day[data$DTOBITO < "2020-02-21"])
data <- data.frame(X = data$X[data$DTOBITO > "2019-10-30"],
                   TIPOBITO = data$TIPOBITO[data$DTOBITO > "2019-10-30"],
                   DTOBITO = data$DTOBITO[data$DTOBITO > "2019-10-30"],
                   CODMUNNATU = data$CODMUNNATU[data$DTOBITO > "2019-10-30"],
                   IDADEanos = data$IDADEanos[data$DTOBITO > "2019-10-30"],
                   SEXO = data$SEXO[data$DTOBITO > "2019-10-30"],
                   ESC2010 = data$ESC2010[data$DTOBITO > "2019-10-30"],
                   CODMUNOCOR = data$CODMUNOCOR[data$DTOBITO > "2019-10-30"],
                   id = data$id[data$DTOBITO > "2019-10-30"],
                   year = data$year[data$DTOBITO > "2019-10-30"],
                   month = data$month[data$DTOBITO > "2019-10-30"],
                   day = data$day[data$DTOBITO > "2019-10-30"])
data = na.omit(data)

# Definindo a Fonte para os Graficos -------------------------------------------
windowsFonts(goth = windowsFont("Century Gothic"))

# Analise Descritiva -----------------------------------------------------------
## Idade
IDADE <- as.numeric(data$IDADEanos)

hist(IDADE, freq = FALSE, breaks = (max(IDADE) - min(IDADE)),
     col = "#BA181B", border = "#660708",
     xlim = c(min(IDADE), max(IDADE)), ylim = c(0, 0.04),
     main = "Frequencia Relativa de Suicidios por Idade",
     xlab = "Idade", ylab = "Densidade", family = "goth")

data3 <- data %>% mutate(ageGroup = case_when(
  IDADEanos >= 15 & IDADEanos < 20 ~ "15-19",
  IDADEanos >= 20 & IDADEanos < 25 ~ "20-24",
  IDADEanos >= 25 & IDADEanos < 30 ~ "25-29",
  IDADEanos >= 30 & IDADEanos < 40 ~ "30-39",
  IDADEanos >= 40 & IDADEanos < 50 ~ "40-49",
  IDADEanos >= 50 & IDADEanos < 60 ~ "50-59",
  IDADEanos >= 60 ~ "Mais de 60"))

tab1 <- data3 %>%
  group_by(ageGroup) %>%
  summarise(n = n())

tab1 = data.frame(Faixa = tab1$ageGroup,
                  Suicidios = tab1$n,
                  Media = c(mean(data3$IDADEanos[data3$ageGroup == "15-19"]),
                            mean(data3$IDADEanos[data3$ageGroup == "20-24"]),
                            mean(data3$IDADEanos[data3$ageGroup == "25-29"]),
                            mean(data3$IDADEanos[data3$ageGroup == "30-39"]),
                            mean(data3$IDADEanos[data3$ageGroup == "40-49"]),
                            mean(data3$IDADEanos[data3$ageGroup == "50-59"]),
                            mean(data3$IDADEanos[data3$ageGroup == "Mais de 60"])))
View(tab1)

## Sexo
data$SEXO <- as.factor(data$SEXO)

data %>% 
  ggplot(aes(x = SEXO)) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), fill = "#BA181B") +
  labs(x = "Sexo", y = "Frequência Relativa", title = "Frequencia Relativa de Suicidios por Sexo") +
  theme(plot.title = element_text(hjust = 0.7, face = "bold"), 
        text = element_text(family = "goth"), 
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgrey"))

## Mortes por Dia
### Base de Dados Temporal
dataT <- tapply(data$DTOBITO, data$DTOBITO, length)

dataT <- data.frame(dataT)
dataTs <- dataT

dataT$per <- seq.int(nrow(dataT))

dataT %>%
  ggplot(aes(x = per, y = dataT)) +
  geom_line(color = "#BA181B", size = 0.75) +
  geom_vline(xintercept = 57, color = "black", lwd = 1, lty = 2) +
  labs(x = "Dia", y = "Número de Suicidios", title = "Série Temporal de Suicidios por Data") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        text = element_text(family = "goth"), 
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgrey"))

tab2 = data.frame(Categoria = c("Idade", "Casos/Dia"),
                  Minimo = c(min(as.numeric(data$IDADEanos)), min(dataT$dataT)),
                  Media = c(mean(as.numeric(data$IDADEanos)), (mean(dataT$dataT))),
                  Maximo = c(max(as.numeric(data$IDADEanos)), max(dataT$dataT)))
View(tab2)
