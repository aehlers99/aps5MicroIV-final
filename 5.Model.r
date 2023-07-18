# Trabalho Final - Grupo 10
# Antonio Ehlers, Bruno Weber, Mel Alencastro

# Parte 5 - Modelagem ----------------------------------------------------------

# Bibliotecas ------------------------------------------------------------------
library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(fixest)
library(stargazer)
library(xlsx)

# Bases de Dados ---------------------------------------------------------------
data <- read.csv("C:/Users/Lenovo/Desktop/Insper/5º SEM/Micro IV/Trabalho Final/PF_Final_M4/Bases de Dados/dataClean.csv", header = TRUE)
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

pop <- read_excel("C:/Users/Lenovo/Desktop/Insper/5º SEM/Micro IV/Trabalho Final/PF_Final_M4/Bases de Dados/pop.xlsx")
pop$CODMUNOCOR <- substr(pop$Município, 1, 6)
pop$names <- substr(pop$Município,8, 100)
pop$CODMUNOCOR <- as.numeric(pop$CODMUNOCOR)

## Unindo as Bases
data <- left_join(data, pop, by = "CODMUNOCOR")

## Deletando Colunas Criadas pelo Merge
data <- data %>% select(-c(Município))

## Criando as Semanas
data_inicial <- as.Date("2019-10-31")
data_final <- as.Date("2020-02-20")

datas <- seq(data_inicial, data_final, by = "day")

semanas_referencia <- format(datas, "%Y-%U")

## Criando o DataFrame
df <- data.frame(datas, semanas_referencia)

df$week <- substr(df$semanas_referencia, 6, 7) %>% as.numeric()

## Comparando Datas
for (i in 1:nrow(data)) {
  for (j in 1:nrow(df)) {
    if (data$DTOBITO[i] == df$datas[j]) {
      data$week[i] <- df$week[j]}}}

dataAux <- data
dataAux$week <- as.integer(dataAux$week)
dataAux$week <- ifelse(dataAux$week == -41, dataAux$week + 42, dataAux$week)
dataAux$week <- dataAux$week - 42
dataAux$week <- ifelse(dataAux$week < 0, dataAux$week + 52, dataAux$week)

data <- dataAux

## Deletando Coluna com Nomes Conflitantes
data$popMun <- data$População_residente
data <- data %>% select(-c(População_residente))

## Agrupando os Dados por "names" e "week" e Calculando a Taxa
dataT <- data %>%
  group_by(names, week)%>%
  summarise(rate = n()/popMun)%>%
  ungroup()

## Deletando as Linhas que Aparecem mais de Uma Vez
dataT <- dataT %>% distinct(names, week, .keep_all = TRUE)

## Criando uma Lista com Todos os Nomes que Aparecem Pelo Menos Uma Vez
namesUnique <- dataT$names %>% unique()
dataNames <- data.frame(namesUnique)

## Repetindo os Nomes de dataNames 16 Vezes no DataFrame dataNames
dataNames <- dataNames[rep(row.names(dataNames), 16),]
dataNames <- data.frame(dataNames)

## Ordenando o DataFrame dataNames por names
dataNames <- dataNames[order(dataNames$dataNames),]

## Criando um Loop Dentro de um Loop para Criar a Coluna "week" com Valores de 1 a 16 para Cada "name"
dataNames <- data.frame(dataNames)
repetitions <- ceiling(nrow(dataNames) / 16)

## Criando a Coluna de Contagem
dataNames$week <- rep(1:16, length.out = nrow(dataNames))[1:nrow(dataNames)]

## Unindo dataNames e dataT por "names" e Ordem
dataNames <- left_join(dataNames, dataT, by = c("dataNames" = "names", "week" = "week"))

## Trocando NA para 0
dataNames$rate<-ifelse(is.na(dataNames$rate), 0, dataNames$rate)

## Transformando dataNames para Numerico
dataNames$week <- as.numeric(dataNames$week)
dataNames$week = dataNames$week - 8
dataNames$dummyLaw <- ifelse(dataNames$week < 0, 0, 1)

# Event Study ------------------------------------------------------------------

## Efeitos da Dummy_law
mod_twfe = feols(rate ~ i(dummyLaw, week) | factor(dataNames), data = dataNames)
iplot(mod_twfe)
summary(mod_twfe)

## Dummy_law e week
mod_test = feols(rate ~  i(week, dummyLaw, ref = 0) | factor(dataNames), 
                 cluster = ~ week,
                 data = dataNames)
iplot(mod_test, type = "b")
summary(mod_test)

## Regressao MQO
mod_twfe = feols(rate ~ i(week) | factor(dataNames), cluster = ~week, data = dataNames)
plot(mod_twfe)

ttt<- lm(rate ~ dummyLaw + as.factor(dataNames) + week, data = dataNames)
ols <- lm(rate~week+dummyLaw+factor(dataNames), data = dataNames)
summary(ttt)
plot(ols)

x1 <- data.frame(summary(ols))

## Exportar
stargazer(ols, type = "text") # inclui Dec

model_df <- data.table(coef(summary(ols)), keep.rownames = 'term')

write.csv(model_df, 'model_df.csv')
