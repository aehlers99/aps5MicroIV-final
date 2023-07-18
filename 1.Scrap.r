# Trabalho Final - Grupo 10
# Antonio Ehlers, Bruno Weber, Mel Alencastro

# Parte 1 - Scrap --------------------------------------------------------------

# Instalando pacotes
install.packages("remotes")

# Mudando dns Proxy

assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
curl::has_internet() 

# Instalando bibliotecas para coleta de dados
remotes::install_github("danicat/read.dbc", force = TRUE)
remotes::install_github("rfsaldanha/microdatasus", force = TRUE)

library(microdatasus)
dados <- fetch_datasus(year_start = 2018, year_end = 2021, uf = "all", information_system = "SIM-DO")
dados <- process_sim(dados)

# Subset as variaveis desejadas do dataframe existente
new_df <- subset(dados, select = c(TIPOBITO, DTOBITO, CODMUNNATU, IDADEanos, SEXO, RACACOR, ESC2010, CODMUNOCOR, TPMORTEOCO, ASSISTMED, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, CIRCOBITO))

write.csv(new_df, "C:\\Users\\anton\\Desktop\\5o\\micro4\\Final\\newDf.csv")

# Imprimindo o novo dataframe
print(new_df)
