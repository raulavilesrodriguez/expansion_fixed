library(tidyverse)
library(dplyr)
library(rvest)
library(httr)
library(readxl)
library(stringr)
library(purrr) # to split strings
library(hrbrthemes)
library(viridis) # pallette of colors
library(viridisLite) # pallette of colors
library(highcharter)
library(writexl)

#________Wrangling_________
df.lineas <- read_excel('CONSOLIDADO_STF_SEPTIEMBRE_2023.xlsx')
df.poblacion <- read_excel('poblacion.xlsx')
df.ivap <- read_excel('ivap.xlsx')

# transform to numeric the poblation variable
colnames(df.poblacion)[colnames(df.poblacion) == '2020'] <- 'poblacion' 
df.poblacion[,9] <- lapply(df.poblacion[,9], as.numeric)
df.poblacion$poblacion <- ifelse(is.na(df.poblacion$poblacion), 0, df.poblacion$poblacion)
df.poblacion <- df.poblacion |> mutate(poblacion = round(poblacion, 0))

colnames(df.lineas)[7] <- "DPA_PARROQ"
df.lineas[which(is.na(df.lineas$POPULAR)), which(colnames(df.lineas) == 'POPULAR')] <- 0
df.lineas[which(is.na(df.lineas$RESIDENCIAL)), which(colnames(df.lineas) == 'RESIDENCIAL')] <- 0
df.lineas[which(is.na(df.lineas$COMERCIAL)), which(colnames(df.lineas) == 'COMERCIAL')] <- 0
df.lineas[which(is.na(df.lineas$`LÍNEAS DE SERVICIO`)), which(colnames(df.lineas) == 'LÍNEAS DE SERVICIO')] <- 0
df.lineas[which(is.na(df.lineas$`TERMINALES DE USO PUBLICO`)), which(colnames(df.lineas) == 'TERMINALES DE USO PUBLICO')] <- 0

df.lineas <- df.lineas |> 
  mutate(total = POPULAR + 
           RESIDENCIAL + 
           COMERCIAL + 
           `LÍNEAS DE SERVICIO`  + 
           `TERMINALES DE USO PUBLICO`)

df.lineas <- df.lineas[, c(1, 7, 8, 29)]

# if i wanna to especify to OPERADORA group_by(DPA_PARROQ, OPERADORA)
df.lineas <- df.lineas |> group_by(DPA_PARROQ) |>
  summarise(
    PARROQUIA = unique(PARROQUIA),
    lineas = sum(total)
    )

# join tibbles
df.total <- left_join(df.poblacion, df.lineas, by = "DPA_PARROQ")
df.total <- df.total |> mutate(CODP = as.numeric(DPA_PARROQ))

# IVAP
df.ivap <- df.ivap[, c(2, 4, 30)]
colnames(df.ivap)[3] <- "ivap"
df.ivap[,3] <- lapply(df.ivap[,3], as.numeric)

# join tibbles
df.total <- left_join(df.total, df.ivap, by = "CODP")
df.total$ivap <- ifelse(is.na(df.total$ivap), 0, df.total$ivap)

df.sin <- df.total |> 
  filter(TIPO == "CABECERA SIN DESAGREGACION" | TIPO == "RURAL") 
df.sin <- df.sin |>filter(is.na(lineas))

df <- df.sin[, c(-7, -10, -11, -12, -13)]
colnames(df)[2] <- "Provincia"
colnames(df)[4] <- "Canton"
colnames(df)[6] <- "Parroquia"

#Export tibble witout service fixed telecom
writexl::write_xlsx(df, 'dfsinservicio.xlsx')



