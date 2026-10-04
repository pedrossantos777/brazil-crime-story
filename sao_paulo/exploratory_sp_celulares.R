library(tidyverse)
library(openxlsx)
library(readxl)
library(sf)
library(arrow)
library(stringr) # Para tratar texto, se necessário

df_cel <- read.csv("dados/CelularesSubtraidos_2026 - CELULAR_2026.csv")
# df_sp <- read_xlsx("dados/SPDadosCriminais_2026.xlsx", sheet = 3)


#remover duplicados
df_cel <- df_cel %>% distinct()


df_cel <- df_cel %>%
  # 1. Corrige vírgulas por pontos (caso suas coordenadas estejam no padrão brasileiro " -23,55")
  mutate(
    LONGITUDE = str_replace(as.character(LONGITUDE), ",", "."),
    LATITUDE  = str_replace(as.character(LATITUDE), ",", ".")
  ) %>%
  
  # 2. Converte explicitamente para numérico
  mutate(
    LONGITUDE = as.numeric(LONGITUDE),
    LATITUDE  = as.numeric(LATITUDE)
  ) %>%
  
  # 3. Remove linhas que possuem coordenadas ausentes (NA)
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))

#transformar lat long em objeto sf
df_cel <- df_cel %>%
  st_as_sf(
    coords = c("LONGITUDE", "LATITUDE"), # IMPORTANTE: Longitude (X) sempre vem antes de Latitude (Y)
    crs = 4326                           # WGS 84 (sistema padrão de coordenadas GPS)
  )

#escrever em parquet dados de sao paulo geolocalizados
df_cel_tabular <- st_drop_geometry(df_cel)

# Write tabular data
write_parquet(df_cel_tabular, "dados/df_cel_sp.parquet")


#selecionar apenas cidade de sp

df_cel_sp <- df_cel |> 
  filter(CIDADE == "S.PAULO")

