#gerar hexagonos 
library(h3jsr)
library(tidyverse)
library(arrow)
library(sf)
df_geo <- read_parquet("dados/SPDadosCriminais_jan_ago2026_geocodificado.parquet")
