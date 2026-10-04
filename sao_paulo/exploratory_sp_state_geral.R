library(tidyverse)
library(openxlsx)
library(arrow)
library(sf)

df <-  read.xlsx("dados/SPDadosCriminais_2026.xlsx", sheet = 2)
#exportar para parquet

df2 <- write_parquet(df,"dados/SPDadosCriminais_janjun2026.parquet")
df1 <- read_parquet("dados/SPDadosCriminais_2026.parquet")

df_unificado <- bind_rows(df1, df2)

df_unificado <- write_parquet(df_unificado,"dados/SPDadosCriminais_jan_ago2026.parquet")
