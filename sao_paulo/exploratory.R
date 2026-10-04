library(openxlsx)
library(tidyverse)
library(sidrar)
library(arrow)

df26 <- read.xlsx("dados/BancoVDE 2026.xlsx")
df26 <- df26 %>%
  mutate(data_referencia = as.Date(data_referencia, origin = "1899-12-30")) #transforma a coluna em tipo data

df_rj_26_muni <- df26 |>
  filter(uf== "RJ")

###############################33
dfrj <- read.csv2("dados/Base_DP_senasp.csv")
dfrj <- dfrj %>%
  mutate(UF = "RJ") %>%
  relocate(UF)

# Populacao estimada do RJ (tabela SIDRA 6579) - nao ha estimativa para 2022/2023 (anos de censo)
pop_rj <- get_sidra(
  x = 6579,
  variable = 9324,
  period = as.character(2015:2024),
  geo = "State",
  geo.filter = list("State" = 33)
) %>%
  select(ano = `Ano`, populacao = `Valor`) %>%
  mutate(UF = "RJ", ano = as.integer(ano)) %>% #transforma a coluna data em tipo inteiro
  relocate(UF)

#2022 e 2023 não possuem dados de população no estado do rj. o que fazer?

# Populacao estimada de todos os municipios do RJ (tabela SIDRA 6579) - mesma
# tabela/variavel de pop_rj, porem em nivel municipal (geo = "City")
pop_rj_municipios <- get_sidra(
  x = 6579,
  variable = 9324,
  period = as.character(2015:2026),
  geo = "City",
  geo.filter = list("State" = 33)
) %>%
  select(
    codigo_municipio = `Município (Código)`,
    municipio = `Município`,
    ano = `Ano`,
    populacao = `Valor`
  ) %>%
  mutate(
    UF = "RJ",
    ano = as.integer(ano),
    municipio = str_remove(municipio, " - RJ$")
  ) %>%
  relocate(UF, codigo_municipio, municipio)

# Junta a populacao estimada de cada municipio (pop_rj_municipios) ao
# df_rj_26_muni pela chave municipio + ano. A chave e normalizada em
# maiusculo porque df26$municipio vem em CAIXA ALTA e pop_rj_municipios$municipio
# vem em Title Case (retorno padrao do SIDRA); "NAO INFORMADO" nao tem
# correspondencia e fica com populacao = NA, o que e esperado.
dfrj_pop <- df_rj_26_muni %>%
  mutate(ano = year(data_referencia)) %>%
  left_join(
    pop_rj_municipios %>% mutate(municipio = str_to_upper(municipio)),
    by = c("municipio", "ano")
  ) %>%
  relocate(populacao, .before = agente)


#unifica as colunas de total vitima e total
dfrj_pop <- dfrj_pop |>
  mutate(total = coalesce(total, 0) + coalesce(total_vitima, 0) + coalesce(total_peso, 0))

#pivota as colunas
dfrj_pop <- dfrj_pop |> 
  pivot_wider(names_from = evento,
              values_from = total)
dfrj_pop <- dfrj_pop |> 
  select(-c(agente, faixa_etaria, feminino, masculino, nao_informado, total_vitima, total_peso, UF))

#reordena as colunas
df_rj_pop <- dfrj_pop |> 
  relocate(codigo_municipio, .before = populacao) |> 
  relocate(ano, .before = data_referencia)


pasta_destino <- "parquets_uf"

# 2. Criar a pasta se ela ainda não existir
if (!dir.exists(pasta_destino)) {
  dir.create(pasta_destino, recursive = TRUE)
}

# 3. Salvar o dataframe em formato .parquet dentro da nova pasta
write_parquet(df_rj_pop, file.path(pasta_destino, "RJ.parquet"))
