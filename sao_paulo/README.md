# Geolocalização das ocorrências — SSP-SP (jan–ago/2026)
Este repositório tem por objetivo disponibilizar a base de dados de ocorrências de São Paulo com os endereços geolocalizados e agregados por hexágonos em diferentes resoluções.
Determinados crimes são propositalmente ocultados da geolocalização, como alternativa foi utilizada uma estratégia para inserir uma localização aproximada da delegacia de onde foi realizada a denúncia.


Explica como a coluna de latitude/longitude do dataset de ocorrências criminais foi conferida e completada, e o que foi feito com os hexágonos H3 gerados a partir dela.

## O problema de partida

O dataframe original (`df`, de `dados/SPDadosCriminais_jan_ago2026.parquet`, 732.808 linhas) tinha **22,8% dos registros sem coordenada utilizável**, por três motivos diferentes:

- **144 registros** com coordenada corrompida por um bug de formatação (faltava o ponto decimal — ex. `-235286` em vez de `-23.5286`).
- **39.740 registros** com endereço preenchido (`LOGRADOURO`, `BAIRRO`), mas sem coordenada — a própria SSP-SP não geocodificou esses boletins.
- **127.040 registros** sob sigilo legal — o campo de endereço vem com o texto "VEDAÇÃO DA DIVULGAÇÃO DOS DADOS...". São majoritariamente estupro, estupro de vulnerável e violência: não existe endereço nenhum para geocodificar.

## O que foi feito com cada grupo

| Grupo | N | Tratamento |
|---|---|---|
| Coordenada já válida | 566.028 | Mantida; os 144 casos com bug de formatação foram corrigidos matematicamente (reinserindo o ponto decimal) |
| Endereço disponível, sem coordenada | 39.740 | Geocodificado por endereço com o pacote `geocodebr`, que roda localmente sobre a base CNEFE (IBGE) — nenhum endereço de boletim é enviado a servidor externo |
| Endereço sob sigilo legal | 127.040 | Aproximado pelo **medoide** (ponto real) dos registros já válidos na mesma circunscrição de delegacia — nunca o centro do bairro, para não recriar risco de identificar a vítima |

**Nenhuma linha foi excluída.** Todo registro ficou com uma coordenada final (`LATITUDE_FINAL`/`LONGITUDE_FINAL`), e cada um carrega de onde essa coordenada veio (`fonte_geo`) e quão exata ela é (`precisao_geo`) — para que uma aproximação nunca seja confundida com um ponto exato numa análise posterior.

13 registros tinham coordenada corrompida de um jeito fora do padrão do bug dos 144 — foram geocodificados por endereço como os demais 39.740, mas ficam marcados (`revisar_manual = TRUE`) para quem quiser auditar a causa raiz depois.

## Hexágonos H3 (resoluções 4, 6, 8 e 9)

Para mapear a base em grades hexagonais sem recriar o problema acima: como os 127.040 registros sigilosos de uma mesma circunscrição compartilham a mesma coordenada aproximada, contá-los direto no hexágono dessa coordenada criaria um pico artificial num único hexágono (e zero nos vizinhos) nas resoluções mais finas (8 e 9).

Em vez disso, cada ocorrência sem ponto confiável é **redistribuída fracionadamente** entre os hexágonos onde já existem ocorrências confiáveis da mesma unidade (circunscrição, bairro ou município), seguindo o padrão espacial real já observado ali. O total por unidade não muda — só deixa de ser um ponto único arbitrário. A soma de ocorrências por hexágono foi conferida e bate exatamente com 732.808 nas quatro resoluções.

## Scripts e arquivos gerados

| Script | Gera |
|---|---|
| `conferir_geolocalizacao_ocorrencias.R` | `dados/SPDadosCriminais_jan_ago2026_geocodificado.parquet` (df_geo) |
| `gerar_hexagonos_df_geo.R` | `dados/hex_r{4,6,8,9}_df_geo.rds` / `.parquet` (grades hexagonais) e `dados/SPDadosCriminais_jan_ago2026_geo_hex.parquet` (df_geo + índice H3 por linha) |

### Colunas novas em `df_geo`

- `LATITUDE_FINAL`, `LONGITUDE_FINAL` — coordenada final a usar
- `LATITUDE_ORIGINAL`, `LONGITUDE_ORIGINAL` — coordenada bruta, preservada
- `fonte_geo` — `original`, `geocodebr` ou `aproximado_circunscricao_medoide`
- `precisao_geo` — nível de precisão (`exata_original`, `numero`, `numero_aproximado`, `logradouro`, `localidade`, `municipio`, `circunscricao_policial`)
- `sigilo_legal` — `TRUE` para os registros sem endereço por sigilo
- `revisar_manual` — `TRUE` para os 13 casos de coordenada corrompida fora do padrão

## Cuidado ao usar

Não plotar os registros sigilosos como pontos individuais num mapa — a aproximação serve para contagem agregada por região, não para apontar um local específico. Ao montar mapas de calor, preferir as grades hexagonais já redistribuídas (`dados/hex_r*_df_geo.*`) em vez de recalcular a partir da coordenada bruta.
