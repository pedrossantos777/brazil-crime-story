# Documentação dos Processos — `security_project`

Este documento descreve, em ordem cronológica, os processos implementados em
`exploratory.R`, o motivo de cada um e o procedimento lógico por trás da
implementação. Serve como referência para reproduzir ou estender a análise.

---

## 1. Carregamento do banco nacional (`df26`)

```r
df26 <- read.xlsx("dados/BancoVDE 2026.xlsx")
```

**Objetivo:** carregar a base nacional (BancoVDE 2026) para uso futuro na análise.

**Procedimento lógico:**
- Usa `openxlsx::read.xlsx()` para ler a primeira planilha do arquivo `.xlsx`.
- Não há tratamento adicional nesta etapa — é apenas a leitura bruta do arquivo.

---

## 2. Carregamento da base do RJ (`dfrj`) e inclusão da coluna `UF`

```r
dfrj <- read.csv2("dados/Base_DP_senasp.csv")
dfrj <- dfrj %>%
  mutate(UF = "RJ") %>%
  relocate(UF)
```

**Objetivo:** carregar a base de segurança pública do Rio de Janeiro
(`Base_DP_senasp.csv`, dados por delegacia/CISP no padrão ISP-RJ/SENASP) e
identificar explicitamente a unidade federativa de origem dos dados, já que o
arquivo original não traz essa informação.

**Procedimento lógico:**
1. `read.csv2()` é usado (em vez de `read.csv()`) porque o arquivo usa `;`
   como separador de campos — padrão comum em exportações de dados
   brasileiros (BR locale, decimal com vírgula).
2. `mutate(UF = "RJ")` cria uma coluna constante, já que toda a base se
   refere ao estado do Rio de Janeiro.
3. `relocate(UF)` move a coluna recém-criada para a primeira posição do
   data frame, sem alterar a ordem das demais colunas.

**Resultado:** `dfrj` passa a ter `UF` como primeira coluna, preenchida com
`"RJ"` em todas as linhas, seguida das colunas originais (`ano`, `mes`,
`risp`, `aisp`, `cisp`, indicadores criminais, etc.).

---

## 3. Consulta à população estimada do RJ via SIDRA (`pop_rj`)

```r
pop_rj <- get_sidra(
  x = 6579,
  variable = 9324,
  period = as.character(2015:2024),
  geo = "State",
  geo.filter = list("State" = 33)
) %>%
  select(ano = `Ano`, populacao = `Valor`) %>%
  mutate(UF = "RJ", ano = as.integer(ano)) %>%
  relocate(UF)
```

**Objetivo:** obter a série de população residente estimada do estado do Rio
de Janeiro junto ao IBGE (via SIDRA), para permitir o cálculo futuro de taxas
(ex.: homicídios por 100 mil habitantes) a partir do `dfrj`.

**Procedimento lógico:**
1. `get_sidra()` (pacote `sidrar`) consulta a **tabela 6579** do SIDRA
   ("População residente estimada"), variável **9324**
   (população residente estimada, em pessoas).
2. `geo = "State"` + `geo.filter = list("State" = 33)` restringem a consulta
   ao nível geográfico de Unidade da Federação, filtrando pelo código do
   IBGE para o Rio de Janeiro (`33`).
3. `period` solicita o intervalo de 2015 a 2024, coerente com o intervalo de
   anos presente em `dfrj`.
4. `select()` renomeia as colunas retornadas pelo SIDRA (`Ano`, `Valor`) para
   nomes mais convenientes (`ano`, `populacao`).
5. `mutate(UF = "RJ", ano = as.integer(ano))` cria a chave de junção `UF` e
   garante que `ano` seja numérico (o SIDRA retorna a coluna de ano como
   texto), compatibilizando o tipo com `dfrj$ano`.
6. `relocate(UF)` posiciona `UF` como primeira coluna, no mesmo padrão usado
   em `dfrj`.

**Observação importante:** a tabela 6579 do SIDRA **não possui estimativas
para anos de Censo** (no caso, 2022 e 2023), pois nesses anos a população é
apurada por contagem direta (Censo), não por estimativa. Por isso, `pop_rj`
não contém linhas para 2022 e 2023 — isso é esperado e tratado na etapa de
junção (seção 4).

---

## 4. Junção da população ao `dfrj` (coluna `populacao`)

```r
dfrj <- dfrj %>%
  left_join(pop_rj, by = c("UF", "ano"))
```

**Objetivo:** anexar ao `dfrj` a população estimada do RJ correspondente a
cada ano, permitindo cálculos de taxas por habitante linha a linha.

**Procedimento lógico:**
1. `left_join()` preserva todas as linhas de `dfrj` (a tabela da esquerda),
   trazendo a coluna `populacao` de `pop_rj` sempre que houver
   correspondência pela chave composta `UF` + `ano`.
2. A chave é composta por `UF` e `ano` porque `pop_rj` é uma série anual e
   `dfrj` é uma base mensal (`ano` + `mes`) — a junção repete o mesmo valor
   de população para todos os meses de um mesmo ano.
3. Como `pop_rj` não cobre 2022 e 2023 (ver seção 3), as linhas de `dfrj`
   referentes a esses dois anos recebem `populacao = NA` após o `left_join`.
   Isso é o comportamento esperado e correto — não é um erro de junção.

**Resultado:** `dfrj` passa a conter a coluna `populacao` ao final, com o
valor anual da população estimada repetido em todas as linhas/meses do
respectivo ano, e `NA` nos anos censitários (2022 e 2023) até que uma fonte
alternativa (ex.: dados do Censo) seja incorporada para preencher essa
lacuna, se necessário.

---

## Resumo do fluxo de dados

```
BancoVDE 2026.xlsx ──► df26 (leitura bruta)

Base_DP_senasp.csv ──► dfrj ──► + coluna UF (constante "RJ", 1ª posição)
                                        │
SIDRA (tabela 6579, var. 9324) ──► pop_rj ──► + coluna UF, ano (int)
                                        │
                                        ▼
                        dfrj + populacao (left_join por UF + ano)
```
