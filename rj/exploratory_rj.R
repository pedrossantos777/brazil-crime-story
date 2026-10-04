library(tidyverse)
library(arrow)

df1 <- read.csv("dados/BaseDPEvolucaoMensalCisp.csv", sep=';', fileEncoding = "latin1")
df2 <- read.csv("dados/DOMensalEstadoDesde1991.csv", sep=';', fileEncoding = "latin1")
