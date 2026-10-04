#teste funcao mapas cloropleticos
library(tidyverse)

mapa_roubo <- mapa_coropletico_crime(crime = "FURTO DE VEÍCULO", resolucao = 8, municipio = "S.PAULO")

print(mapa_roubo)

ggsave(filename = "graphics/furtos_veiculo_spcity.png", plot = mapa_roubo)
