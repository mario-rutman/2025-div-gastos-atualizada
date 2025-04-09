
# 1. Importando os dados e salvando em rds. ----------------------------

library(tidyverse)
library(readxl)
desp_mario_maria <- read_excel("raw_data/excel_csv/despesas_mario_maria.xlsx", 
                                   col_types = c("text", "text", "text", 
                                                 "numeric", "date"))

saveRDS(desp_mario_maria, "raw_data/rds/desp_mario_maria.rds")



# 2. Calculando o balanço mensal. -----------------------------------------

library(dplyr)
library(lubridate)
library(glue)

# Supondo que seu dataframe 'desp_mario_maria' já esteja carregado

# Escolhendo a data espcífica e fazendo o balanço.
# NÃO ESQUECER DE ATUALIZAR A DATA!!!

data_atual = "2025-04-01"

balanco <- desp_mario_maria %>%
    filter(data == ymd(data_atual), tipo == "comum") %>% 
    group_by(nome) %>%
    summarise(total_mario_maria = sum(reais, na.rm = TRUE)) %>% 
    arrange(nome)
    
# Diferença das despesas comuns dividido por 2
diferenca <- round((balanco[1,2] - balanco[2,2]) / 2, 0) 

# Determinar quem paga e montar a mensagem usando glue.
if (diferenca > 0) {
    mensagem <- glue("O Mário deve pagar à Maria R${diferenca}")
} else if (diferenca < 0) {
    mensagem <- glue("A Maria deve pagar ao Mário R${abs(diferenca)}")
} else {
    mensagem <- "Os gastos de O Mário e A Maria são iguais."
}

# Exibir a mensagem
print(mensagem)
