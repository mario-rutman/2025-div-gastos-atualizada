# 1. Importando os dados, faxinando e salvando em rds. ----------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(glue)
library(ggplot2)
library(forcats)

Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

desp_mario_maria <- read_excel("raw_data/excel_csv/despesas_mario_maria.xlsx",
  col_types = c(
    "text", "text", "text",
    "numeric", "date"
  )
) %>%
  mutate(
    data_02 = format(data, "%y-%m"),
    data_03 = format(data, "%b-%y") # Muda o formato da data.
  ) %>%
    # Usa a ordem original quando transforma em fator, por isso, no caso,
    # mantém a ordem da coluna data_03.
    mutate(data_03 = fct_inorder(data_03))
 
saveRDS(desp_mario_maria, "raw_data/rds/desp_mario_maria.rds")



# 2. Calculando o balanço mensal. -----------------------------------------

# Escolhendo a data espcífica e fazendo o balanço.
# NÃO ESQUECER DE ATUALIZAR A DATA!!!

data_atual <- "2025-04-01"

balanco <- desp_mario_maria %>%
  filter(data == ymd(data_atual), tipo == "comum") %>%
  group_by(nome) %>%
  summarise(total_mario_maria = sum(reais, na.rm = TRUE)) %>%
  arrange(nome)

# Diferença das despesas comuns dividido por 2
diferenca <- round((balanco[1, 2] - balanco[2, 2]) / 2, 0)

# Determinar quem paga e montar a mensagem usando glue.
if (diferenca > 0) {
  mensagem <- glue("O Mário deve pagar à Maria R${diferenca}")
} else if (diferenca < 0) {
  mensagem <- glue("A Maria deve pagar ao Mário R${abs(diferenca)}")
} else {
  mensagem <- "Os gastos de ambos foram iguais. Nimguém paga nada ao outro"
}

# Exibir a mensagem
print(mensagem)

# 3. Fazendo o gráfico do gasto individual total por data. -------------------------

# Calculando o total de gastos de o_mario por data.

tot_o_mario_data <- desp_mario_maria %>%
  group_by(data_03, tipo) %>%
  summarise(sub_tot = sum(reais, na.rm = TRUE)) %>%
  mutate(tot = ifelse(tipo == "comum", sub_tot / 2, sub_tot)) %>%
  group_by(data_03) %>%
  summarise(tot_mario = sum(tot, na.rm = TRUE))

# Fazendo o gráfico de colunas.

ggplot(tot_o_mario_data, aes(x = data_03, y = tot_mario)) +
  geom_col(fill = "#E95420", color = "black") + # cor laranja ubuntu
  labs(title = "Mário: total de gastos por mês", x = NULL, y = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid.major.x = element_blank(), # Remover linhas de grade verticais principais
    panel.grid.minor.x = element_blank(), # Remover linhas de grade verticais secundárias (se houver)
    panel.grid.major.y = element_line(color = "black"), # Definir cor das linhas de grade horizontais principais
    panel.grid.minor.y = element_line(color = "black"), # Remover linhas de grade horizontais secundárias (se houver)
  )

# 4. Calculando agora os totais por mes por rubrica. ----------------------------

tot_o_mario_rubrica_data <- desp_mario_maria %>%
  group_by(data_03, tipo, rubrica) %>%
  summarise(sub_tot = sum(reais, na.rm = TRUE)) %>%
  mutate(tot = ifelse(tipo == "comum", sub_tot / 2, sub_tot)) %>%
  group_by(data_03, rubrica) %>%
  summarise(tot_mario_rubrica = sum(tot, na.rm = TRUE))


# Criar o gráfico com facet_wrap
ggplot(tot_o_mario_rubrica_data, aes(x = data_03, y = tot_mario_rubrica)) +
  geom_col(fill = "green2", color = "black") + # Cor laranja Ubuntu
  facet_wrap(~rubrica, ncol = 3) + # Criar um gráfico para cada rubrica
  labs(
    title = "Mário: total de gastos por rubrica por mês",
    x = NULL,
    y = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 15), # Tamanho do texto dos títulos dos facets
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.minor.y = element_blank()
  )
