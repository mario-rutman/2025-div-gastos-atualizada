# 1. Importando os dados do googlesheets, faxinando e salvando em rds. ----------------------------

# Escolhendo a data espcífica e fazendo o balanço.
# NÃO ESQUECER DE ATUALIZAR A DATA!!!

data_atual <- "2025-09-01"

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(glue)
library(ggplot2)
library(forcats)
library(googlesheets4)

Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

# Importar a primeira aba da planilha ''
url_da_planilha <- "https://docs.google.com/spreadsheets/d/1qlUhf-nd9jSDocQMZ75SI9ZOzM7gU8QIYLk3aoUv4y8/edit?gid=0#gid=0"

# Faxinando e salvando.
desp_mario_maria <- read_sheet(url_da_planilha) %>%
  mutate(
    data_02 = format(data, "%y-%m"),
    data_03 = format(data, "%b-%y") # Muda o formato da data.
  ) %>%
  # Usa a ordem original quando transforma em fator, por isso, no caso,
  # mantém a ordem da coluna data_03.
  mutate(data_03 = fct_inorder(data_03))

saveRDS(desp_mario_maria, "R projetos no Acer Aspire 3/2025_div_gastos_atualizadas/raw_data/rds/desp_mario_maria.rds")


# 2. Calculando o balanço mensal. -----------------------------------------

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
  mensagem <- "Os gastos de ambos foram iguais. Ninguém paga nada ao outro"
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
  summarise(tot_mario = sum(tot, na.rm = TRUE)) %>%
  mutate(total_simplif = round(tot_mario / 1000, 1))

# Fazendo o gráfico de colunas.

ggplot(tot_o_mario_data, aes(x = data_03, y = tot_mario)) +
  geom_col(fill = "#E95420", color = "black") + # cor laranja ubuntu
  geom_text(aes(label = total_simplif, fontface = "bold"), vjust = -0.3, size = 5.0) + # Adiciona os valores
  labs(title = "Mário: total de gastos por mês (em milhares de R$).", x = NULL, y = NULL) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
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
  summarise(tot_mario_rubrica = sum(tot, na.rm = TRUE)) %>%
  mutate(total_simplif = round(tot_mario_rubrica / 1000, 1))


# Criar o gráfico com facet_wrap
ggplot(tot_o_mario_rubrica_data, aes(x = data_03, y = tot_mario_rubrica)) +
  geom_col(fill = "green2", color = "black") + # Cor laranja Ubuntu
  geom_text(aes(label = total_simplif, fontface = "bold"), vjust = -0.3, size = 5.0) + # Adiciona os valores
  scale_y_continuous(limits = c(0, 18000), breaks = seq(0, 15000, by = 5000)) + # Definir o eixo Y
  facet_wrap(~rubrica, ncol = 3) + # Criar um gráfico para cada rubrica
  labs(
    title = "Mário: total de gastos por rubrica por mês (em milhares de R$).",
    x = NULL,
    y = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 15), # Tamanho do texto dos títulos dos facets
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.minor.y = element_blank()
  )
