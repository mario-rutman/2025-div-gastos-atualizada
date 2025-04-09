# 1. Fazendo o gráfico do gasto individual total por data. -------------------------

library(ggplot2)

# Calculando o total de gastos de o_mario por data.

tot_o_mario_data <- desp_mario_maria %>%
  group_by(data, tipo) %>%
  summarise(sub_tot = sum(reais, na.rm = TRUE)) %>%
  mutate(tot = ifelse(tipo == "comum", sub_tot / 2, sub_tot)) %>%
  group_by(data) %>%
  summarise(tot_mario = sum(tot, na.rm = TRUE))

# Fazendo o gráfico.

library(ggplot2)

# Supondo que seu dataframe 'tot_o_mario_data' já esteja carregado

# Criar o gráfico de colunas
ggplot(tot_o_mario_data, aes(x = data, y = tot_mario)) +
  geom_col(fill = "#E95420") + # cor laranja ubuntu
  labs(title = "Total de Gastos do Mário", x = NULL, y = NULL) +
  scale_x_datetime(date_labels = "%Y-%m", date_breaks = "1 month") + # Formatar eixo x
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(), # Remover linhas de grade verticais principais
    panel.grid.minor.x = element_blank(), # Remover linhas de grade verticais secundárias (se houver)
    panel.grid.major.y = element_line(color = "black"), # Definir cor das linhas de grade horizontais principais
    panel.grid.minor.y = element_line(color = "black"), # Remover linhas de grade horizontais secundárias (se houver)
  )

# Calculando agora os totais por mes por rubrica.

tot_o_mario_rubrica_data <- desp_mario_maria %>%
  group_by(data, tipo, rubrica) %>%
  summarise(sub_tot = sum(reais, na.rm = TRUE)) %>%
  mutate(tot = ifelse(tipo == "comum", sub_tot / 2, sub_tot)) %>%
  group_by(data, rubrica) %>%
  summarise(tot_mario_rubrica = sum(tot, na.rm = TRUE))


# Criar o gráfico com facet_wrap
ggplot(tot_o_mario_rubrica_data, aes(x = data, y = tot_mario_rubrica)) +
  geom_col(fill = "green2") + # Cor laranja Ubuntu
  facet_wrap(~rubrica, ncol = 3) + # Criar um gráfico para cada rubrica
  labs(
    title = "Total de Gastos de O Mário por Rubrica e Data",
    x = NULL,
    y = NULL
  ) +
  scale_x_datetime(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 15), # Tamanho do texto dos títulos dos facets
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.minor.y = element_blank()
  )

