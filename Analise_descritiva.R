# Loading packages and functions -----------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
options(OutDec=",") # opção para salvar plots com "," como separador decimal

# Importing Data ---------------------------------------------------------------

# Filtering dataset
dados <- readr::read_csv("data_sets/Sleep_Efficiency.csv") |>
  dplyr::filter(!is.na(Awakenings) &
    !is.na(`Alcohol consumption`)) |>
  dplyr::mutate(
    # Dicotomizando as variáveis
    Gender = dplyr::case_when(
      Gender == "Female" ~ 1,
      TRUE ~ 0
    ),
    `Alcohol consumption` = dplyr::case_when(
      `Alcohol consumption` == 0 ~ 0,
      TRUE ~ 1
    ),
    `Exercise frequency` = dplyr::case_when(
      `Exercise frequency` == 0 ~ 0,
      TRUE ~ 1
    ),
    `Smoking status` = dplyr::case_when(
      `Smoking status` == "Yes" ~ 1,
      TRUE ~ 0
    ),
    # Transforming percentage to proportion
    `REM sleep percentage` = `REM sleep percentage` / 100,
    `Deep sleep percentage` = `Deep sleep percentage` / 100,
    `Light sleep percentage` = `Light sleep percentage` / 100
  ) |>
  # Renaming variables
  dplyr::rename(
    `REM sleep proportion` = `REM sleep percentage`,
    `Deep sleep proportion` = `Deep sleep percentage`,
    `Light sleep proportion` = `Light sleep percentage`
  ) |>
  # Removing NA's
  na.omit() |>
  # Removing useless variables
  dplyr::select(
    -c(1, 4, 5)
  )

# Métricas variáveis
summary(dados)

# Histograma Variável resposta - eficiência do sono
hist(dados$`Sleep efficiency`)

ggplot(dados, aes(x = `Sleep efficiency`)) +
  geom_histogram(color = "black", fill = "white", bins = 10) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),         # Remove grid
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Adiciona borda
    axis.line = element_blank()           # Remove linhas dos eixos para não sobrepor à borda
  ) +
  labs(
    x = "Eficiência do Sono",
    y = "Frequência",
  )

# função para salvar o plot em pdf
ggsave("histograma_sono.pdf", width = 6, height = 4, units = "in")


# Plot para as variáveis dicotômicas
df <- dados |>
  dplyr::select(
    `Alcohol consumption`, `Smoking status`
  ) |>
  mutate(
    `Alcohol consumption` = ifelse(`Alcohol consumption` == "1", "Sim", "Não"),
    `Smoking status` = ifelse(`Smoking status` == "1", "Sim", "Não")
  ) |>
  rename(
    `Consumiu Álcool` = `Alcohol consumption`,
    Fumante = `Smoking status`
  )

df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "resposta") %>%
  group_by(variavel, resposta) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variavel) %>%
  mutate(prop = n / sum(n))

ggplot(df_long, aes(x = variavel, y = prop, fill = resposta)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::number_format(decimal.mark = ",")) +
  labs(x = "", y = "Proporção", fill = "Resposta") +
  scale_fill_manual(values = c("salmon", "skyblue")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), # Remove grid
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Adiciona borda
    axis.line = element_blank() # Remove linhas dos eixos para não sobrepor à borda
  ) +
  labs(
    y = "Proporção",
  )

# função para salvar o plot em pdf
ggsave("barplot_CA_TAB.pdf", width = 6, height = 4, units = "in")
