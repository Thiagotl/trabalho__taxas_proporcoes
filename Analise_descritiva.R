# Loading packages and functions -----------------------------------------------
library(ggplot2)

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


summary(data_sleep[2])
sqrt(var(data_sleep[2]))
hist(data_sleep$Age, ylab = "Frequência", main = "Histograma das Idades", xlab = "Idade")
# Observa-se que a maior parte dos dados são de adultos com média de 40 anos idade,
# porém também contem crianças e idosos.

data_sleep$sexo_bin <- ifelse(data_sleep$Gender == "Female", 1, 0)
ggplot(data_sleep[3], aes(x = Gender)) +
  geom_bar() +
  labs(title = "Distribuição por Sexo", x = "Sexo", y = "Frequência") +
  theme_minimal()
# É a mesma quantidade, nem precisa de gráfico

summary(data_sleep[6]) # duração do sono
sqrt(var(data_sleep[6]))
hist(data_sleep$`Sleep duration`, ylab = "Frequência", main = "Histograma da duração do Sono", xlab = "Duração (h)", breaks = 5)
# A maioria dorme pelo menos 7 horas com uma média de 7.5 horas de sono, porém apenas 25% das pessoas
# atinge as 8 horas de sono recomendadas

summary(data_sleep[7])
sqrt(var(data_sleep[7]))
hist(data_sleep$`Sleep efficiency`, ylab = "Frequência", main = "Histograma da eficiência do Sono", xlab = "Eficiência do Sono (%)", breaks = 5)
# Todos tiveram pelo menos metade do sono aproveitado, em média tiverem 82% de eficiência no sono

summary(data_sleep[8])
sqrt(var(data_sleep[8]))
hist(data_sleep$`REM sleep percentage`, ylab = "Frequência", main = "Histograma do Pré-Sono (REM)", xlab = "Pré-Sono (%)", breaks = 3)
# Tempo de transição entre acordado e sono leve, a maioria das pessoas teve 22% do sono dessa forma.

summary(data_sleep[9])
sqrt(var(data_sleep[9]))
hist(data_sleep$`Deep sleep percentage`, ylab = "Frequência", main = "Histograma do Sono Profundo", xlab = "Sono Profundo (%)")
# Tempo de pesado, esse teve uma grande variação de 245! Acabou ficando divido ao meio, entre as pessoas
# que conseguiram um sono profundo (a maioria) e as que não conseguiram.

summary(data_sleep[10])
sqrt(var(data_sleep[10]))
hist(data_sleep$`Light sleep percentage`, ylab = "Frequência", main = "Histograma do Sono Leve", xlab = "Sono Leve (%)")
# Praticamente o inverso do sono pesado.

na11 <- na.omit(data_sleep[11])
summary(na11)
sqrt(var(na11))
ggplot(na11, aes(x = Awakenings)) +
  geom_bar() +
  labs(title = "Gráfico de barras de quantas vezes acordou durante a noite", x = "Quantidade de vezes que acordou", y = "Frequência") +
  theme_minimal()
# Em média as pessoas acordaram 1 ou 2 vezes na noite.

na12 <- na.omit(data_sleep[12])
summary(na12)
sqrt(var(na12))
hist(data_sleep$`Caffeine consumption`, ylab = "Frequência", main = "Histograma do consumo de cafeína", xlab = "Consumo de Cafeína (g)")
boxplot(data_sleep$`Caffeine consumption`, main = "Boxplot do Consumo de Cafeína", ylab = "Cafeína Ingerida (g)")
# O consumo de cafeína foi em média 25 por pessoa, e apesar de que há diferença significativa
# entre o consumo de cada um, a variância foi superestimada por um outlier que consumiu o DOBRO do segundo

na13 <- na.omit(data_sleep[13])
summary(na13)
sqrt(var(na13))
boxplot(data_sleep$`Alcohol consumption`, main = "Boxplot do Consumo de Álcool", ylab = "Álcool Ingerido (L)")
# Assim como o outro vício, há uma discrepância grande nos dados, onde metade não ingeriu nada,
# mas a média é de 1 por pessoas devido a outliers que consumiram muito alcool.

smoking <- data_sleep$`Smoking status`

data_sleep$cigas_bin <- ifelse(data_sleep$`Smoking status` == "Yes", 1, 0)
ggplot(data_sleep[14], aes(x = smoking)) +
  geom_bar() +
  labs(title = "Gráfico sobre o consumo de tabaco", x = "Consumo de Tabaco", y = "Frequência") +
  theme_minimal()
# Aproximadamente 66% das pessoas da pesquisa não consumem tabaco.

na14 <- na.omit(data_sleep[15])
summary(na14)
sqrt(var(na14))
ggplot(na11, aes(x = Awakenings)) +
  geom_bar() +
  labs(title = "Gráfico de barras da frequência de atividade física", x = "Quantidade de vezes que se exercita na semana", y = "Frequência") +
  theme_minimal()
# Em média as pessoas acordaram se exercitam duas vezes por semana, tendo uma variação grande entre
# os estudados, nesse caso um hábito saudável, mas assim como os vícios é difícil de balancear.
