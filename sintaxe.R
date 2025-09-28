library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

dados <- read.csv("Tour_Winners_data_1.csv", sep = ",", header = TRUE)

# ========================================
# Como a idade influencia nas vitórias
# ========================================
dados |>
  group_by(age) |>
  summarise(vitorias = n(), .groups = "drop") |>
  arrange(age) |>
  ggplot(aes(x = age, y = vitorias)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Influência da idade nas vitórias",
    x = "Idade",
    y = "Número de vitórias"
  ) +
  theme_minimal()


# ========================================
# vitorias por time
# ========================================
dados |>
  group_by(Team) |>
  summarise(vitorias = n(), .groups = "drop") |>
  ggplot(aes(x = reorder(Team, vitorias), y = vitorias, fill = Team)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Vitórias por time",
    x = "Time",
    y = "Número de vitórias"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ========================================
# Criando dataset com faixas de 30 em 30 anos
# ========================================

library(dplyr)

# ========================================
# Criando dataset com faixas de 30 em 30 anos
# ========================================

# 1) Carregar dados
df <- read_csv("Tour_Winners_data_1.csv")

# 2) Garantir que colunas numéricas sejam numéricas
# Remove caracteres não numéricos, troca vírgula por ponto e converte
df <- df %>%
  mutate(across(
    contains("km"),
    ~ as.numeric(gsub(",", ".", gsub("[^0-9,\\.]", "", .x)))
  ))

# 3) Definir faixas fixas de 30 em 30 anos, terminando em 2023
breaks <- c(seq(1903, 1993, by = 30), 2023)

df <- df %>%
  mutate(
    Year_Group = cut(
      Year,
      breaks = breaks,
      right = FALSE, 
      include.lowest = TRUE,
      labels = paste0(breaks[-length(breaks)], " - ", breaks[-1])
    )
  )

# 4) Função de moda (NA em caso de empate)
moda_ou_na <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  tab <- table(x)
  top <- names(tab[tab == max(tab)])
  if (length(top) == 1) return(top) else return(NA)
}

# 5) Identificar colunas quantitativas e qualitativas
quant_cols <- df %>%
  select(where(is.numeric)) %>%
  select(-Year, -Tour_No) %>%
  names()

qual_cols <- df %>%
  select(where(\(x) is.character(x) || is.factor(x))) %>%
  select(-Year_Group) %>%
  names()

# 6) Resumir: médias para numéricas, moda para qualitativas
dataset_faixas <- df %>%
  group_by(Year_Group) %>%
  summarise(
    across(all_of(quant_cols), ~ mean(.x, na.rm = TRUE)),
    across(all_of(qual_cols), moda_ou_na),
    .groups = "drop"
  )

# 7) Ver resultado
print(dataset_faixas)

# ========================================
# Análise de BMI pela faixa de anos
# ========================================

ggplot(dataset_faixas, aes(x = Year_Group, y = BMI, group = 1)) +
  geom_col(fill = "steelblue") +
  geom_line(color = "red", linewidth = 1.2) +
  labs(
    title = "Média de BMI por Faixa de Ano com Linha de Tendência (sem regressão)",
    x = "Faixa de Ano",
    y = "Média de BMI"
  ) +
  theme_minimal()

# ========================================
# Análise de BMI por função do ciclista
# ========================================

#Variável rider_type estava retornando múltiplos factors para o mesmo tipo
#Tratando essa variável

dados <- dados %>%
  mutate(rider_type_.PPS. = as.character(rider_type_.PPS.),      
         rider_type_.PPS. = str_trim(str_to_lower(rider_type_.PPS.)),
         rider_type_.PPS. = factor(rider_type_.PPS.))    


ggplot(dados, aes(x = rider_type_.PPS., y = BMI)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(
    title = "Distribuição do BMI por Tipo de Ciclista",
    x = "Tipo de Ciclista",
    y = "BMI"
  ) +
  theme_minimal()

# ========================================
# Tipo de ciclista por faixa de ano
# ========================================

# Criar dataset com a coluna de faixas de anos
dados <- dados %>%
  mutate(Year_Group = case_when(
    Year >= 1903 & Year <= 1933 ~ "1903-1933",
    Year >= 1934 & Year <= 1963 ~ "1934-1963",
    Year >= 1964 & Year <= 1993 ~ "1964-1993",
    Year >= 1994 & Year <= 2023 ~ "1994-2023"
  ))

# Contar vencedores por rider_type e faixa de ano
contagem <- dados %>%
  group_by(Year_Group, rider_type_.PPS.) %>%
  summarise(n = n(), .groups = "drop")

# Gráfico de barras
ggplot(contagem, aes(x = Year_Group, y = n, fill = rider_type_.PPS.)) +
  geom_col(position = "dodge") +
  labs(
    title = "Número de Vencedores por Tipo de Ciclista e Faixa de Ano",
    x = "Faixa de Ano",
    y = "Número de Vencedores",
    fill = "Tipo de Ciclista"
  ) +
  theme_minimal()

# ========================================
# Relação idade do ciclista e faixa de ano
# ========================================

ggplot(dados, aes(x = factor(Year_Group), y = age)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(
    title = "Distribuição da Idade por Tipo de Ciclista",
    x = "Tipo de Ciclista",
    y = "Idade"
  ) +
  theme_minimal()

# ========================================
# 3 Países com mais vitórias por faixa de ano
# ========================================

top_countries <- dados %>%
  group_by(Year_Group, Country) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year_Group) %>%
  slice_max(order_by = n, n = 3)  # pega os top 3


ggplot(top_countries, aes(x = Year_Group, y = n, fill = Country)) +
  geom_col(position = "dodge") +
  labs(
    title = "Top 3 Países por Faixa de Ano",
    x = "Faixa de Ano",
    y = "Número de Vitórias",
    fill = "País"
  ) +
  theme_minimal()

# ========================================
# Quilometragem do Tour por faixa de ano
# ========================================

ggplot(dataset_faixas, aes(x=Year_Group, y= `Tour_overall_length_(km)`)) +
  geom_col(position = "dodge", fill = "blue")
