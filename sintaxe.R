library(dplyr)
library(ggplot2)

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



