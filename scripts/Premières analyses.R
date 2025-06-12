# Chargement des bibliothèques
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)

df <- read_delim("data/bdd_2015_2024.csv", delim = ";", locale = locale(encoding = "ISO-8859-1"))

df <- df %>%
  mutate(
    moyenne_generale = as.numeric(moyenne_generale),
    toeic = as.numeric(toeic)
  )

# 1. Comptage des filières 1A, 2A, 3A par année scolaire
filiere_stats <- df %>%
  select(annee_scolaire, filiere_1A, filiere_2A, filiere_3A) %>%
  pivot_longer(cols = starts_with("filiere_"), names_to = "niveau", values_to = "filiere") %>%
  filter(!is.na(filiere) & filiere != "") %>%
  group_by(annee_scolaire, niveau, filiere) %>%
  summarise(nb = n(), .groups = "drop")


filiere_stats <- filiere_stats %>%
  group_by(filiere) %>%
  mutate(total = sum(nb)) %>%
  ungroup() %>%
  mutate(filiere = fct_lump_n(filiere, n = 10))  # Garder les 10 plus fréquentes

ggplot(filiere_stats, aes(x = annee_scolaire, y = nb, fill = filiere)) +
  geom_col(position = "stack") +
  facet_wrap(~niveau, scales = "free_y") +
  scale_fill_brewer(palette = "Set3") +  # Palette douce
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  labs(
    title = "Évolution des filières par niveau et par année",
    x = "Année scolaire",
    y = "Nombre d'étudiants"
  )
# 2. Spécialisation en 2A selon la moyenne

