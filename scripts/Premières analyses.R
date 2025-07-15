library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(arrow)

bdd <- read_parquet("data/bdd_2015_2024.parquet")

# 1. Dénombrement du nombre d'élèves par voie d'admission

nb_eleve_voie_admission <- bdd %>%
  group_by(annee_scolaire,concours_origine) %>% 
  summarise(nb_eleve = n())


filiere_stats <- bdd %>%
  select(id_etudiant, annee_scolaire, filiere_1A, filiere_2A, filiere_3Abis) %>%
  pivot_longer(cols = starts_with("filiere_"), names_to = "niveau", values_to = "filiere") %>%
  filter(!is.na(filiere) & filiere != "") %>%
  group_by(id_etudiant, annee_scolaire, niveau, filiere) %>%
  summarise(nb = n(), .groups = "drop")
  


