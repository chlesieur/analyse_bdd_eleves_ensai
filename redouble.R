# Création d'une variable redoublement/exclusion

rm(list = ls())

library(dplyr)
library(arrow)
library(knitr)
library(ggplot2)
library(forcats)
library(kableExtra)
library(scales)
library(tidyr)

data <- read_parquet('data/bdd_2015_2025.parquet')

data <- data %>% 
  filter(!id_etudiant %in% c("11643","12316")) %>% # Leur année d'admission est différente dans le doute, on enlève 
  select(-c(nom, prenom))

data <- data %>% 
  filter(statut_etudiant %in% c("Ingénieur","Attaché"))

table(data$spe_entree)

data_t <- unique(data %>% 
                   select(id_etudiant,statut_etudiant,annee,annee_ecole,spe_entree) %>% 
                   filter(!(annee == 2015 & annee_ecole == "3A"),
                          !(annee == 2025 & spe_entree == "AST-2A"),
                          !(annee == 2025 & annee_ecole == "1A"))
                 )

id_dupli <- unique(data_t$id_etudiant[duplicated(data_t$id_etudiant)])
liste_id <- unique(data_t$id_etudiant)
exclu_1A <- setdiff(liste_id,id_dupli)

data_exclu_1A <- data_t %>% 
  filter(id_etudiant %in% exclu_1A,annee_ecole == "1A") %>% 
  select(id_etudiant,annee) %>% 
  rename("annee_exclusion" = annee)

data_exclu_1A

#---------- Redoublement

data_1A <- unique(data %>% 
  filter(annee_ecole == "1A") %>% 
  select(id_etudiant, annee))

redoublant_1A <- data_1A$id_etudiant[duplicated(data_1A$id_etudiant)]

data_redoubl_1A <- unique(data_1A %>% 
  filter(id_etudiant %in% redoublant_1A) %>% 
  group_by(id_etudiant) %>% 
  mutate(annee_redoublement = min(annee)) %>% 
  ungroup() %>% 
  select(id_etudiant,annee_redoublement)) 


#----- Graphique

data_l <- unique(data %>% 
  select(annee,id_etudiant,annee_ecole,statut_etudiant, bloc_an, spe_entree))


data_l <- data_l %>% 
  left_join(data_redoubl_1A %>% mutate(annee = annee_redoublement, redoublement_flag = 1),
            by = c("id_etudiant","annee")) %>% 
  mutate(redoublement = coalesce(redoublement_flag,0)) %>% 
  select(-redoublement_flag)

data_l_2020 <- data_l %>% 
  filter(annee =="2020",
         annee_ecole=="1A",
         statut_etudiant == "Attaché")

data_l <- data_l %>% 
  left_join(data_exclu_1A %>% mutate(annee = annee_exclusion, exclusion_flag = 1),
            by = c("id_etudiant","annee")) %>% 
  mutate(exclusion = coalesce(exclusion_flag,0)) %>% 
  select(-exclusion_flag)

data_l <- data_l %>% 
  mutate(sanction = exclusion + redoublement,
         annee_sanction = max(annee_redoublement,annee_exclusion))

data_l %>% 
  filter(annee != 2025,
         annee_ecole == "1A") %>% 
  group_by(annee,statut_etudiant) %>% 
  summarise(taux_sanction = mean(sanction)*100, .groups = "drop") %>% 
  ggplot(aes(x=annee,y=taux_sanction,color=statut_etudiant,group=statut_etudiant)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Taux de redoublement/exclusion entre 2015 et 2025",
    x = "Année",
    y = "Taux",
    color = "Ingénieur/Attaché"
  ) 

data_l %>% 
  filter(annee != 2025,
         statut_etudiant == "Ingénieur",
         annee_ecole == "1A",
         spe_entree != "AST-1A") %>% 
  group_by(bloc_an,spe_entree) %>% 
  summarise(taux_sanction = mean(sanction)*100, .groups = "drop") %>% 
  ggplot(aes(x=spe_entree,y=taux_sanction,fill = spe_entree)) +
  geom_col() +
  facet_wrap(~ bloc_an) +
  labs(
    title = "Échec en 1A (redoublement + exclusion/démission, logique cohorte)",
    x = "Spécialité d'entrée",
    y = "Part"
  ) +
  theme_minimal() +
  theme(legend.position = "none")




ggplot(data_l, aes(
  x = spe_entree,
  y = part,
  fill = spe_entree
)) +
  geom_col() +
  facet_wrap(~ bloc_an) +
  labs(
    title = "Échec en 1A (redoublement + exclusion/démission, logique cohorte)",
    x = "Spécialité d'entrée",
    y = "Part"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

