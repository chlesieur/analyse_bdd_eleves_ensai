library(purrr)
library(dplyr)
library(stringr)
library(arrow)

# Répertoire où déposer les exports pamplemousse tels quels
repertoire <- "data/export pamplemousse"

# Lecture automatique des exports pamplemousse
fichiers_csv <- list.files(path = repertoire, pattern = "*.csv")

# tri les fichiers 
fichiers_csv_tries <- sort(fichiers_csv)

# Initialise un dataframe bdd
bdd <- data.frame()

for (fichier in fichiers_csv_tries) {
  
  data <- read.csv2(paste0(repertoire,"/",fichier), encoding = "latin1")
  
  # Vérifie si les colonnes sont les mêmes que celles de la bdd
  if (ncol(bdd) > 0 && !identical(names(data), names(bdd))) {
    stop(paste("Les colonnes du fichier", fichier, "ne correspondent pas aux autres fichiers."))
  }
  
  # Ajoute les données du fichier courant à la bdd
  bdd <- rbind(bdd, data)
}

# Vérification de doublons
any(duplicated(bdd))
bdd <- unique(bdd)
any(duplicated(bdd))

# Gestion des problèmes de formats (suppression des =)
noms_variables <- names(bdd)
nouveaux_noms <- gsub("^X.", "", noms_variables)
names(bdd) <- nouveaux_noms

supprimer_egal <- function(x) {
  if (is.character(x)) {
    gsub("^=", "", x)
  } else {
    x
  }
}

bdd <- as.data.frame(sapply(bdd, supprimer_egal))


# Seules les variables de note, coeff et rang sont numérisées

bdd$moyenne_ue <- gsub(",", ".", bdd$moyenne_ue)
bdd$moyenne_ue <- as.numeric(bdd$moyenne_ue)

bdd$moyenne_matiere <- gsub(",", ".", bdd$moyenne_matiere)
bdd$moyenne_matiere <- as.numeric(bdd$moyenne_matiere)

bdd$moyenne_generale <- gsub(",", ".", bdd$moyenne_generale)
bdd$moyenne_generale <- as.numeric(bdd$moyenne_generale)

bdd$total_coeff <- as.numeric(bdd$total_coeff)

bdd$total_ects <- as.numeric(bdd$total_ects)

bdd$rang_matiere <- gsub(",", ".", bdd$rang_matiere)
bdd$rang_matiere <- as.numeric(bdd$rang_matiere)

bdd$rang_max_matiere <- gsub(",", ".", bdd$rang_max_matiere)
bdd$rang_max_matiere <- as.numeric(bdd$rang_max_matiere)

bdd$toeic <- as.numeric(bdd$toeic)

bdd <- bdd %>% arrange(desc(annee_courante), voie_lib, nom) %>% 
  rename(annee=annee_courante)

# Création des variables de travail
# Choix à discuter

bdd_2 <- bdd %>%
  mutate(
    annee_scolaire = as.character(paste0(as.numeric(annee),"-",as.numeric(annee)+1)),
    annee_ecole = case_when(
      substr(voie_lib, 1, 2) == "1A" ~ "1A",
      substr(voie_lib, 1, 2) == "2A" ~ "2A",
      substr(voie_lib, 1, 2) == "3A" ~ "3A",
      TRUE ~ "Autres"
    ),
    statut_etudiant = case_when(
      grepl("ing", libelle_statut_etudiant, ignore.case = TRUE) == TRUE ~ "Ingénieur",
      grepl("att", libelle_statut_etudiant, ignore.case = TRUE) == TRUE ~ "Attaché",
      grepl("Mastère", libelle_statut_etudiant) == TRUE ~ "Mastère",
      grepl("Master", libelle_statut_etudiant) == TRUE ~ "Master",
      TRUE ~ "Autres"),
    voie_entree = case_when(
      grepl("Concours", concours_origine) == TRUE ~ "Concours-1A",
      grepl("admission en 1ère année", concours_origine) == TRUE ~ "AST-1A",
      grepl("admission en 2e année|STID", concours_origine) == TRUE ~ "AST-2A",
      grepl("MSc Big Data", concours_origine) == TRUE ~ "AST-2A",
      grepl("Erasmus", concours_origine) == TRUE ~ "Erasmus",
      TRUE ~ "Autres"),
    specialite_entree=case_when(
      grepl("mathématiques", concours_origine) == TRUE ~ "Mathématiques",
      grepl("économie et sciences sociales", concours_origine) == TRUE ~ "Économie",
      grepl("STID", concours_origine) == TRUE ~ "But-Stid",
      grepl("interne", concours_origine) == TRUE ~ "Interne",
      TRUE ~ "Autres"),
    filiere_1A = case_when(
      substr(voie_lib, 1, 2) == "1A" & grepl("Maths", voie_lib, ignore.case = TRUE) ~ "Maths",
      substr(voie_lib, 1, 2) == "1A" & grepl("Eco", voie_lib, ignore.case = TRUE) ~ "Eco",
      substr(voie_lib, 1, 2) == "1A" & grepl("Interne", voie_lib, ignore.case = TRUE) ~ "Interne",
      substr(voie_lib, 1, 2) == "1A" & grepl("Stid", voie_lib, ignore.case = TRUE) ~ "But",
      substr(voie_lib, 1, 2) == "1A" ~ "Autres", 
    ),
    filiere_2A = case_when(
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 1", voie_lib, ignore.case = TRUE) ~ "ATPA",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 2|Menu 3|DIGISPORT", voie_lib, ignore.case = TRUE) ~ "SB/GS/DIGISPORT",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 4", voie_lib, ignore.case = TRUE) ~ "ID/STD",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 5", voie_lib, ignore.case = TRUE) ~ "MES/EMOS",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 6", voie_lib, ignore.case = TRUE) ~ "MSP/ES",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 7", voie_lib, ignore.case = TRUE) ~ "GR",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 8", voie_lib, ignore.case = TRUE) ~ "MAR",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 9", voie_lib, ignore.case = TRUE) ~ "ERASMUS OUT",
      substr(voie_lib, 1, 2) == "2A" ~ "Autres"    
  ),
    filiere_3A = case_when(
      substr(voie_lib, 1, 2) == "3A" & grepl("Att,Master", voie_lib, ignore.case = TRUE) ~ "MSP",
      substr(voie_lib, 1, 2) == "3A" & grepl("GDRIF/GR", voie_lib, ignore.case = TRUE) ~ "GR",
      substr(voie_lib, 1, 2) == "3A" & grepl("GS", voie_lib, ignore.case = TRUE) ~ "GS",
      substr(voie_lib, 1, 2) == "3A" & grepl("MKT", voie_lib, ignore.case = TRUE) ~ "MKT",
      substr(voie_lib, 1, 2) == "3A" & grepl("ID", voie_lib, ignore.case = TRUE) ~ "ID",
      substr(voie_lib, 1, 2) == "3A" & grepl("ISTS", voie_lib, ignore.case = TRUE) ~ "ISTS",
      substr(voie_lib, 1, 2) == "3A" & grepl("MES", voie_lib, ignore.case = TRUE) ~ "MES",
      substr(voie_lib, 1, 2) == "3A" & grepl("M", voie_lib, ignore.case = TRUE) ~ "M",
      substr(voie_lib, 1, 2) == "3A" & grepl("SID", voie_lib, ignore.case = TRUE) ~ "SID",
      substr(voie_lib, 1, 2) == "3A" & grepl("SV", voie_lib, ignore.case = TRUE) ~ "SV",
      substr(voie_lib, 1, 2) == "3A" & grepl("ES", voie_lib, ignore.case = TRUE) ~ "ES",
      substr(voie_lib, 1, 2) == "3A" ~ "Autres"    
    ),
    sexe = case_when(
      libelle_etat_civil == "Monsieur" ~ "Homme",
      libelle_etat_civil == "Madame" ~ "Femme",
      TRUE ~ "Autres"
    ),
    nationalite = case_when(
      id_nationalite == 100 ~ "Français",
      TRUE ~"Étranger"
    ),
    cat_matiere = case_when(
      id_type_matiere == 1 ~"Informatique",
      id_type_matiere == 2 ~"Mathématiques/Statistiques",
      id_type_matiere == 3 ~"Économie",
      id_type_matiere == 4~ "Langues/Humanités",
      TRUE ~ "Autres"
    )
  )

# Récupération du travail de Stéphane sur les filières 3A

filieres_stephane_3A <- readxl::read_xlsx("data/bdd_2015_2024 explo.xlsx", sheet = "Filières") %>% 
  select(matiere, annee, filiere_3Abis)
filieres_stephane_3A$annee <- substr(filieres_stephane_3A$annee,1,4)
  
filieres_stephane_3A <- filieres_stephane_3A %>% 
  distinct(annee, matiere, .keep_all = TRUE)

bdd_3 <- left_join(bdd_2,filieres_stephane_3A, 
                   by = c("annee", "matiere"),
                   keep = FALSE)

table(bdd_3$filiere_3Abis)

# On conserve les variables utiles
bdd <- as.data.frame(sapply(bdd_3, supprimer_egal)) %>% 
  select(annee_scolaire, id_etudiant, nom, prenom, sexe, nationalite, id_nationalite, paysnai, etab_origine_formation, concours_origine, concours_annee,
         bac_annee, bac_mention, toeic,
         annee_ecole, statut_etudiant, voie_entree, specialite_entree, filiere_1A, filiere_2A, filiere_3A, filiere_3Abis,
         id_type_matiere, cat_matiere, code_matiere, matiere, ue, moyenne_matiere, moyenne_ue, moyenne_generale, total_coeff, total_ects, rang_matiere, rang_max_matiere, voie_lib)

# Labels pour création d'un dictionnaire
bdd$annee_scolaire <- structure(bdd$annee_scolaire, label = "Année de scolarité")
bdd$id_etudiant <- structure(bdd$id_etudiant, label = "Identifiant de l'étudiant")
bdd$nom <- structure(bdd$nom , label = "Nom de l'étudiant")
bdd$prenom <- structure(bdd$prenom, label = "Prénom de l'étudiant")
bdd$sexe <- structure(bdd$sexe, label = "Sexe de l'étudiant")
bdd$nationalite <- structure(bdd$nationalite, label = "Nationalité de l'étudiant")
bdd$id_nationalite <- structure(bdd$id_nationalite, label = "Identifiant de la nationalité de l'étudiant")
bdd$paysnai <- structure(bdd$paysnai, label = "Pays de naissance de l'étudiant")
bdd$etab_origine_formation <- structure(bdd$etab_origine_formation, label = "Type d'établissement d'origine de l'étudiant")
bdd$concours_origine <- structure(bdd$concours_origine, label = "Concours d'origine de l'étudiant")
bdd$concours_annee <- structure(bdd$concours_annee, label = "Année d'entrée à l'Ensai de l'étudiant")
bdd$bac_annee <- structure(bdd$bac_annee, label = "Année d'obtention du bac de l'étudiant")
bdd$bac_mention <- structure(bdd$bac_mention, label = "Mention au bac de l'étudiant")
bdd$toeic <- structure(bdd$toeic, label = "Score au Toeic ")
bdd$annee_ecole <- structure(bdd$annee_ecole, label = "Niveau/Année dans l'école (1A-2A-3A)")
bdd$statut_etudiant <- structure(bdd$statut_etudiant, label = "Statut de l'étudiant")
bdd$voie_entree <- structure(bdd$voie_entree, label = "Voie d'entrée à l'Ensai")
bdd$specialite_entree <- structure(bdd$specialite_entree, label = "Spécialité à l'entrée à l'Ensai")
bdd$filiere_1A <- structure(bdd$filiere_1A, label = "Filière en 1ère année à l'Ensai")
bdd$filiere_2A <- structure(bdd$filiere_2A, label = "Filière en 2ème année à l'Ensai")
bdd$filiere_3A <- structure(bdd$filiere_3A, label = "Filière en 3ème année à l'Ensai")
bdd$id_type_matiere <- structure(bdd$id_type_matiere, label = "Identifiant de la matière")
bdd$cat_matiere <- structure(bdd$cat_matiere, label = "Catégorie de la matière")
bdd$code_matiere <- structure(bdd$code_matiere, label = "Code de la matière")
bdd$matiere <- structure(bdd$matiere, label = "Nom de la matière")
bdd$ue <- structure(bdd$ue, label = "Nom de l'UE de la matière")
bdd$moyenne_matiere <- structure(bdd$moyenne_matiere, label = "Note moyenne de l'étudiant à la matière")
bdd$moyenne_ue <- structure(bdd$moyenne_ue, label = "Note moyenne de l'étudiant à l'a matière'UE")
bdd$voie_lib <- structure(bdd$voie_lib, label = "Libellé long de la voie d'appartenance")
bdd$rang_matiere <- structure(bdd$voie_lib, label = "Rang dans la matière")
bdd$rang_max_matiere <- structure(bdd$voie_lib, label = "Rang max dans la matière")
bdd$moyenne_generale <- structure(bdd$voie_lib, label = "Moyenne générale sur l'année")
bdd$total_coeff <- structure(bdd$voie_lib, label = "Coefficient de la matière")
bdd$total_ectcs <- structure(bdd$voie_lib, label = "Crédits ECTS de la matière")

# Export en cvs
write.csv2(bdd, "data/bdd_2015_2024.csv", row.names = FALSE)

# Export en parquet
write_parquet(bdd,"data/bdd_2015_2024.parquet")
