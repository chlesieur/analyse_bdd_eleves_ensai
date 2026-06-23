library(purrr)
library(dplyr)
library(stringr)
library(arrow)
library(writexl)
library(labelled)
library(sjlabelled)
library(stringi)
library(readr)
library(readxl)
library(janitor)
library(writexl)

### 1. Lecture des fichiers d'input

### 1.1. Lecture des fichiers de la requete principale 

# Il s'agit d'utiliser les requêtes suivantes et les déposer dans des dossier "requete principale" et "requete points bonus et jury":
# Scola - Extraction notes - Requête principale - Pour base élèves
# Scola - Extraction notes - Rangs et points de bonus et de jury - Pour base élèves
repertoire <- "C:/Users/clesieur/Documents/Base_eleve/analyse_bdd_eleves_ensai/data"

# Lecture automatique des exports pamplemousse
fichiers_csv <- list.files(path = paste0(repertoire,"/export pamplemousse/requete principale/"), pattern = "\\.csv$")

# tri les fichiers 
fichiers_csv_tries <- sort(fichiers_csv)

# Concaténation verticale des fichiers de chaque année de la requête principale
bdd <- data.frame()
for (fichier in fichiers_csv_tries) {
  data <- read.csv2(paste0(repertoire,"/export pamplemousse/requete principale/",fichier), encoding = "latin1")
  if (ncol(bdd) > 0 && !identical(names(data), names(bdd))) {
    stop(paste("Les colonnes du fichier", fichier, "ne correspondent pas aux autres fichiers."))
  }
  bdd <- rbind(bdd, data)
}

# Vérification de doublons
bdd_sans_doublon <- bdd %>%
  distinct(X.annee_courante, X.id_etudiant, X.code_matiere, .keep_all = TRUE) %>% 
  filter(X.moyenne_generale != "=")

### 1.2. Lecture des fichiers de la requete secondaire sur les points de bonus et de jury

# Lecture automatique des exports pamplemousse
fichiers_csv <- list.files(path = paste0(repertoire,"/export pamplemousse/requete points bonus et jury"), pattern = "*.csv")

# tri les fichiers 
fichiers_csv_tries <- sort(fichiers_csv)

# Concaténation verticale des fichiers de chaque année de la requête principale
bdd_points <- data.frame()
for (fichier in fichiers_csv_tries) {
  data <- read.csv2(paste0(repertoire,"/export pamplemousse/requete points bonus et jury/",fichier), encoding = "latin1")
  if (ncol(bdd_points) > 0 && !identical(names(data), names(bdd_points))) {
    stop(paste("Les colonnes du fichier", fichier, "ne correspondent pas aux autres fichiers."))
  }
  bdd_points <- rbind(bdd_points, data)
}

bdd_points_1 <- bdd_points %>% 
  select(-c("X.ue","X.matiere","X","X.id_type_matiere","X.moyenne_matiere"))

# Elimination des doublons
bdd_points_2 <- bdd_points_1 %>%
  distinct(X.annee_courante, X.id_etudiant, X.code_matiere, .keep_all = TRUE)

### 1.3.Lecture des fichiers d'admission au concours maths

integrants_maths_att <- "data/admissions/integrants_maths_attachés_2015_2025.xlsx"
integrants_maths_ing <- "data/admissions/integrants_maths_ingénieurs_2015_2025.xlsx"

onglets_math_att <- excel_sheets(integrants_maths_att)

integrants_maths_att_2015_2025 <- map_dfr(
  onglets_math_att,
  ~ read_excel(integrants_maths_att, sheet = .x) %>%
    clean_names() %>%   # met les noms en snake_case
    select(nom, prenom, ccc_ran_com) 
)

onglets_math_ing <- excel_sheets(integrants_maths_ing)

integrants_maths_ing_2015_2025 <- map_dfr(
  onglets_math_ing,
  ~ read_excel(integrants_maths_ing, sheet = .x) %>%
    clean_names() %>%   # met les noms en snake_case
    select(nom, prenom, ccc_ran_com)
)

integrants_maths_2015_2025 <- bind_rows(integrants_maths_att_2015_2025,integrants_maths_ing_2015_2025) %>% 
  rename(rang_cc_maths = ccc_ran_com)

### 1.4.Lecture des fichiers d'admission au concours BL

integrants_BL_att <- "data/admissions/integrants_BL_attachés_2015_2025.xlsx"
integrants_BL_ing <- "data/admissions/integrants_BL_ingénieurs_2015_2025.xlsx"

onglets_BL_att <- excel_sheets(integrants_BL_att)

integrants_BL_att_2015_2025 <- map_dfr(
  onglets_BL_att,
  ~ read_excel(integrants_BL_att, sheet = .x) %>%
    clean_names() %>%
    select(any_of(c("nom", "prenom", "moyenne", "rang", "rang_cc"))) %>%
    mutate(
      moyenne = as.numeric(moyenne),
      rang = as.numeric(rang),
      rang_cc = as.numeric(rang_cc)
    )
) %>%
  filter(!if_all(everything(), is.na))


onglets_BL_ing <- excel_sheets(integrants_BL_ing)

integrants_BL_ing_2015_2025 <- map_dfr(
  onglets_BL_ing,
  ~ read_excel(integrants_BL_ing, sheet = .x) %>%
    clean_names() %>%
    select(any_of(c("nom", "prenom", "moyenne", "rang", "rang_cc"))) %>%
    mutate(
      moyenne = as.numeric(moyenne),
      rang = as.numeric(rang),
      rang_cc = as.numeric(rang_cc)
    )
) %>%
  filter(!if_all(everything(), is.na))

integrants_BL_2015_2025 <- bind_rows(integrants_BL_att_2015_2025,integrants_BL_ing_2015_2025) %>% 
  rename(rang_cc_bl = rang_cc) %>% 
  select(-rang)

### 1.5.Lecture des fichiers d'admission au concours BL

integrants_D2_att <- "data/admissions/integrants_D2_attachés_2015_2025.xlsx"
integrants_D2_ing <- "data/admissions/integrants_D2_ingénieurs_2015_2025.xlsx"

onglets_D2_att <- excel_sheets(integrants_D2_att)

integrants_D2_att_2015_2025 <- map_dfr(
  onglets_D2_att,
  ~ read_excel(integrants_D2_att, sheet = .x) %>%
    clean_names() %>%
    select(any_of(c("nom", "prenom", "moyenne", "rang", "rang_cc"))) %>%
    mutate(
      moyenne = as.numeric(moyenne),
      rang = as.numeric(rang),
      rang_cc = as.numeric(rang_cc)
    )
) %>%
  filter(!if_all(c(rang, rang_cc), is.na))


onglets_D2_ing <- excel_sheets(integrants_D2_ing)

integrants_D2_ing_2015_2025 <- map_dfr(
  onglets_D2_ing,
  ~ read_excel(integrants_D2_ing, sheet = .x) %>%
    clean_names() %>%
    select(any_of(c("nom", "prenom", "moyenne", "rang", "rang_cc"))) %>%
    mutate(
      moyenne = as.numeric(moyenne),
      rang = as.numeric(rang),
      rang_cc = as.numeric(rang_cc)
    )
) %>%
  filter(!if_all(c(moyenne, rang), is.na))

integrants_D2_2015_2025 <- bind_rows(integrants_D2_att_2015_2025,integrants_D2_ing_2015_2025) %>% 
  rename(rang_cc_d2 = rang_cc) %>% 
  select(-rang)

### 2. fusion des bases

bdd_2 <- left_join(bdd_sans_doublon, bdd_points_2, 
                 by = c("X.annee_courante",
                        "X.id_etudiant",
                        "X.code_matiere"))

bdd <- unique(bdd_2)

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

bdd <- as.data.frame(sapply(bdd, supprimer_egal)) %>% 
  rename(annee = annee_courante)

# fonction pour épurer les nom prenoms en vue de l'appariemment
clean_text <- function(x, first_only = TRUE) {
  x <- ifelse(is.na(x), "", x)                          # NA -> ""
  x <- stringr::str_to_lower(x)                         # minuscules
  x <- stringr::str_trim(x)                             # trim
  x <- stringi::stri_trans_general(x, "Latin-ASCII")    # é/è/É→e, ç→c, œ→oe
  
  # on garde provisoirement les espaces pour pouvoir prendre le 1er "mot"
  x <- stringr::str_replace_all(x, "[^a-z0-9 ]", " ")
  x <- stringr::str_squish(x)
  
  if (isTRUE(first_only)) {
    x <- sub("\\s.*$", "", x)                           # ne garder que avant le 1er espace
  }
  
  # nettoyage final : plus que lettres/chiffres
  x <- gsub("[^a-z0-9]", "", x)
  x
}

bdd <- bdd %>% mutate(id = paste0(clean_text(nom, TRUE), clean_text(prenom, TRUE)))


integrants_maths_2015_2025 <- integrants_maths_2015_2025 %>%
  mutate(id = paste0(clean_text(nom, TRUE), clean_text(prenom, TRUE)))

integrants_BL_2015_2025 <- integrants_BL_2015_2025 %>%
  mutate(id = paste0(clean_text(nom, TRUE), clean_text(prenom, TRUE)))

integrants_D2_2015_2025 <- integrants_D2_2015_2025 %>%
  mutate(id = paste0(clean_text(nom, TRUE), clean_text(prenom, TRUE)))


bdd_x <- bdd %>%
  left_join(integrants_maths_2015_2025, by = "id") %>%
  left_join(integrants_BL_2015_2025, by = "id") %>%
  left_join(integrants_D2_2015_2025, by = "id")

# Etudes des quelques cas non appariés
non_appariees <- integrants_maths_2015_2025 %>%
  anti_join(bdd, by = "id") %>% 
  filter(!is.na(nom))

bdd_x_unique <- bdd_x %>%
  distinct(nom.x, prenom.x, .keep_all = TRUE)

# Nombre de lignes appariées
nb_match <- sum(!is.na(bdd_x_unique$nom.y))

# Taux d’appariement (%)
taux_appariement <- nb_match / nrow(integrants_maths_2015_2025) * 100
taux_appariement

# Ajout des non appariés à la main
bdd_y <- bdd_x %>%
  mutate(rang_cc_maths = case_when(
    id == "diopn" ~ 2393L,
    id == "seghaieraziz" ~ 2548L,
    id == "mahjoubibeyrem" ~ 2541L,
    id == "blaiechamine" ~ 2074L,
    TRUE ~ rang_cc_maths))

# Restait une seule interrogation : Laurie BANOS a-t-elle changé de nom de famille
test2 <- bdd_y %>%  filter(id == "pinellaurie")
# ça ne semble pas être Laurie PINEL et l'autre Laurie 'LETERRIER' a une affectation donc a priori non


# Etudes des quelques cas non appariés BL
non_appariees_BL <- integrants_BL_2015_2025 %>%
  anti_join(bdd, by = "id") %>% 
  filter(!is.na(nom))

# Etudes des quelques cas non appariés D2
non_appariees_D2 <- integrants_D2_2015_2025 %>%
  anti_join(bdd, by = "id") %>% 
  filter(!is.na(nom))


################# Création des variables de travail ########################"""

print(names(bdd_y))

bdd_2 <- bdd_y %>%
  filter(grepl("SORT", voie_lib, ignore.case = TRUE) != TRUE) %>% 
  rename(nom = nom.x, prenom = prenom.x) %>%  # Renommage des colonnes
  mutate(
    annee_scolaire = as.character(paste0(as.numeric(annee),"-",as.numeric(annee)+1)),
    annee_ecole = case_when(
      substr(voie_lib, 1, 2) == "1A" ~ "1A",
      substr(voie_lib, 1, 2) == "2A" ~ "2A",
      substr(voie_lib, 1, 2) == "3A" ~ "3A",
      grepl("Mast|SDDP", voie_lib, ignore.case = TRUE) == TRUE ~ "3A",
      TRUE ~ "Autres"
    ),
    statut_etudiant = case_when(
      grepl("ing", libelle_statut_etudiant, ignore.case = TRUE) == TRUE ~ "Ingénieur",
      grepl("att|Master ES|Master MSP|Master STD", libelle_statut_etudiant, ignore.case = TRUE) == TRUE ~ "Attaché",
      grepl("MSD", libelle_statut_etudiant) == TRUE ~ "MSD",
      grepl("MSSD", libelle_statut_etudiant) == TRUE ~ "MS",
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
      TRUE ~ NA_character_
    ),
    filiere_2A = case_when(
      substr(voie_lib, 1, 2) == "2A" & grepl("ATT", voie_lib, ignore.case = TRUE) ~ "ATT",     
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 1|ATPA", voie_lib, ignore.case = TRUE) ~ "ATPA",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 2|Menu 3|SB|GS|GSS", voie_lib, ignore.case = TRUE) ~ "SB/GS",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 4|ID|STD", voie_lib, ignore.case = TRUE) ~ "ID/STD",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 5|MES|EMOS", voie_lib, ignore.case = TRUE) ~ "MES/EMOS",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 6|MSP|ES", voie_lib, ignore.case = TRUE) ~ "MSP/ES",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 7|GR", voie_lib, ignore.case = TRUE) ~ "GR",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 8|MAR", voie_lib, ignore.case = TRUE) ~ "MAR",
      substr(voie_lib, 1, 2) == "2A" & grepl("Menu 9|SCOL EXT|ERASMUS OUT", voie_lib, ignore.case = TRUE) ~ "SCOL EXT",
      substr(voie_lib, 1, 2) == "2A" & grepl("DIGISPORT", voie_lib, ignore.case = TRUE) ~ "DIGISPORT",
      substr(voie_lib, 1, 2) == "2A" ~ "Autres",
      TRUE ~ NA_character_
    ),
    filiere_3A = case_when(
      substr(voie_lib, 1, 2) == "3A" & grepl("MES|ISTS", voie_lib, ignore.case = TRUE) ~ "MES",
      substr(voie_lib, 1, 2) == "3A" & grepl("MSP", voie_lib, ignore.case = TRUE) ~ "MSP",
      substr(voie_lib, 1, 2) == "3A" & grepl("ES", voie_lib, ignore.case = TRUE) ~ "ES",
      substr(voie_lib, 1, 2) == "3A" & grepl("STD", voie_lib, ignore.case = TRUE) ~ "STD",     
      substr(voie_lib, 1, 2) == "3A" & grepl("EMOS", voie_lib, ignore.case = TRUE) ~ "EMOS",
      substr(voie_lib, 1, 2) == "3A" & grepl("GDRIF|GDRIF/GR|GR|3A GR", voie_lib, ignore.case = TRUE) ~ "GR",
      substr(voie_lib, 1, 2) == "3A" & grepl("GS", voie_lib, ignore.case = TRUE) ~ "GS",
      substr(voie_lib, 1, 2) == "3A" & grepl("MKT|3A M", voie_lib, ignore.case = TRUE) ~ "MKT",
      substr(voie_lib, 1, 2) == "3A" & grepl("ID", voie_lib, ignore.case = TRUE) ~ "ID",
      substr(voie_lib, 1, 2) == "3A" & grepl("SBIO|SV|3A SBIO", voie_lib, ignore.case = TRUE) ~ "SBIO",
      substr(voie_lib, 1, 2) == "3A" & grepl("DIGISPORT", voie_lib, ignore.case = TRUE) ~ "DIGISPORT",
      substr(voie_lib, 1, 2) == "3A" & grepl("ATPA", voie_lib, ignore.case = TRUE) ~ "ATPA",
      substr(voie_lib, 1, 2) == "3A" & grepl("SCOL EXT", voie_lib, ignore.case = TRUE) ~ "SCOL EXT",
      substr(voie_lib, 1, 2) == "3A" ~ "Autres",
      TRUE ~ NA_character_
    ),
    sexe = case_when(
      libelle_etat_civil == "Monsieur" ~ "Homme",
      libelle_etat_civil == "Madame" ~ "Femme",
      TRUE ~ "Autres"
    ),
    nationalite = case_when(
      id_nationalite == 100 ~ "Français",
      id_nationalite != 100 & substr(id_nationalite, 1, 1) == "1" ~ "UE",
      TRUE ~ "Étranger hors UE"
    ),
    cat_matiere = case_when(
      id_type_matiere == 1 ~"Informatique",
      id_type_matiere == 2 ~"Mathématiques/Statistiques",
      id_type_matiere == 3 ~"Économie",
      id_type_matiere == 4~ "Langues/Humanités",
      TRUE ~ "Autres"
    )
  ) %>% 
  # Suppression des colonnes indésirables
  select(-any_of(c("nom.y", "prenom.y", "id","nom.x.x","prenom.x.x","moyenne.x","nom.y.y","prenom.y.y","moyenne.y")))

table(bdd_2$filiere_2A)

autres_restants <- bdd_2 %>% 
  filter(filiere_2A == "AUTRES") %>%
  select(voie_lib, annee) %>% 
  distinct

table(bdd_2$annee_ecole)


# Récupération du travail de Stéphane sur les filières 3A
filieres_stephane_3A <- readxl::read_xlsx("data/modifs stephane/bdd_2015_2024 explo.xlsx", sheet = "Filières") %>% 
  select(matiere, annee, filiere_3Abis)
filieres_stephane_3A$annee <- substr(filieres_stephane_3A$annee,1,4)
  
filieres_stephane_3A <- filieres_stephane_3A %>% 
  distinct(annee, matiere, .keep_all = TRUE)

bdd_3 <- left_join(bdd_2,filieres_stephane_3A, 
                   by = c("annee", "matiere"),
                   keep = FALSE)


table(bdd_3$annee_ecole)

# Seules les variables de note, bonus, coeff et rang sont numérisées

# bdd_3$moyenne_ue <- gsub(",", ".", bdd_3$moyenne_ue)
# bdd_3$moyenne_ue <- as.numeric(bdd_3$moyenne_ue)
# 
# bdd_3$moyenne_matiere <- gsub(",", ".", bdd_3$moyenne_matiere)
# bdd_3$moyenne_matiere <- as.numeric(bdd_3$moyenne_matiere)
# 
# bdd_3$moyenne_generale <- gsub(",", ".", bdd_3$moyenne_generale)
# bdd_3$moyenne_generale <- as.numeric(bdd_3$moyenne_generale)
# 
# bdd_3$total_coeff <- as.numeric(bdd_3$total_coeff)
# 
# bdd_3$total_ects <- as.numeric(bdd_3$total_ects)
# 
# bdd_3$rang_matiere <- gsub(",", ".", bdd_3$rang_matiere)
# bdd_3$rang_matiere <- as.numeric(bdd_3$rang_matiere)
# 
# bdd_3$rang_max_matiere <- gsub(",", ".", bdd_3$rang_max_matiere)
# bdd_3$rang_max_matiere <- as.numeric(bdd_3$rang_max_matiere)
# 
# bdd_3$toeic <- as.numeric(bdd_3$toeic)

# bdd_3$point_bonus <- gsub(",", ".", bdd_3$point_bonus)
# bdd_3$point_jury <- as.numeric(bdd_3$point_jury)


# Numérisation des variables de note, coeff, et rang, moyennes

## patch pour rang et rang max

# bdd_rang <- read_parquet("Z:/0_Direction_des_Etudes/Base_eleve/data/20250618/bdd_2015_2025.parquet")
# 
# bdd_rang_1 <- bdd_rang %>% 
#   select(annee,id_etudiant,code_matiere,rang_matiere, rang_max_matiere, point_jury)


bdd_3$point_jury <- ifelse(grepl("points de jury", bdd_3$matiere, ignore.case = TRUE),
                           bdd_3$point_jury, 
                           NA)

## Numérisation de certaines variables
bdd_3$moyenne_matiere <- gsub("\\.", ",", bdd_3$moyenne_matiere)

numeriser <- function(data, vars, decimal_mark = ",") {
  data %>% mutate(across(
    all_of(vars),
    ~ {                          
      lbl  <- attr(.x, "label")  
      out  <- parse_number(     
        as.character(.x),
        locale = locale(decimal_mark = decimal_mark)
      )
      if (!is.null(lbl))         
        attr(out, "label") <- lbl
      out                         
    }
  ))
}


bdd_4 <- numeriser(
  bdd_3,
  c("rang_matiere",
  "rang_max_matiere",
  "moyenne_matiere",
  "moyenne_generale",
  "total_coeff",
  "total_ects",
  "moyenne_generale",
  "moyenne_ue",
  "toeic",
  "MES1", "MHS1", "MIS1", "MSS1",
  "MES2", "MHS2", "MIS2", "MSS2",
  "MGS1", "MGS2","rang_cc_maths","rang_cc_bl","rang_cc_d2"
))

bdd_4 <- bdd_4 %>% arrange(desc(annee), voie_lib, nom) %>% 
  rename(annee=annee) %>% 
  select(-c("situation","X","rattrapage_max", "RES1", "RHS1", "RIS1", "RSS1",
            "RES2", "RHS2", "RIS2", "RSS2","redoublement"))

#########  Création des variables redoublement et exclusion_demission 
######### Elles  ne sont pas bien renseignées dans Pamlemousse ############

niveaux <- c("1A","2A","3A")

# Base : uniquement les années 1A/2A/3A
base <- bdd_4 %>%
  filter(annee_ecole %in% niveaux) %>%
  mutate(
    annee       = as.integer(annee),
    annee_ecole = factor(annee_ecole, levels = niveaux)
  ) %>%
  distinct(id_etudiant, annee, annee_ecole, .keep_all = TRUE)

# Indicateurs par étudiant
indicateurs <- base %>%
  group_by(id_etudiant) %>%
  summarise(
    redoublement       = as.integer(any(id_commentaire_bulletin_ref == 4)),
    redoublement_annee = paste(sort(unique(annee_ecole[id_commentaire_bulletin_ref == 4])),
                               collapse = ", "),
    
    demission          = as.integer(any(id_commentaire_bulletin_ref == 3)),
    demission_annee    = paste(sort(unique(annee_ecole[id_commentaire_bulletin_ref == 3])),
                               collapse = ", "),
    
    exclusion          = as.integer(any(id_commentaire_bulletin_ref == 2)),
    exclusion_annee    = paste(sort(unique(annee_ecole[id_commentaire_bulletin_ref == 2])),
                               collapse = ", "),
    
    .groups = "drop"
  ) %>%
  mutate(
    redoublement_annee = na_if(redoublement_annee, ""),
    demission_annee    = na_if(demission_annee, ""),
    exclusion_annee    = na_if(exclusion_annee, "")
  )

# Résultat final : une ligne par étudiant
base_final <- base %>%
  distinct(id_etudiant) %>%
  left_join(indicateurs, by = "id_etudiant")

table(base_final$redoublement)
table(base_final$demission)
table(base_final$exclusion)

bdd_7 <- left_join(bdd_4, base_final, by = "id_etudiant")

# ## Anonymisation
# 
# # Clé secrète pour chiffrer/déchiffrer
# cle_secrete <- "Theophilus81!" # (me demander mon mot de passe - CL)
# 
# # Fonction de chiffrement (XOR + Base64)
# encrypt_id <- function(id_vector, cle) {
#   sapply(id_vector, function(id) {
#     id_raw <- as.integer(charToRaw(as.character(id)))
#     cle_raw <- as.integer(charToRaw(cle))
#     cle_longue <- rep(cle_raw, length.out = length(id_raw))
#     xor_result <- bitwXor(id_raw, cle_longue)
#     base64enc::base64encode(as.raw(xor_result))
#   })
# }
# 
# # Fonction de déchiffrement (Base64 + XOR)
# decrypt_id <- function(enc_vector, cle) {
#   sapply(enc_vector, function(enc) {
#     chiffré <- as.integer(base64enc::base64decode(enc))
#     cle_raw <- as.integer(charToRaw(cle))
#     cle_longue <- rep(cle_raw, length.out = length(chiffré))
#     xor_result <- bitwXor(chiffré, cle_longue)
#     rawToChar(as.raw(xor_result))
#   })
# }


# # Chiffrement
# bdd_7$id_crypte <- encrypt_id(bdd_7$id_etudiant, cle_secrete)
# 

# Déchiffrement
#bdd_7$id_decrypte <- decrypt_id(bdd_7$id_crypte, cle_secrete)

#  Vérification
# print(bdd_7)
# print(all(bdd_7$id_etudiant == bdd_7$id_decrypte))  # Doit afficher TRUE

# Ajout de variables

# recoder spe_entree pour maths
bdd_7 <- bdd_7 %>%
  mutate(spe_entree = case_when(
    grepl("mpi|mp2i|math[\\s\\-]*phys[\\s\\-]*info", etab_origine_formation, ignore.case = TRUE) ~ "MPI",
    grepl("pc|ps", etab_origine_formation, ignore.case = TRUE) ~ "PC/PSI",
    grepl("mp", etab_origine_formation, ignore.case = TRUE) ~ "MP",
    grepl("math", concours_origine, ignore.case = TRUE) ~ "MP",
    TRUE ~ "MP"
  ))

# recoder spe_entree pour ECO
bdd_7$spe_entree <- ifelse(bdd_7$concours_origine =="Concours externe : spécialité 'économie et sciences sociales'",
                           "BL", 
                           ifelse(bdd_7$concours_origine =="concours externe : spécialité 'économie et sciences sociales'",
                                  "BL",
                                  ifelse (bdd_7$concours_origine == "Concours externe : spécialité 'économie et gestion'",
                                          "D2",
                                          bdd_7$spe_entree)))
table(bdd_7$concours_origine)

# recoder spe_entree pour Autres
bdd_7 <- bdd_7 %>%
  mutate(
    spe_entree = case_when(
      grepl("IUT STID", concours_origine, ignore.case = TRUE) ~ "BUT",
      grepl("AST-1A", voie_entree, ignore.case = TRUE) ~ "AST-1A",
      grepl("AST-2A", voie_entree, ignore.case = TRUE) ~ "AST-2A",
      concours_origine == "Concours interne" ~ "INSEE",
      concours_origine %in% c("Contractuel", "Erasmus") ~ "ERASMUS",
      TRUE ~ spe_entree  
    )
  )

table(bdd_7$spe_entree, useNA = "ifany")

# Il s'agit de continuer d'affiner à partir de tous les cas possibles
sort(table(bdd_7$etab_origine_formation), decreasing = TRUE)

# Prépa étoile

bdd_7 <- bdd_7 %>%
  mutate(prepa_etoile = case_when(
    grepl("\\*", etab_origine_formation, ignore.case = TRUE) ~ 1,
    TRUE ~ 0
  ))

# creation des blocs annuels
bdd_7$bloc_an<- ifelse(bdd_7$annee %in% c(2015,2016,2017),
                              "2015-2017",
                              ifelse (bdd_7$annee %in% c(2018,2019,2020),
                                      "2018-2020",
                                      ifelse(bdd_7$annee %in% c(2021,2022,2023),
                                             "2021-2023",
                                             "2024-2025")))

# Correction de la variable att_ing

bdd_8 <- bdd_7 %>% 
  select(annee, voie_lib, nom, prenom, statut_etudiant, att_ing)

bdd_9 <- bdd_8[substr(tolower(bdd_8$statut_etudiant), 1, 3) != substr(tolower(bdd_8$att_ing), 1, 3), ] %>% 
  distinct

# Export en xlsx
write_xlsx(bdd_9, path = "att_ing.xlsx")

# Export en csv
write.csv2(bdd_7, "data/bdd_2015_2025.csv", row.names = FALSE)

# Export en xlsx
write_xlsx(bdd_7, path = "data/bdd_2015_2025.xlsx")

# Export en parquet
write_parquet(bdd_7,"data/bdd_2015_2025.parquet")

# Export en RDS
write_rds(bdd_7, "data/bdd_2015_2025.rds")

# Dictionnaire des variables

# Labels pour création d'un dictionnaire
bdd_7$annee_scolaire <- structure(bdd_7$annee_scolaire, label = "Année de scolarité")
#bdd_7$id_crypte <- structure(bdd_7$id_crypte , label = "Identifiant crypté de l'étudiant")
bdd_7$sexe <- structure(bdd_7$sexe, label = "Sexe de l'étudiant")
bdd_7$nationalite <- structure(bdd_7$nationalite, label = "Nationalité de l'étudiant")
bdd_7$id_nationalite <- structure(bdd_7$id_nationalite, label = "Identifiant de la nationalité de l'étudiant")
bdd_7$paysnai <- structure(bdd_7$paysnai, label = "Pays de naissance de l'étudiant")
bdd_7$etab_origine_formation <- structure(bdd_7$etab_origine_formation, label = "Type d'établissement d'origine de l'étudiant")
bdd_7$concours_origine <- structure(bdd_7$concours_origine, label = "Concours d'origine de l'étudiant")
bdd_7$concours_annee <- structure(bdd_7$concours_annee, label = "Année d'entrée à l'Ensai de l'étudiant")
bdd_7$bac_annee <- structure(bdd_7$bac_annee, label = "Année d'obtention du bac de l'étudiant")
bdd_7$bac_mention <- structure(bdd_7$bac_mention, label = "Mention au bac de l'étudiant")
bdd_7$toeic <- structure(bdd_7$toeic, label = "Score au Toeic ")
bdd_7$annee_ecole <- structure(bdd_7$annee_ecole, label = "Niveau/Année dans l'école (1A-2A-3A)")
bdd_7$statut_etudiant <- structure(bdd_7$statut_etudiant, label = "Statut de l'étudiant")
bdd_7$voie_entree <- structure(bdd_7$voie_entree, label = "Voie d'entrée à l'Ensai")
bdd_7$specialite_entree <- structure(bdd_7$specialite_entree, label = "Spécialité à l'entrée à l'Ensai")
bdd_7$filiere_1A <- structure(bdd_7$filiere_1A, label = "Filière en 1ère année à l'Ensai")
bdd_7$filiere_2A <- structure(bdd_7$filiere_2A, label = "Filière en 2ème année à l'Ensai")
bdd_7$filiere_3A <- structure(bdd_7$filiere_3A, label = "Filière en 3ème année à l'Ensai")
bdd_7$id_type_matiere <- structure(bdd_7$id_type_matiere, label = "Identifiant de la matière")
bdd_7$cat_matiere <- structure(bdd_7$cat_matiere, label = "Catégorie de la matière")
bdd_7$code_matiere <- structure(bdd_7$code_matiere, label = "Code de la matière")
bdd_7$matiere <- structure(bdd_7$matiere, label = "Nom de la matière")
bdd_7$ue <- structure(bdd_7$ue, label = "Nom de l'UE de la matière")
bdd_7$moyenne_matiere <- structure(bdd_7$moyenne_matiere, label = "Note moyenne de l'étudiant à la matière")
bdd_7$moyenne_ue <- structure(bdd_7$moyenne_ue, label = "Note moyenne de l'étudiant à l'a matière'UE")
bdd_7$voie_lib <- structure(bdd_7$voie_lib, label = "Libellé long de la voie d'appartenance")
bdd_7$rang_matiere <- structure(bdd_7$voie_lib, label = "Rang dans la matière")
bdd_7$rang_max_matiere <- structure(bdd_7$voie_lib, label = "Rang max dans la matière")
bdd_7$moyenne_generale <- structure(bdd_7$voie_lib, label = "Moyenne générale sur l'année")
bdd_7$total_coeff <- structure(bdd_7$voie_lib, label = "Coefficient de la matière")
bdd_7$total_ects <- structure(bdd_7$total_ects, label = "Crédits ECTS de la matière")
bdd_7$filiere_3Abis <- structure(bdd_7$filiere_3Abis, label = "Filière en 3ème année à l'Ensai revue par Stéphane")
bdd_7$toeic <- structure(bdd_7$toeic, label = "Score au Toeic")
bdd_7$annee <- structure(bdd_7$annee, label = "Année en cours")
bdd_7$libelle_etat_civil <- structure(bdd_7$libelle_etat_civil, label = "État civil")
bdd_7$id_ref_paysnai <- structure(bdd_7$id_ref_paysnai, label = "Identifiant du pays de naissance de l'étudiant")
bdd_7$libelle_statut_etudiant <- structure(bdd_7$libelle_statut_etudiant, label = "Statut de l'étudiant")
bdd_7$libelle_statut_etudiant <- structure(bdd_7$libelle_statut_etudiant, label = "Statut de l'étudiant")
bdd_7$certif_anglais_org <- structure(bdd_7$certif_anglais_org, label = "Organisme de certification du niveau d'anglais")
bdd_7$certif_anglais_score<- structure(bdd_7$certif_anglais_score, label = "Score du certificat du niveau d'anglais")
bdd_7$date_naissance <- structure(bdd_7$date_naissance, label = "Date de naissance")
bdd_7$double_diplome <- structure(bdd_7$double_diplome, label = "Douple diplôme")
bdd_7$autre_diplome <- structure(bdd_7$autre_diplome, label = "Autre diplôme")
bdd_7$att_ing <- structure(bdd_7$att_ing, label = "Attaché ou Ingénieur")
bdd_7$bac_spe1 <- structure(bdd_7$bac_spe1, label = "Spécialité de bac 1")
bdd_7$bac_spe2 <- structure(bdd_7$bac_spe2, label = "Spécialité de bac 2")
bdd_7$cpge <- structure(bdd_7$cpge, label = "Type de classe prépa")
bdd_7$bourse <- structure(bdd_7$bourse, label = "Boursier")
bdd_7$bourse_type <- structure(bdd_7$bourse_type, label = "Type de bourse")
bdd_7$bourse_montant <- structure(bdd_7$bourse_montant, label = "Montant de la bourse")
bdd_7$id_pcs_pere <- structure(bdd_7$id_pcs_pere, label = "Identifiant PCS du père")
bdd_7$id_pcs_mere <- structure(bdd_7$id_pcs_mere, label = "Identifiant PCS de la mère")
bdd_7$MES1 <- structure(bdd_7$MES1, label = "Moyenne Économie S1")
bdd_7$MHS1 <- structure(bdd_7$MHS1, label = "Moyenne Humanités S1")
bdd_7$MSS1 <- structure(bdd_7$MSS1, label = "Moyenne Statistique S1")
bdd_7$MIS1 <- structure(bdd_7$MIS1, label = "Moyenne Informatique S1")
bdd_7$MES2 <- structure(bdd_7$MES2, label = "Moyenne Économie S2")
bdd_7$MHS2 <- structure(bdd_7$MHS2, label = "Moyenne Humanités S2")
bdd_7$MSS2 <- structure(bdd_7$MSS2, label = "Moyenne Statistique S2")
bdd_7$MIS2 <- structure(bdd_7$MIS2, label = "Moyenne Informatique S2")
bdd_7$MGS1 <- structure(bdd_7$MGS1, label = "Moyenne Générale S1")
bdd_7$MGS2 <- structure(bdd_7$MGS2, label = "Moyenne Générale S2")
bdd_7$AV <- structure(bdd_7$MGS2, label = "Année validée")
bdd_7$point_bonus <- structure(bdd_7$point_bonus, label = "Point de bonus")
bdd_7$point_jury <- structure(bdd_7$point_jury, label = "Point de jury")
#bdd_7$objectifs_matiere <- structure(bdd_7$objectifs_matiere, label = "Commentaire matière dont explication point de bonus")
bdd_7$commentaire <- structure(bdd_7$commentaire, label = "Commentaire sur la décision de validation de l'année")
bdd_7$bonus_type <- structure(bdd_7$bonus_type, label = "type de bonus (à déterminer)")
bdd_7$id_commentaire_bulletin_ref <- structure(bdd_7$id_commentaire_bulletin_ref, label = "Décision de validation (cf table table_bulletin_ref_id_bonus)")
bdd_7$verrou <- structure(bdd_7$verrou, label = "Variable récupérée dans commentaire (à déterminer)")
#bdd_7$id_crypte <- structure(bdd_7$id_crypte, label = "Identifiant crypté de l'étudiant")
bdd_7$rang_cc_maths <- structure(bdd_7$rang_cc_maths, label = "Classement au concours commun mathématiques")
bdd_7$rang_cc_bl <- structure(bdd_7$rang_cc_bl, label = "Classement au concours BL")
bdd_7$rang_cc_d2 <- structure(bdd_7$rang_cc_d2, label = "Classement au concours D2")
bdd_7$spe_entree <- structure(bdd_7$spe_entree, label = "Spécialité à l'entrée")
bdd_7$prepa_etoile <- structure(bdd_7$prepa_etoile, label = "Provenance d'une prépa étoile")
bdd_7$redoublement <- structure(bdd_7$redoublement, label = "Redoublement")
bdd_7$redoublement_annee <- structure(bdd_7$redoublement_annee, label = "Année(s) de redoublement")
bdd_7$demission <- structure(bdd_7$demission, label = "Démission de l'étudiant")
bdd_7$demission_annee <- structure(bdd_7$demission_annee, label = "Année de démission de l'étudiant")
bdd_7$exclusion <- structure(bdd_7$exclusion, label = "Exclusion de l'étudiant")
bdd_7$exclusion_annee <- structure(bdd_7$exclusion_annee, label = "Année d'exclusion de l'étudiant")

# Fabrication du dictionnaire
dictionnaire_labels <- function(df) {
  tibble(
    variable = names(df),
    label    = map_chr(df, ~ attr(.x, "label") %||% ""),
    format   = map_chr(df, ~ class(.x)[1])      
  )
}

dict <- dictionnaire_labels(bdd_7)

write_xlsx(dict, path = "data/dictionnaire.xlsx")

write_parquet(dict, "data/dictionnaire.parquet")

