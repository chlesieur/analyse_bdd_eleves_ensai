library(purrr)
library(dplyr)
library(stringr)
library(arrow)
library(writexl)
library(labelled)
library(sjlabelled)
library(stringi)

######### Lecture des fichiers de la requete principale ##########

# Répertoire où déposer les exports pamplemousse tels quels
repertoire <- "data/export pamplemousse"

# Lecture automatique des exports pamplemousse
fichiers_csv <- list.files(path = paste0(repertoire,"/requete principale"), pattern = "*.csv")

# tri les fichiers 
fichiers_csv_tries <- sort(fichiers_csv)

# Initialise un dataframe bdd
bdd <- data.frame()

for (fichier in fichiers_csv_tries) {
  
  data <- read.csv2(paste0(repertoire,"/requete principale/",fichier), encoding = "latin1")
  
  # Vérifie si les colonnes sont les mêmes que celles de la bdd
  if (ncol(bdd) > 0 && !identical(names(data), names(bdd))) {
    stop(paste("Les colonnes du fichier", fichier, "ne correspondent pas aux autres fichiers."))
  }
  
  # Ajoute les données du fichier courant à la bdd
  bdd <- rbind(bdd, data)
}

# Vérification de doublons
bdd <- bdd %>%
  distinct(X.annee_courante, X.id_etudiant, X.code_matiere, .keep_all = TRUE)


######### Lecture des fichiers de la requete points bonus et jury ##########

# Répertoire où déposer les exports pamplemousse tels quels
repertoire <- "data/export pamplemousse"

# Lecture automatique des exports pamplemousse
fichiers_csv <- list.files(path = paste0(repertoire,"/requete points bonus et jury"), pattern = "*.csv")

# tri les fichiers 
fichiers_csv_tries <- sort(fichiers_csv)

# Initialise un dataframe bdd
bdd_points <- data.frame()

for (fichier in fichiers_csv_tries) {
  
  data <- read.csv2(paste0(repertoire,"/requete points bonus et jury/",fichier), encoding = "latin1")
  
  # Vérifie si les colonnes sont les mêmes que celles de la bdd
  if (ncol(bdd_points) > 0 && !identical(names(data), names(bdd_points))) {
    stop(paste("Les colonnes du fichier", fichier, "ne correspondent pas aux autres fichiers."))
  }
  
  # Ajoute les données du fichier courant à la bdd
  bdd_points <- rbind(bdd_points, data)
}

bdd_points_1 <- bdd_points %>% 
  select(-c("X.ue","X.matiere","X","X.id_type_matiere"))

# Elimination des doublons
bdd_points_2 <- bdd_points_1 %>%
  distinct(X.annee_courante, X.id_etudiant, X.code_matiere, .keep_all = TRUE)

################# fusion des deux bases ########################"""

bdd_2 <- left_join(bdd, bdd_points_2, 
                 by = c("X.annee_courante",
                        "X.id_etudiant",
                        "X.code_matiere"))

bdd <- unique(bdd_2)

######################## Nettoyage ##########################

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

# Création des variables de travail
# Choix à discuter

bdd_2 <- bdd %>%
  mutate(
    annee_scolaire = as.character(paste0(as.numeric(annee),"-",as.numeric(annee)+1)),
    annee_ecole = case_when(
      substr(voie_lib, 1, 2) == "1A" ~ "1A",
      substr(voie_lib, 1, 2) == "2A" ~ "2A",
      substr(voie_lib, 1, 2) == "3A" ~ "3A",
      grepl("Mast", voie_lib, ignore.case = TRUE) == TRUE ~ "3A",
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
                   keep = FALSE) %>% 
  select(-point_jury)

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

bdd_rang <- read_parquet("Z:/0_Direction_des_Etudes/Base élève/analyse_bdd_eleves_ensai/data/20250618/bdd_2015_2024.parquet")

bdd_rang_1 <- bdd_rang %>% 
  select(annee,id_etudiant,code_matiere,rang_matiere, rang_max_matiere, point_jury)

bdd_3b <- left_join(bdd_3, bdd_rang_1, by = c("annee","id_etudiant","code_matiere"))

bdd_4$point_jury <- ifelse(grepl("points de jury", bdd_4$matiere, ignore.case = TRUE),
                           bdd_4$point_jury, 
                           NA)

table(bdd_4$point_jury)
table(bdd_4$point_bonus)

## Numérisation de certaines variables

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


library(readr)

bdd_4 <- numeriser(
  bdd_3b,
  c("rang_matiere",
  "rang_max_matiere",
  "moyenne_generale",
  "total_coeff",
  "total_ects",
  "moyenne_generale",
  "moyenne_matiere",
  "moyenne_ue",
  "toeic",
  "MES1", "MHS1", "MIS1", "MSS1",
  "MES2", "MHS2", "MIS2", "MSS2",
  "MGS1", "MGS2"
))

bdd_4 <- bdd_4 %>% arrange(desc(annee), voie_lib, nom) %>% 
  rename(annee=annee) %>% 
  select(-c("situation","X","rattrapage_max", "RES1", "RHS1", "RIS1", "RSS1",
            "RES2", "RHS2", "RIS2", "RSS2","nom","prenom"))



## Anonymisation

# 🔑 Clé secrète pour chiffrer/déchiffrer
cle_secrete <- "Theophilus81!" # (me demander mon mot de passe - CL)

# 🔐 Fonction de chiffrement (XOR + Base64)
encrypt_id <- function(id_vector, cle) {
  sapply(id_vector, function(id) {
    id_raw <- as.integer(charToRaw(as.character(id)))
    cle_raw <- as.integer(charToRaw(cle))
    cle_longue <- rep(cle_raw, length.out = length(id_raw))
    xor_result <- bitwXor(id_raw, cle_longue)
    base64enc::base64encode(as.raw(xor_result))
  })
}

# 🔓 Fonction de déchiffrement (Base64 + XOR)
decrypt_id <- function(enc_vector, cle) {
  sapply(enc_vector, function(enc) {
    chiffré <- as.integer(base64enc::base64decode(enc))
    cle_raw <- as.integer(charToRaw(cle))
    cle_longue <- rep(cle_raw, length.out = length(chiffré))
    xor_result <- bitwXor(chiffré, cle_longue)
    rawToChar(as.raw(xor_result))
  })
}


# 🔐 Chiffrement
bdd_4$id_crypte <- encrypt_id(bdd_4$id_etudiant, cle_secrete)

bdd_4 <- bdd_4 %>% 
  select(-id_etudiant)

# 🔓 Déchiffrement
#bdd_4$id_decrypte <- decrypt_id(bdd_4$id_crypte, cle_secrete)

# ✅ Vérification
print(bdd_4)
print(all(bdd_4$id_etudiant == bdd_4$id_decrypte))  # Doit afficher TRUE

# Export en csv
write.csv2(bdd, "data/bdd_2015_2024.csv", row.names = FALSE)

# Export en xlsx
bdd_4 <- bdd_4 %>% 
  mutate(across(where(is.character),
                ~ stri_encode(., from = "latin1", to = "UTF-8"))) 

library(writexl)
write_xlsx(bdd_4, path = "data/bdd_2015_2024.xlsx")

# Export en parquet
write_parquet(bdd_4,"data/bdd_2015_2024.parquet")

# Export en RDS
write_rds(bdd_4, "data/bdd_2015_2024.rds")

# Dictionnaire des variables

# Labels pour création d'un dictionnaire
bdd_4$annee_scolaire <- structure(bdd_4$annee_scolaire, label = "Année de scolarité")
bdd_4$nom <- structure(bdd_4$nom , label = "Nom de l'étudiant")
bdd_4$prenom <- structure(bdd_4$prenom, label = "Prénom de l'étudiant")
bdd_4$sexe <- structure(bdd_4$sexe, label = "Sexe de l'étudiant")
bdd_4$nationalite <- structure(bdd_4$nationalite, label = "Nationalité de l'étudiant")
bdd_4$id_nationalite <- structure(bdd_4$id_nationalite, label = "Identifiant de la nationalité de l'étudiant")
bdd_4$paysnai <- structure(bdd_4$paysnai, label = "Pays de naissance de l'étudiant")
bdd_4$etab_origine_formation <- structure(bdd_4$etab_origine_formation, label = "Type d'établissement d'origine de l'étudiant")
bdd_4$concours_origine <- structure(bdd_4$concours_origine, label = "Concours d'origine de l'étudiant")
bdd_4$concours_annee <- structure(bdd_4$concours_annee, label = "Année d'entrée à l'Ensai de l'étudiant")
bdd_4$bac_annee <- structure(bdd_4$bac_annee, label = "Année d'obtention du bac de l'étudiant")
bdd_4$bac_mention <- structure(bdd_4$bac_mention, label = "Mention au bac de l'étudiant")
bdd_4$toeic <- structure(bdd_4$toeic, label = "Score au Toeic ")
bdd_4$annee_ecole <- structure(bdd_4$annee_ecole, label = "Niveau/Année dans l'école (1A-2A-3A)")
bdd_4$statut_etudiant <- structure(bdd_4$statut_etudiant, label = "Statut de l'étudiant")
bdd_4$voie_entree <- structure(bdd_4$voie_entree, label = "Voie d'entrée à l'Ensai")
bdd_4$specialite_entree <- structure(bdd_4$specialite_entree, label = "Spécialité à l'entrée à l'Ensai")
bdd_4$filiere_1A <- structure(bdd_4$filiere_1A, label = "Filière en 1ère année à l'Ensai")
bdd_4$filiere_2A <- structure(bdd_4$filiere_2A, label = "Filière en 2ème année à l'Ensai")
bdd_4$filiere_3A <- structure(bdd_4$filiere_3A, label = "Filière en 3ème année à l'Ensai")
bdd_4$id_type_matiere <- structure(bdd_4$id_type_matiere, label = "Identifiant de la matière")
bdd_4$cat_matiere <- structure(bdd_4$cat_matiere, label = "Catégorie de la matière")
bdd_4$code_matiere <- structure(bdd_4$code_matiere, label = "Code de la matière")
bdd_4$matiere <- structure(bdd_4$matiere, label = "Nom de la matière")
bdd_4$ue <- structure(bdd_4$ue, label = "Nom de l'UE de la matière")
bdd_4$moyenne_matiere <- structure(bdd_4$moyenne_matiere, label = "Note moyenne de l'étudiant à la matière")
bdd_4$moyenne_ue <- structure(bdd_4$moyenne_ue, label = "Note moyenne de l'étudiant à l'a matière'UE")
bdd_4$voie_lib <- structure(bdd_4$voie_lib, label = "Libellé long de la voie d'appartenance")
bdd_4$rang_matiere <- structure(bdd_4$voie_lib, label = "Rang dans la matière")
bdd_4$rang_max_matiere <- structure(bdd_4$voie_lib, label = "Rang max dans la matière")
bdd_4$moyenne_generale <- structure(bdd_4$voie_lib, label = "Moyenne générale sur l'année")
bdd_4$total_coeff <- structure(bdd_4$voie_lib, label = "Coefficient de la matière")
bdd_4$total_ects <- structure(bdd_4$total_ects, label = "Crédits ECTS de la matière")
bdd_4$filiere_3Abis <- structure(bdd_4$filiere_3Abis, label = "Filière en 3ème année à l'Ensai revue par Stéphane")
bdd_4$toeic <- structure(bdd_4$toeic, label = "Score au Toeic")
bdd_4$annee <- structure(bdd_4$annee, label = "Année en cours")
bdd_4$libelle_etat_civil <- structure(bdd_4$libelle_etat_civil, label = "État civil")
bdd_4$id_ref_paysnai <- structure(bdd_4$id_ref_paysnai, label = "Identifiant du pays de naissance de l'étudiant")
bdd_4$libelle_statut_etudiant <- structure(bdd_4$libelle_statut_etudiant, label = "Statut de l'étudiant")
bdd_4$libelle_statut_etudiant <- structure(bdd_4$libelle_statut_etudiant, label = "Statut de l'étudiant")
bdd_4$certif_anglais_org <- structure(bdd_4$certif_anglais_org, label = "Organisme de certification du niveau d'anglais")
bdd_4$certif_anglais_score<- structure(bdd_4$certif_anglais_score, label = "Score du certificat du niveau d'anglais")
bdd_4$date_naissance <- structure(bdd_4$date_naissance, label = "Date de naissance")
bdd_4$double_diplome <- structure(bdd_4$double_diplome, label = "Douple diplôme")
bdd_4$autre_diplome <- structure(bdd_4$autre_diplome, label = "Autre diplôme")
bdd_4$redoublement <- structure(bdd_4$redoublement, label = "Redoublement")
bdd_4$att_ing <- structure(bdd_4$att_ing, label = "Attaché ou Ingénieur")
bdd_4$bac_spe1 <- structure(bdd_4$bac_spe1, label = "Spécialité de bac 1")
bdd_4$bac_spe2 <- structure(bdd_4$bac_spe2, label = "Spécialité de bac 2")
bdd_4$cpge <- structure(bdd_4$cpge, label = "Type de classe prépa")
bdd_4$bourse <- structure(bdd_4$bourse, label = "Boursier")
bdd_4$bourse_type <- structure(bdd_4$bourse_type, label = "Type de bourse")
bdd_4$bourse_montant <- structure(bdd_4$bourse_montant, label = "Montant de la bourse")
bdd_4$id_pcs_pere <- structure(bdd_4$id_pcs_pere, label = "Identifiant PCS du père")
bdd_4$id_pcs_mere <- structure(bdd_4$id_pcs_mere, label = "Identifiant PCS de la mère")
bdd_4$MES1 <- structure(bdd_4$MES1, label = "Moyenne Économie S1")
bdd_4$MHS1 <- structure(bdd_4$MHS1, label = "Moyenne Humanités S1")
bdd_4$MSS1 <- structure(bdd_4$MSS1, label = "Moyenne Statistique S1")
bdd_4$MIS1 <- structure(bdd_4$MIS1, label = "Moyenne Informatique S1")
bdd_4$MES2 <- structure(bdd_4$MES2, label = "Moyenne Économie S2")
bdd_4$MHS2 <- structure(bdd_4$MHS2, label = "Moyenne Humanités S2")
bdd_4$MSS2 <- structure(bdd_4$MSS2, label = "Moyenne Statistique S2")
bdd_4$MIS2 <- structure(bdd_4$MIS2, label = "Moyenne Informatique S2")
bdd_4$MGS1 <- structure(bdd_4$MGS1, label = "Moyenne Générale S1")
bdd_4$MGS2 <- structure(bdd_4$MGS2, label = "Moyenne Générale S2")
bdd_4$AV <- structure(bdd_4$MGS2, label = "Année validée")
bdd_4$point_bonus <- structure(bdd_4$point_bonus, label = "Point de bonus")
bdd_4$point_jury <- structure(bdd_4$point_jury, label = "Point de jury")
bdd_4$objectifs_matiere <- structure(bdd_4$objectifs_matiere, label = "Commentaire matière dont explication point de bonus")
bdd_4$commentaire <- structure(bdd_4$commentaire, label = "Commentaire sur la décision de validation de l'année")
bdd_4$bonus_type <- structure(bdd_4$bonus_type, label = "type de bonus (à déterminer)")
bdd_4$id_commentaire_bulletin_ref <- structure(bdd_4$id_commentaire_bulletin_ref, label = "Décision de validation (cf table table_bulletin_ref_id_bonus)")
bdd_4$verrou <- structure(bdd_4$verrou, label = "Variable récupérée dans commentaire (à déterminer)")
bdd_4$id_crypte <- structure(bdd_4$id_crypte, label = "Identifiant crypté de l'étudiant")



# Fabrication du dictionnaire
dictionnaire_labels <- function(df) {
  tibble(
    variable = names(df),
    label    = map_chr(df, ~ attr(.x, "label") %||% ""),
    format   = map_chr(df, ~ class(.x)[1])      
  )
}

dict <- dictionnaire_labels(bdd_4)

write_xlsx(dict, path = "data/dictionnaire.xlsx")



