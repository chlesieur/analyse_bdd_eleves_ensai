library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(readxl)
library(arrow)
library(splines)

bdd <- read_excel("D:/data/bdd_2015_2024.xlsx")#lire le fichier en excel
write_parquet(bdd, "D:/data/bdd_2015_2024.parquet")
bdd <- read_parquet("D:/data/bdd_2015_2024.parquet")
#str(bdd)

# travaux sur 1A et 2A
bdd_1a2a <- bdd[bdd$annee_ecole %in% c("1A","2A"),]
bdd_1a2a$spe_entree <- ifelse(bdd_1a2a$concours_origine =="Concours externe : spécialité 'économie et sciences sociales'",
                              "Eco BL", 
                              ifelse(bdd_1a2a$concours_origine =="concours externe : spécialité 'économie et sciences sociales'",
                                     "Eco BL",
                                     ifelse (bdd_1a2a$concours_origine == "Concours externe : spécialité 'économie et gestion'",
                                             "Eco D2",
                                             bdd_1a2a$specialite_entree)))

#voir toute les modalité de la variable spe_entree                                                                                         bdd_1a2a$specialite_entree)))
#unique(bdd_1a2a$spe_entree)#on a trouvé 6 modalités
#unique(bdd_1a2a$concours_origine)

# recoder spe_entree pour Autres
bdd_1a2a$spe_entree <- ifelse(bdd_1a2a$concours_origine =="Admission sur titres (dossier + entretien) : niveau L3",
                              "AST 1A", 
                              ifelse(bdd_1a2a$concours_origine =="Admission sur titres (dossier + entretien) : niveau M1 ou plus, admission en 1ère année",
                                     "AST 1A",
                                     ifelse (bdd_1a2a$concours_origine == "Admission sur titres (dossier + entretien) : niveau M1 ou plus, admission en 2e année",
                                             "AST 2A",
                                             ifelse (bdd_1a2a$concours_origine == "Concours externe",
                                                     "ERASMUS - Contractuels",
                                                     ifelse (bdd_1a2a$concours_origine == "Contractuel",
                                                             "ERASMUS - Contractuels",
                                                             ifelse (bdd_1a2a$concours_origine == "Erasmus",
                                                                     "ERASMUS - Contractuels",
                                                                     bdd_1a2a$spe_entree))))))


#bdd_1a2a$spe_entree 

#recoder spe_entree pour mathématiques


#CAS DE PSI
bddmat<-bdd_1a2a[bdd_1a2a$concours_origine %in% c("Concours externe : spécialité 'mathématiques'"), ]
#unique(bddmat$etab_origine_formation)

bddmat_PSI <- grepl("PSI", bddmat$etab_origine_formation, ignore.case = TRUE) &
    !grepl("MPSI", bddmat$etab_origine_formation, ignore.case = TRUE)


bdd_1a2a$spe_entree[which(bddmat_PSI)] <- "PSI"


#CAS DE MP
# Liste des motifs à inclure mp
motifs_inclus_mp <- c(
  "MP",
  "math et physique",
  "mathématique et phy",
  "Mathématique-Physique",
  "Mathématique Physique",
  "CPGE Maths et Physique",
  "Mathématiques Physique" ,
  "Maths-physique"   
  # tu peux rajouter ici
)

# Liste des motifs à exclure mp
motifs_exclus_mp <- c(
  "MPSI-MPI",
  "MPI",
  "MP2I-PSI",
  "MP2I/MPI" ,
  "MPI",
  "CPGE MP* Spécialité informatique" ,
  "MP option SI",
  "MPSI-PSI",
  "CPGE Maths-Physique option Informatique"  
  # tu peux rajouter ici
)

# -----------------------------
# Sélection
bddmat_mp <- grepl(paste(motifs_inclus_mp, collapse = "|"),
        bddmat$etab_origine_formation,
        ignore.case = TRUE) &
    !grepl(paste(motifs_exclus_mp, collapse = "|"),
           bddmat$etab_origine_formation,
           ignore.case = TRUE)


bdd_1a2a$spe_entree[which(bddmat_mp)] <- "MP"


#cas de MPI
# Liste des motifs à inclure mpi
motifs_inclus_mpi <- c(
  "MPSI-MPI",
  "MPI",
  "CPGE MP* Spécialité informatique" ,
  "MP2I/MPI" ,
  "CPGE Maths-Physique option Informatique"
  
  
  # tu peux rajouter ici
)

# Liste des motifs à exclure mpi
motifs_exclus_mpi <- c(
  "MPSI-PSI"
  # tu peux rajouter ici
)

# -----------------------------
# Sélection
bddmat_mpi<- grepl(paste(motifs_inclus_mpi, collapse = "|"),
        bddmat$etab_origine_formation,
        ignore.case = TRUE) &
    !grepl(paste(motifs_exclus_mpi, collapse = "|"),
           bddmat$etab_origine_formation,
           ignore.case = TRUE)


bdd_1a2a$spe_entree[which(bddmat_mpi)] <- "MPI"

#cas de PC
# Liste des motifs à inclure PC
motifs_inclus_pc <- c(
  "PC",
  "Physique-Chimie",
  "Physique chimie"  
  # tu peux rajouter ici
)



# Sélection
bddmat_PC <- grepl(paste(motifs_inclus_pc, collapse = "|"),
        bddmat$etab_origine_formation,
        ignore.case = TRUE)


bdd_1a2a$spe_entree[which(bddmat_PC)] <- "PC"

#maintenant je vais modifier la variable mathématiques par ces noms

bdd_1a2a$spe_entree




#  conserver une ligne par eleve et par année 
bdd_1a2a_net <- bdd_1a2a[!duplicated(cbind(bdd_1a2a$id_crypte,bdd_1a2a$annee)),]


# creation boucles sortie stat par bloc annee

bdd_1a2a_net$bloc_an<- ifelse(bdd_1a2a_net$annee %in% c(2015,2016,2017),
                              "2015-2017",
                              ifelse (bdd_1a2a_net$annee %in% c(2018,2019,2020),
                                      "2018-2020",
                                      ifelse(bdd_1a2a_net$annee %in% c(2021,2022),
                                             "2021-2022",
                                             "2023-2024")))


#unique(bdd_1a2a$spe_entree)

# denombrement eleves sur 10 ans
stock_denomb_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_denomb_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_denomb_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_denomb_2a) <-  c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")



# Colonnes de référence (vides au départ)
all_cols_1a <- colnames(stock_denomb_1a)
all_cols_2a <- colnames(stock_denomb_2a)

for (i in unique(bdd_1a2a_net$bloc_an)) {
  
  bdd_1a2a_net_an <- bdd_1a2a_net[bdd_1a2a_net$bloc_an == i, ]
  
  denomb_1a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "1A") %>%
    group_by(spe_entree) %>%
    summarise(nb_eleves_1a = n()) %>%
    tidyr::pivot_wider(names_from = spe_entree, values_from = nb_eleves_1a) %>%
    mutate(across(everything(), ~ replace_na(.x, 0)))
  
  denomb_2a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "2A") %>%
    group_by(spe_entree) %>%
    summarise(nb_eleves_2a = n()) %>%
    tidyr::pivot_wider(names_from = spe_entree, values_from = nb_eleves_2a) %>%
    mutate(across(everything(), ~ replace_na(.x, 0)))
  
  # Ajouter colonnes manquantes et réordonner
  for (col in setdiff(all_cols_1a, names(denomb_1a))) {
    denomb_1a[[col]] <- 0
  }
  
  denomb_1a <- denomb_1a[all_cols_1a]
  rownames(denomb_1a) <- paste0("Nb élèves 1A sur ", i)
  
  for (col in setdiff(all_cols_2a, names(denomb_2a))) {
    denomb_2a[[col]] <- 0
  }
  denomb_2a <- denomb_2a[all_cols_2a]
  rownames(denomb_2a) <- paste0("Nb élèves 2A sur ", i)
  
  stock_denomb_1a <- rbind(stock_denomb_1a, denomb_1a)
  stock_denomb_2a <- rbind(stock_denomb_2a, denomb_2a)
}

stock_denomb_1a <- stock_denomb_1a[-1,]
stock_denomb_2a <- stock_denomb_2a[-1,]


stock_denomb_2a



########## resultats eleves sur 10 ans#########################################
########## resultats eleves sur 10 ans#########################################


stock_moy_g_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_moy_g_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_moy_g_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_moy_g_2a) <-  c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


stock_moy_stat_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_moy_stat_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_moy_stat_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_moy_stat_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


stock_moy_eco_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_moy_eco_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_moy_eco_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_moy_eco_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


stock_moy_info_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_moy_info_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_moy_info_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_moy_info_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


stock_moy_hum_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_moy_hum_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_moy_hum_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_moy_hum_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")




all_specialites_1a <- c("AST 1A","But-Stid", 
                     "Eco BL","Eco D2","ERASMUS - Contractuels" , "Interne", "Mathématiques", "MP", 
                     "MPI", "PC", "PSI")


all_specialites_2a <- c("AST 1A", "AST 2A", "But-Stid", 
                     "Eco BL", "Eco D2", "ERASMUS - Contractuels", 
                     "Interne", "Mathématiques", "MP", 
                     "MPI", "PC", "PSI")


for (i in unique(bdd_1a2a_net$bloc_an)) {
  #  i <- "2021-2022"
  bdd_1a2a_net_an <- bdd_1a2a_net[bdd_1a2a_net$bloc_an == i, ]


denomb_1a <- bdd_1a2a_net_an %>%
  filter( annee_ecole == "1A") %>%
  group_by(spe_entree) %>%
  summarise(
    moy_g    = round(mean(moyenne_generale, na.rm = TRUE), 1),
    moy_stat = round(mean(c(mean(MSS1, na.rm = TRUE), mean(MSS2, na.rm = TRUE)), na.rm = TRUE), 1),
    moy_eco  = round(mean(c(mean(MES1, na.rm = TRUE), mean(MES2, na.rm = TRUE)), na.rm = TRUE), 1),
    moy_info = round(mean(c(mean(MIS1, na.rm = TRUE), mean(MIS2, na.rm = TRUE)), na.rm = TRUE), 1),
    moy_hum  = round(mean(c(mean(MHS1, na.rm = TRUE), mean(MHS2, na.rm = TRUE)), na.rm = TRUE), 1)
  ) %>%
  # Ajouter les spécialités manquantes
  complete(spe_entree = all_specialites_1a, fill = list(
    moy_g = 0, moy_stat = 0, moy_eco = 0, moy_info = 0, moy_hum = 0
  )) %>%
  # Garder l'ordre de all_specialites
  mutate(spe_entree = factor(spe_entree, levels = all_specialites_1a)) %>%
  arrange(spe_entree)



denomb_2a <- bdd_1a2a_net_an%>%
  filter(annee_ecole == "2A") %>%
  group_by(spe_entree) %>%
  summarise(
    moy_g    = round(mean(moyenne_generale, na.rm = TRUE), 1),
    moy_stat = round(mean(c(mean(MSS1, na.rm = TRUE), mean(MSS2, na.rm = TRUE)), na.rm = TRUE), 1),
    moy_eco  = round(mean(c(mean(MES1, na.rm = TRUE), mean(MES2, na.rm = TRUE)), na.rm = TRUE), 1),
    moy_info = round(mean(c(mean(MIS1, na.rm = TRUE), mean(MIS2, na.rm = TRUE)), na.rm = TRUE), 1),
    moy_hum  = round(mean(c(mean(MHS1, na.rm = TRUE), mean(MHS2, na.rm = TRUE)), na.rm = TRUE), 1)
  ) %>%
  # Ajouter les spécialités manquantes
  complete(spe_entree = all_specialites_2a, fill = list(
    moy_g = 0, moy_stat = 0, moy_eco = 0, moy_info = 0, moy_hum = 0
  )) %>%
  # Garder l'ordre de all_specialites
  mutate(spe_entree = factor(spe_entree, levels = all_specialites_2a)) %>%
  arrange(spe_entree)


denomb_1a <- as.data.frame(t(denomb_1a))
colnames(denomb_1a) <- denomb_1a[1,]
denomb_1a_mg <- as.data.frame(denomb_1a[2,])
rownames(denomb_1a_mg)[1]  <- paste0("Moyenne des moyennes générales 1A sur ", i)
denomb_1a_stat<- as.data.frame(denomb_1a[3,])
rownames(denomb_1a_stat)[1]  <- paste0("Moyenne des moyennes en UE STAT 1A sur ",i)
denomb_1a_eco <- as.data.frame(denomb_1a[4,])
rownames(denomb_1a_eco)[1]  <- paste0("Moyenne des moyennes en UE ECO 1A sur ",i)
denomb_1a_info<- as.data.frame(denomb_1a[5,])
rownames(denomb_1a_info)[1]  <- paste0("Moyenne des moyennes en UE INFO 1A sur ",i)
denomb_1a_hum <- as.data.frame(denomb_1a[6,])
rownames(denomb_1a_hum)[1]  <- paste0("Moyenne des moyennes en UE HUMANITES 1A sur ",i)

denomb_2a <- as.data.frame(t(denomb_2a))
colnames(denomb_2a) <- denomb_2a[1,]
denomb_2a_mg <- as.data.frame(denomb_2a[2,])
rownames(denomb_2a_mg)[1]  <- paste0("Moyenne des moyennes générales 2A sur ", i)
denomb_2a_stat<- as.data.frame(denomb_2a[3,])
rownames(denomb_2a_stat)[1]  <- paste0("Moyenne des moyennes en UE STAT 2A sur ",i)
denomb_2a_eco <- as.data.frame(denomb_2a[4,])
rownames(denomb_2a_eco)[1]  <- paste0("Moyenne des moyennes en UE ECO 2A sur ",i)
denomb_2a_info<- as.data.frame(denomb_2a[5,])
rownames(denomb_2a_info)[1]  <- paste0("Moyenne des moyennes en UE INFO 2A sur ",i)
denomb_2a_hum <- as.data.frame(denomb_2a[6,])
rownames(denomb_2a_hum)[1]  <- paste0("Moyenne des moyennes en UE HUMANITES 2A sur ",i)

stock_moy_g_1a <- rbind(stock_moy_g_1a,denomb_1a_mg)
stock_moy_g_2a <- rbind(stock_moy_g_2a,denomb_2a_mg)
stock_moy_stat_1a <- rbind(stock_moy_stat_1a,denomb_1a_stat)
stock_moy_stat_2a <- rbind(stock_moy_stat_2a,denomb_2a_stat)
stock_moy_eco_1a <- rbind(stock_moy_eco_1a,denomb_1a_eco)
stock_moy_eco_2a <- rbind(stock_moy_eco_2a,denomb_2a_eco)
stock_moy_info_1a <- rbind(stock_moy_info_1a,denomb_1a_info)
stock_moy_info_2a <- rbind(stock_moy_info_2a,denomb_2a_info)
stock_moy_hum_1a <- rbind(stock_moy_hum_1a,denomb_1a_hum)
stock_moy_hum_2a <- rbind(stock_moy_hum_2a,denomb_2a_hum)
}

stock_moy_g_1a <- stock_moy_g_1a[-1,]
stock_moy_g_2a <- stock_moy_g_2a[-1,]
stock_moy_stat_1a <- stock_moy_stat_1a[-1,]
stock_moy_stat_2a <- stock_moy_stat_2a[-1,]
stock_moy_eco_1a <- stock_moy_eco_1a[-1,]
stock_moy_eco_2a <- stock_moy_eco_2a[-1,]
stock_moy_info_1a <- stock_moy_info_1a[-1,]
stock_moy_info_2a <- stock_moy_info_2a[-1,]
stock_moy_hum_1a <- stock_moy_hum_1a[-1,]
stock_moy_hum_2a <- stock_moy_hum_2a[-1,]


typeof(stock_moy_stat_1a)


#  part eleves moins de 12 de moyenne

#  part eleves moins de 12 de moyenne

bdd_1a2a_net$p12_g <- ifelse(bdd_1a2a_net$moyenne_generale<12,
                             1,
                             0)
bdd_1a2a_net$p12_stat <- ifelse((bdd_1a2a_net$MSS1+bdd_1a2a_net$MSS2)/2<12,
                                1,
                                0)
bdd_1a2a_net$p12_eco <- ifelse((bdd_1a2a_net$MES1+bdd_1a2a_net$MES2)/2<12,
                               1,
                               0)
bdd_1a2a_net$p12_info <- ifelse((bdd_1a2a_net$MIS1+bdd_1a2a_net$MIS2)/2<12,
                                1,
                                0)
bdd_1a2a_net$p12_hum <- ifelse((bdd_1a2a_net$MHS1+bdd_1a2a_net$MHS2)/2<12,
                               1,
                               0)


stock_p12_g_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_p12_g_1a) <-  c("AST 1A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_p12_g_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_p12_g_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


stock_p12_stat_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_p12_stat_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_p12_stat_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_p12_stat_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


stock_p12_eco_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_p12_eco_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_p12_eco_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_p12_eco_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


stock_p12_info_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_p12_info_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_p12_info_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_p12_info_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


stock_p12_hum_1a <- as.data.frame(matrix(NA,1,11))
colnames(stock_p12_hum_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")

stock_p12_hum_2a <- as.data.frame(matrix(NA,1,12))
colnames(stock_p12_hum_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques","MP","MPI","PC","PSI")


for (i in unique(bdd_1a2a_net$bloc_an)) {
  #  i <- "2021-2022"
  bdd_1a2a_net_an <- bdd_1a2a_net[bdd_1a2a_net$bloc_an == i, ]
  
  
  denomb_1a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "1A") %>%
    group_by(spe_entree) %>%
    summarise(p12_g = sum(p12_g,na.rm = T),
              p12_stat = sum(p12_stat,na.rm = T),
              p12_eco = sum(p12_eco,na.rm = T),
              p12_info = sum(p12_info,na.rm = T),
              p12_hum =sum(p12_hum,na.rm = T))%>%
    # Ajouter les spécialités manquantes
    complete(spe_entree = all_specialites_1a, fill = list(
      p12_g = 0, p12_stat = 0, p12_eco = 0, p12_info = 0, p12_hum = 0
    )) %>%
    # Garder l'ordre de all_specialites
    mutate(spe_entree = factor(spe_entree, levels = all_specialites_1a)) %>%
    arrange(spe_entree)
  
  
  denomb_2a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "2A") %>%
    group_by(spe_entree) %>%
    summarise(p12_g = sum(p12_g,na.rm = T),
              p12_stat = sum(p12_stat,na.rm = T),
              p12_eco = sum(p12_eco,na.rm = T),
              p12_info = sum(p12_info,na.rm = T),
              p12_hum =sum(p12_hum,na.rm = T))%>%
    # Ajouter les spécialités manquantes
    complete(spe_entree = all_specialites_2a, fill = list(
      p12_g = 0, p12_stat = 0, p12_eco = 0, p12_info = 0, p12_hum = 0
    )) %>%
    # Garder l'ordre de all_specialites
    mutate(spe_entree = factor(spe_entree, levels = all_specialites_2a)) %>%
    arrange(spe_entree)
  
  
  denomb_1a <- as.data.frame(t(denomb_1a))
  colnames(denomb_1a) <- denomb_1a[1,]
  denomb_1a_mg <- as.data.frame(denomb_1a[2,])
  rownames(denomb_1a_mg)[1]  <- paste0("Part des élèves avec une moyenne générale en 1A < 12 sur ", i)
  denomb_1a_stat<- as.data.frame(denomb_1a[3,])
  rownames(denomb_1a_stat)[1]  <- paste0("Part des élèves avec une moyenne en UE STAT en 1A < 12 sur ",i)
  denomb_1a_eco <- as.data.frame(denomb_1a[4,])
  rownames(denomb_1a_eco)[1]  <- paste0("Part des élèves avec une moyenne en UE ECO en 1A < 12 sur ",i)
  denomb_1a_info<- as.data.frame(denomb_1a[5,])
  rownames(denomb_1a_info)[1]  <- paste0("Part des élèves avec une moyenne en UE INFO en 1A < 12 sur ",i)
  denomb_1a_hum <- as.data.frame(denomb_1a[6,])
  rownames(denomb_1a_hum)[1]  <- paste0("Part des élèves avec une moyenne en UE HUMANITES en 1A < 12 sur ",i)
  
  denomb_2a <- as.data.frame(t(denomb_2a))
  colnames(denomb_2a) <- denomb_2a[1,]
  denomb_2a_mg <- as.data.frame(denomb_2a[2,])
  rownames(denomb_2a_mg)[1]  <- paste0("Part des élèves avec une moyenne générale en 2A < 12 sur ", i)
  denomb_2a_stat<- as.data.frame(denomb_2a[3,])
  rownames(denomb_2a_stat)[1]  <- paste0("Part des élèves avec une moyenne en UE STAT en 2A < 12 sur ",i)
  denomb_2a_eco <- as.data.frame(denomb_2a[4,])
  rownames(denomb_2a_eco)[1]  <- paste0("Part des élèves avec une moyenne en UE ECO en 2A < 12 sur ",i)
  denomb_2a_info<- as.data.frame(denomb_2a[5,])
  rownames(denomb_2a_info)[1]  <- paste0("Part des élèves avec une moyenne en UE INFO en 2A < 12 sur ",i)
  denomb_2a_hum <- as.data.frame(denomb_2a[6,])
  rownames(denomb_2a_hum)[1]  <- paste0("Part des élèves avec une moyenne en UE HUMANITES en 2A < 12 sur ",i)
  
  stock_p12_g_1a <- rbind(stock_p12_g_1a,denomb_1a_mg)
  stock_p12_g_2a <- rbind(stock_p12_g_2a,denomb_2a_mg)
  stock_p12_stat_1a <- rbind(stock_p12_stat_1a,denomb_1a_stat)
  stock_p12_stat_2a <- rbind(stock_p12_stat_2a,denomb_2a_stat)
  stock_p12_eco_1a <- rbind(stock_p12_eco_1a,denomb_1a_eco)
  stock_p12_eco_2a <- rbind(stock_p12_eco_2a,denomb_2a_eco)
  stock_p12_info_1a <- rbind(stock_p12_info_1a,denomb_1a_info)
  stock_p12_info_2a <- rbind(stock_p12_info_2a,denomb_2a_info)
  stock_p12_hum_1a <- rbind(stock_p12_hum_1a,denomb_1a_hum)
  stock_p12_hum_2a <- rbind(stock_p12_hum_2a,denomb_2a_hum)
}

stock_p12_g_1a <- stock_p12_g_1a[-1,]
stock_p12_g_2a <- stock_p12_g_2a[-1,]
stock_p12_stat_1a <- stock_p12_stat_1a[-1,]
stock_p12_stat_2a <- stock_p12_stat_2a[-1,]
stock_p12_eco_1a <- stock_p12_eco_1a[-1,]
stock_p12_eco_2a <- stock_p12_eco_2a[-1,]
stock_p12_info_1a <- stock_p12_info_1a[-1,]
stock_p12_info_2a <- stock_p12_info_2a[-1,]
stock_p12_hum_1a <- stock_p12_hum_1a[-1,]
stock_p12_hum_2a <- stock_p12_hum_2a[-1,]

for (i in 1:4) {
  # i<-3
  for (j in 1:11) {
    # j<-2
    stock_p12_g_1a[i,j]<-round(as.numeric(stock_p12_g_1a[i,j])/as.numeric(stock_denomb_1a[i,j])*100,1)
    stock_p12_stat_1a[i,j]<-round(as.numeric(stock_p12_stat_1a[i,j])/as.numeric(stock_denomb_1a[i,j])*100,1)
    stock_p12_eco_1a[i,j]<-round(as.numeric(stock_p12_eco_1a[i,j])/as.numeric(stock_denomb_1a[i,j])*100,1)
    stock_p12_info_1a[i,j]<-round(as.numeric(stock_p12_info_1a[i,j])/as.numeric(stock_denomb_1a[i,j])*100,1)
    stock_p12_hum_1a[i,j]<-round(as.numeric(stock_p12_hum_1a[i,j])/as.numeric(stock_denomb_1a[i,j])*100,1)
    
  }
}

for (i in 1:4) {
  # i<-1
  for (j in 1:12) {
    # j<-8
    stock_p12_g_2a[i,j]<-round(as.numeric(stock_p12_g_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
    stock_p12_stat_2a[i,j]<-round(as.numeric(stock_p12_stat_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
    stock_p12_eco_2a[i,j]<-round(as.numeric(stock_p12_eco_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
    stock_p12_info_2a[i,j]<-round(as.numeric(stock_p12_info_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
    stock_p12_hum_2a[i,j]<-round(as.numeric(stock_p12_hum_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
  }
}


stock_p12_g_2a



######Étudier la réussite aux matières mathématiques en fonction de la filière d'origine / mode d'intégration###
bdd_spécial_mat <- bdd_1a2a_net %>%
  filter(!is.na(MSS1)) %>%   # on enlève les NA
  mutate(spe_entree = factor(spe_entree))

ggplot(bdd_spécial_mat, aes(x = spe_entree, y = MSS1, fill = spe_entree)) +
  geom_boxplot() +
  labs(title = "Réussite en mathématiques du semestre 1 selon la filière d'origine",
       x = "Filière d'origine",
       y = "Note en mathématiques") +
  theme_minimal()

anova_filiere <- aov(MSS1 ~ spe_entree, data = bdd_spécial_mat)
summary(anova_filiere)
TukeyHSD(anova_filiere)




bdd_spécial_mat_periode <- bdd_1a2a_net %>%
  filter(!is.na(MSS1)) %>% 
  mutate(across(c(spe_entree, bloc_an), as.factor))


ggplot(bdd_spécial_mat_periode, aes(x = spe_entree, y = MSS1, fill = bloc_an)) +
  geom_boxplot() +
  labs(title = "Réussite en mathématiques selon la filière d'origine",
       x = "Filière d'origine",
       y = "Note en mathématiques") +
  theme_minimal()

#analyser les résultats selon ces filières d'origine



# 1. PRÉPARATION DES DONNÉES MATHÉMATIQUES

donnees_maths <- bdd_1a2a_net %>%
  select(annee_ecole, MSS1, MSS2,MIS1,MIS2 ,
         sexe,spe_entree, moyenne_generale, AV, 
         bac_mention, paysnai, redoublement,bloc_an) %>%
  mutate(
    # Calcul des moyennes par domaine mathématique
    MSS_moyenne = ifelse(!is.na(MSS1) & !is.na(MSS2), (MSS1 + MSS2) / 2, 
                         ifelse(!is.na(MSS1), MSS1, MSS2)),
    
    # Évolution entre semestres
    MSS_evolution = MSS2 - MSS1,
    
    # Indicateurs de réussite (seuil à 10/20)
    MSS_reussi = case_when(
      MSS_moyenne >= 10 ~ "Réussi",
      MSS_moyenne < 10 ~ "Échec",
      TRUE ~ "Non évalué"
      
    ),
    # Niveau de performance
    niveau_maths = case_when(
      MSS_moyenne >= 16 ~ "Excellent",
      MSS_moyenne >= 14 ~ "Très bien",
      MSS_moyenne >= 12 ~ "Bien",
      MSS_moyenne >= 10 ~ "Assez bien",
      MSS_moyenne < 10 ~ "Insuffisant",
      TRUE ~ "Non évalué"
    )
  )


# 2. STATISTIQUES DESCRIPTIVES PAR ANNÉE ET SEMESTRE
stats_maths_annee <- donnees_maths %>%
  group_by(annee_ecole,bloc_an) %>%
  summarise(
    n_etudiants = n(),
    # Statistiques MSS (Math/Stats/Sciences)
    MSS1_moy = round(mean(MSS1, na.rm = TRUE), 2),
    MSS2_moy = round(mean(MSS2, na.rm = TRUE), 2),
    MSS_evolution_moy = round(mean(MSS_evolution, na.rm = TRUE), 2),
    MSS_taux_reussite = round(mean(MSS_moyenne >= 10, na.rm = TRUE) * 100, 1)
  )

cat("\n=== STATISTIQUES PAR ANNÉE D'ÉCOLE ===\n")
print(stats_maths_annee)


# 3. ANALYSE DE LA PROGRESSION ENTRE SEMESTRES
progression_analyse <- donnees_maths %>%
  filter(!is.na(MSS1) & !is.na(MSS2)) %>%
  group_by(annee_ecole,bloc_an) %>%
  summarise(
    n = n(),
    # Tests de progression
    MSS_progression = mean(MSS2 > MSS1, na.rm = TRUE) * 100,
    
  )

cat("\n=== ANALYSE DE LA PROGRESSION ENTRE SEMESTRES ===\n")
print(progression_analyse)

# 4. ANALYSE PAR PROFIL D'ÉTUDIANT
# Par sexe
analyse_sexe <- donnees_maths %>%
  filter(!is.na(sexe)) %>%
  group_by(annee_ecole, sexe,bloc_an) %>%
  summarise(
    n = n(),
    MSS_moyenne = round(mean(MSS_moyenne, na.rm = TRUE), 2),
    # Tests de progression
    MSS_progression = mean(MSS2 > MSS1, na.rm = TRUE) * 100,
    
    # Ampleur moyenne de la progression
    MSS_gain_moyen = round(mean(MSS_evolution, na.rm = TRUE), 2)
  )

cat("\n=== ANALYSE PAR SEXE ===\n")
print(analyse_sexe)



# Par filière d'origine
if(!all(is.na(donnees_maths$spe_entree))) {
  analyse_concours <- donnees_maths %>%
    filter(!is.na(spe_entree)) %>%
    group_by(annee_ecole, spe_entree,bloc_an) %>%
    summarise(
      n = n(),
      MSS_moyenne = round(mean(MSS_moyenne, na.rm = TRUE), 2),
      .groups = 'drop'
    ) %>%
    filter(n >= 5)  # Garder seulement les groupes avec au moins 5 étudiants
  
  cat("\n=== ANALYSE PAR FILIERE D'ORIGINE ===\n")
  print(analyse_concours)
}


# 5. ANALYSE PAR MENTION AU BAC
if(!all(is.na(donnees_maths$bac_mention))) {
  analyse_bac_mention <- donnees_maths %>%
    filter(!is.na(bac_mention)) %>%
    group_by(annee_ecole, bac_mention,bloc_an) %>%
    summarise(
      n = n(),
      MSS_moyenne = round(mean(MSS_moyenne, na.rm = TRUE), 2),
      MSS_gain_moyen = round(mean(MSS_evolution, na.rm = TRUE), 2),
      .groups = 'drop'
    ) %>%
    filter(n >= 3)  # Garder seulement les groupes avec au moins 3 étudiants
  
  cat("\n=== ANALYSE PAR MENTION AU BAC ===\n")
  print(analyse_bac_mention)
}

# 6 ANALYSE DES ÉTUDIANTS EN DIFFICULTÉ
etudiants_difficulte <- donnees_maths %>%
  filter(MSS_moyenne < 10, !is.na(MSS_moyenne)) %>%
  group_by(annee_ecole,bloc_an) %>%
  summarise(
    n_echec = n(),
    MSS_moyenne_echec = round(mean(MSS_moyenne, na.rm = TRUE), 2),
    pourcentage_progression = round(mean(MSS2 > MSS1, na.rm = TRUE) * 100, 1),
    gain_moyen_echec = round(mean(MSS_evolution, na.rm = TRUE), 2),
    .groups = 'drop'
  )

cat("\n=== ANALYSE DES ÉTUDIANTS EN DIFFICULTÉ (< 10) ===\n")
print(etudiants_difficulte)





# 7. VISUALISATIONS

# Graphique 1: Distribution des notes MSS par année,par blocs  et semestre
p1 <- donnees_maths %>%
  select(annee_ecole, MSS1, MSS2, bloc_an) %>%
  pivot_longer(cols = c(MSS1, MSS2), names_to = "semestre", values_to = "note") %>%
  filter(!is.na(note)) %>%
  ggplot(aes(x = annee_ecole, y = note, fill = semestre)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("MSS1" = "#3498db", "MSS2" = "#e74c3c"),
                    labels = c("MSS1" = "Semestre 1", "MSS2" = "Semestre 2")) +
  labs(title = "Distribution des notes MSS par année, semestre et bloc",
       x = "Année d'école", y = "Note MSS", fill = "Semestre") +
  facet_wrap(~ bloc_an) +   # ➡️ séparation par bloc
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p1)

#TEST POUR COMPARER SEMESTRE 1 ET SEMESTRE 2

donnees_long <- donnees_maths %>%
  pivot_longer(cols = c(MSS1, MSS2),
               names_to = "semestre",
               values_to = "note") %>%
  filter(!is.na(note))   # enlever les manquants


anova_model <- aov(note ~ semestre * bloc_an, data = donnees_long)
summary(anova_model)

#Il existe une différence significative entre les semestres (p=8.32e-13), le semestre 2 ayant des notes plus élevées en moyenne.en moyenne, les notes de MSS sont significativement plus hautes au semestre 2 qu’au semestre 1.
#Les résultats évoluent aussi significativement selon les blocs d’années (2.84e-13), indiquant une amélioration globale au fil du temps.les performances changent selon les périodes (meilleures dans les blocs récents).
#Enfin, l’interaction est significative (p=6.01e-05), ce qui montre que l’écart entre semestre 1 et semestre 2 varie selon les périodes : faible avant 2020, plus marqué après 2022.la différence entre S1 et S2 n’est pas la même selon les périodes.




# Graphique 2: Taux de réussite par année
p2 <- stats_maths_annee %>%
  ggplot(aes(x = annee_ecole, y = MSS_taux_reussite)) +
  geom_col(fill = "#27ae60", alpha = 0.8, color = "black") +
  geom_text(aes(label = paste0(MSS_taux_reussite, "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "Taux de réussite en mathématiques par année d'école",
       x = "Année d'école", y = "Taux de réussite (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  coord_cartesian(ylim = c(0, 100))

print(p2)

# Graphique 3: Évolution entre semestres
p3 <- donnees_maths %>%
  filter(!is.na(MSS_evolution)) %>%
  ggplot(aes(x = MSS_evolution)) +
  geom_histogram(bins = 30, fill = "#f39c12", alpha = 0.7, color = "black") +
  facet_grid(bloc_an ~ annee_ecole, scales = "free_y") +  # bloc_an en lignes, année en colonnes
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Distribution de l'évolution des notes MSS (S2 - S1)",
       x = "Évolution (points)", y = "Nombre d'étudiants") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p3)




# Graphique 4: Performance par niveau
niveau_data <- donnees_maths %>%
  filter(!is.na(niveau_maths)) %>%
  count(annee_ecole, niveau_maths) %>%
  group_by(annee_ecole) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

p4 <- ggplot(niveau_data, aes(x = annee_ecole, y = pourcentage, fill = niveau_maths)) +
  geom_col() +
  scale_fill_manual(values = c("Excellent" = "#27ae60", "Très bien" = "#2ecc71", 
                               "Bien" = "#f39c12", "Assez bien" = "#e67e22", 
                               "Insuffisant" = "#e74c3c", "Non évalué" = "#95a5a6"),
                    name = "Niveau") +
  labs(title = "Répartition des niveaux de performance par année",
       x = "Année d'école", y = "Pourcentage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "bottom")

print(p4)

# Graphique 5: Comparaison par sexe (si données disponibles)
if(!all(is.na(donnees_maths$sexe))) {
  p5 <- donnees_maths %>%
    filter(!is.na(sexe), !is.na(MSS_moyenne)) %>%
    ggplot(aes(x = sexe, y = MSS_moyenne, fill = sexe)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~annee_ecole) +
    scale_fill_manual(values = c("Homme" = "#3498db", "Femme" = "#e74c3c")) +
    labs(title = "Performance en mathématiques par sexe et année",
         x = "Sexe", y = "Moyenne MSS", fill = "Sexe") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  print(p5)
}

# Graphique 6: Corrélation MSS1 vs MSS2
p6 <- donnees_maths %>%
  filter(!is.na(MSS1), !is.na(MSS2)) %>%
  ggplot(aes(x = MSS1, y = MSS2)) +
  geom_point(alpha = 0.6, color = "#3498db") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~annee_ecole) +
  labs(title = "Corrélation entre les notes MSS1 et MSS2",
       x = "Note MSS Semestre 1", y = "Note MSS Semestre 2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p6)


#effet du niveau d'anglais sur les résultats scolaires



donnees_anglais <- bdd_1a2a_net %>%
  select(annee_ecole, certif_anglais_org, certif_anglais_score, 
         moyenne_generale, AV, sexe, filiere_1A, concours_origine,
         bac_mention, paysnai, nationalite, redoublement,spe_entree,bloc_an) %>%
  mutate(
    # Nettoyage du score TOEIC
    score_anglais = case_when(
      !is.na(certif_anglais_score) & certif_anglais_score != "" ~ 
        as.numeric(certif_anglais_score),
      TRUE ~ NA_real_
    ),
    
    # Indicateur de certification
    a_certification = case_when(
      !is.na(certif_anglais_org) & certif_anglais_org != "" ~ "Oui",
      TRUE ~ "Non"
    ),
    
    # Catégorisation du niveau TOEIC
    niveau_toeic = case_when(
      is.na(score_anglais) ~ "Non renseigné",
      score_anglais >= 900 ~ "Excellent (≥900)",
      score_anglais >= 800 ~ "Très bien (800-899)",
      score_anglais >= 700 ~ "Bien (700-799)",
      score_anglais >= 600 ~ "Correct (600-699)",
      score_anglais < 600 ~ "Insuffisant (<600)",
      TRUE ~ "Non renseigné"
    ),
    
    # Indicateur d'étudiant étranger
    etudiant_etranger = case_when(
      nationalite != "Français" | paysnai != "FRANCE" ~ "Étranger",
      TRUE ~ "Français"
    ),
    
    # Indicateur de réussite
    reussite = case_when(
      AV == "1" ~ "Réussite",
      AV == "0" ~ "Échec",
      TRUE ~ "Non renseigné"
    ),
    
    # Niveau de performance générale
    niveau_performance = case_when(
      moyenne_generale >= 16 ~ "Excellent",
      moyenne_generale >= 14 ~ "Très bien",
      moyenne_generale >= 12 ~ "Bien",
      moyenne_generale >= 10 ~ "Assez bien",
      moyenne_generale < 10 ~ "Insuffisant",
      TRUE ~ "Non évalué"
    )
  )

# Aperçu des données
cat("=== APERÇU DES DONNÉES ANGLAIS ===\n")
cat("Nombre total d'observations:", nrow(donnees_anglais), "\n")
cat("Étudiants avec certification anglais:", sum(donnees_anglais$a_certification == "Oui", na.rm = TRUE), "\n")
cat("Scores TOEIC disponibles:", sum(!is.na(donnees_anglais$score_anglais)), "\n")

# ===============================
# 2. STATISTIQUES DESCRIPTIVES
# ===============================

# Distribution des certifications
distrib_certif <- donnees_anglais %>%
  count(annee_ecole, certif_anglais_org) %>%
  filter(!is.na(certif_anglais_org)) %>%
  arrange(annee_ecole, desc(n))

cat("\n=== DISTRIBUTION DES CERTIFICATIONS ===\n")
print(distrib_certif)

# Statistiques des scores TOEIC par année
stats_toeic_annee <- donnees_anglais %>%
  filter(!is.na(score_anglais)) %>%
  group_by(annee_ecole,bloc_an) %>%
  summarise(
    n_scores = n(),
    score_moyen = round(mean(score_anglais), 0),
    score_median = round(median(score_anglais), 0),
    score_min = min(score_anglais),
    score_max = max(score_anglais),
    ecart_type = round(sd(score_anglais), 0),
    .groups = 'drop'
  )

cat("\n=== STATISTIQUES SCORES TOEIC PAR ANNÉE ===\n")
print(stats_toeic_annee)

# ===============================
# 3. ANALYSE DE L'IMPACT SUR LA NOTE GÉNÉRALE
# ===============================

# Comparaison moyenne générale selon la certification
impact_certification <- donnees_anglais %>%
  group_by(annee_ecole, a_certification) %>%
  summarise(
    n = n(),
    moyenne_gen = round(mean(moyenne_generale, na.rm = TRUE), 2),
    mediane_gen = round(median(moyenne_generale, na.rm = TRUE), 2),
    taux_reussite = round(mean(AV == "1", na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )

cat("\n=== IMPACT DE LA CERTIFICATION SUR LA PERFORMANCE ===\n")
print(impact_certification)

# Analyse par niveau de TOEIC
impact_niveau_toeic <- donnees_anglais %>%
  filter(!is.na(score_anglais)) %>%
  group_by(annee_ecole, niveau_toeic,bloc_an) %>%
  summarise(
    n = n(),
    moyenne_gen = round(mean(moyenne_generale, na.rm = TRUE), 2),
    taux_reussite = round(mean(AV == "1", na.rm = TRUE) * 100, 1),
    score_toeic_moyen = round(mean(score_anglais), 0),
    .groups = 'drop'
  ) %>%
  filter(n >= 3)

cat("\n=== IMPACT DU NIVEAU TOEIC SUR LA PERFORMANCE ===\n")
print(impact_niveau_toeic)

# ===============================
# 4. ANALYSE PAR PROFIL D'ÉTUDIANT
# ===============================

# Impact par sexe
analyse_sexe_anglais <- donnees_anglais %>%
  filter(!is.na(sexe), !is.na(score_anglais)) %>%
  group_by(annee_ecole, sexe) %>%
  summarise(
    n = n(),
    score_toeic_moyen = round(mean(score_anglais), 0),
    moyenne_gen = round(mean(moyenne_generale, na.rm = TRUE), 2),
    .groups = 'drop'
  )

cat("\n=== ANALYSE PAR SEXE ===\n")
print(analyse_sexe_anglais)

# Impact par spécialité d'entrée
analyse_filiere_anglais <- donnees_anglais %>%
  filter(!is.na(spe_entree), !is.na(score_anglais)) %>%
  group_by(annee_ecole, spe_entree) %>%
  summarise(
    n = n(),
    score_toeic_moyen = round(mean(score_anglais), 0),
    moyenne_gen = round(mean(moyenne_generale, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  filter(n >= 3)

cat("\n=== ANALYSE PAR SPE_ENTREE ===\n")
print(analyse_filiere_anglais)

# Impact selon l'origine (français vs étranger)
analyse_origine <- donnees_anglais %>%
  filter(!is.na(score_anglais)) %>%
  group_by(annee_ecole, etudiant_etranger) %>%
  summarise(
    n = n(),
    score_toeic_moyen = round(mean(score_anglais), 0),
    moyenne_gen = round(mean(moyenne_generale, na.rm = TRUE), 2),
    taux_reussite = round(mean(AV == "1", na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )

cat("\n=== ANALYSE PAR ORIGINE ===\n")
print(analyse_origine)

# Graphique 1: Distribution des scores TOEIC par année
p1 <- donnees_anglais %>%
  filter(!is.na(score_anglais)) %>%
  ggplot(aes(x = annee_ecole, y = score_anglais, fill = annee_ecole)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 0.8) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(title = "Distribution des scores TOEIC par année d'école",
       x = "Année d'école", y = "Score TOEIC", fill = "Année") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p1)

# Graphique 2: Relation score TOEIC vs moyenne générale
p2 <- donnees_anglais %>%
  filter(!is.na(score_anglais), !is.na(moyenne_generale)) %>%
  ggplot(aes(x = score_anglais, y = moyenne_generale)) +
  geom_point(aes(color = annee_ecole), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 1) +
  facet_wrap(~annee_ecole) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Relation entre score TOEIC et moyenne générale",
       x = "Score TOEIC", y = "Moyenne générale", color = "Année") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p2)


# Graphique 3: Moyenne générale par niveau TOEIC
niveau_order <- c("Insuffisant (<600)", "Correct (600-699)", "Bien (700-799)", 
                  "Très bien (800-899)", "Excellent (≥900)")

p3 <- donnees_anglais %>%
  filter(!is.na(score_anglais), niveau_toeic != "Non renseigné") %>%
  mutate(niveau_toeic = factor(niveau_toeic, levels = niveau_order)) %>%
  ggplot(aes(x = niveau_toeic, y = moyenne_generale, fill = niveau_toeic)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~annee_ecole) +
  scale_fill_manual(values = c("#e74c3c", "#f39c12", "#f1c40f", "#2ecc71", "#27ae60")) +
  labs(title = "Moyenne générale par niveau TOEIC",
       x = "Niveau TOEIC", y = "Moyenne générale", fill = "Niveau TOEIC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p3)





# Graphique 4: Comparaison avec/sans certification
p4 <- donnees_anglais %>%
  filter(!is.na(moyenne_generale)) %>%
  ggplot(aes(x = a_certification, y = moyenne_generale, fill = a_certification)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  facet_wrap(~annee_ecole) +
  scale_fill_manual(values = c("Non" = "#95a5a6", "Oui" = "#3498db")) +
  labs(title = "Performance selon la présence d'une certification anglais",
       x = "Certification anglais", y = "Moyenne générale", 
       fill = "Certification") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p4)



# ===============================
# 6. CORRÉLATIONS ET TESTS STATISTIQUES
# ===============================

# Corrélation score TOEIC vs moyenne générale
correlation_analysis <- donnees_anglais %>%
  filter(!is.na(score_anglais), !is.na(moyenne_generale)) %>%
  group_by(annee_ecole) %>%
  summarise(
    n = n(),
    correlation = round(cor(score_anglais, moyenne_generale), 3),
    p_value = if(n() > 2) round(cor.test(score_anglais, moyenne_generale)$p.value, 4) else NA,
    significatif = case_when(
      is.na(p_value) ~ "Non testé",
      p_value < 0.001 ~ "*** (p < 0.001)",
      p_value < 0.01 ~ "** (p < 0.01)",
      p_value < 0.05 ~ "* (p < 0.05)",
      TRUE ~ "Non significatif"
    ),
    .groups = 'drop'
  )

cat("\n=== CORRÉLATION SCORE TOEIC - MOYENNE GÉNÉRALE ===\n")
print(correlation_analysis)

# Corrélation niveau TOEIC vs moyenne générale
correlation_analysis1 <- donnees_anglais %>%
  filter(!is.na(score_anglais),
         !is.na(moyenne_generale),
         niveau_toeic != "Non renseigné") %>%
  mutate(niveau_toeic = factor(niveau_toeic, levels = niveau_order),
         niveau_num = as.numeric(niveau_toeic)) %>%  # conversion en numérique
  group_by(annee_ecole) %>%
  summarise(
    n = n(),
    correlation = if (n() > 2) round(cor(niveau_num, moyenne_generale, method = "spearman"), 3) else NA,
    p_value = if (n() > 2) round(cor.test(niveau_num, moyenne_generale, method = "spearman")$p.value, 4) else NA,
    significatif = case_when(
      is.na(p_value) ~ "Non testé",
      p_value < 0.001 ~ "*** (p < 0.001)",
      p_value < 0.01 ~ "** (p < 0.01)",
      p_value < 0.05 ~ "* (p < 0.05)",
      TRUE ~ "Non significatif"
    ),
    .groups = "drop"
  )
cat("\n=== CORRÉLATION SCORE TOEIC - MOYENNE GÉNÉRALE ===\n")
print(correlation_analysis1)
#Les coefficients (~0.11) indiquent une relation très faible mais positive entre les deux variables.
#Autrement dit, les étudiants avec un meilleur score TOEIC ont tendance à avoir une meilleure moyenne générale, mais cette tendance est loin d’être forte.
#Les résultats mettent en évidence une corrélation positive mais faible entre le score TOEIC et la moyenne générale, aussi bien en 1A (r = 0.11, p < 0.01) qu’en 2A (r = 0.113, p < 0.001). Bien que statistiquement significative en raison du grand nombre d’observations, la force de la relation reste limitée : le score TOEIC n’explique qu’environ 1% de la variance des résultats académiques. Cela suggère que la maîtrise de l’anglais est associée aux performances générales, mais qu’elle n’en constitue pas un facteur déterminant.

# Corrélation niveau TOEIC vs moyenne générale par bloc année

correlation_analysis2 <- donnees_anglais %>%
  filter(!is.na(score_anglais),
         !is.na(moyenne_generale),
         niveau_toeic != "Non renseigné") %>%
  mutate(niveau_toeic = factor(niveau_toeic, levels = niveau_order)) %>%
  group_by(annee_ecole, bloc_an) %>%
  summarise(
    n = n(),
    correlation = if (n() > 2) round(cor(score_anglais, moyenne_generale, method = "pearson"), 3) else NA,
    p_value = if (n() > 2) round(cor.test(score_anglais, moyenne_generale, method = "pearson")$p.value, 4) else NA,
    significatif = case_when(
      is.na(p_value) ~ "Non testé",
      p_value < 0.001 ~ "*** (p < 0.001)",
      p_value < 0.01 ~ "** (p < 0.01)",
      p_value < 0.05 ~ "* (p < 0.05)",
      TRUE ~ "Non significatif"
    ),
    .groups = "drop"
  )
cat("\n=== CORRÉLATION SCORE TOEIC - MOYENNE GÉNÉRALE ===\n")
print(correlation_analysis2)
#L’analyse met en évidence une corrélation globalement faible entre score TOEIC et moyenne générale, variant selon les périodes et les années d’école. En 1A, la relation est le plus souvent non significative, ce qui suggère que la maîtrise de l’anglais n’est pas un facteur déterminant de la réussite académique au début du cursus. En revanche, en 2A, la corrélation devient significative dans certains blocs (notamment 2018–2020 avec r = 0.212, p < 0.001), indiquant qu’un meilleur score TOEIC est associé à une moyenne générale légèrement plus élevée. Toutefois, même dans ces cas, la part de variance expliquée reste faible (<5%), soulignant que la performance académique dépend principalement d’autres facteurs.