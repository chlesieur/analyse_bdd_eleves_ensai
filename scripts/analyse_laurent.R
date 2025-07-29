# Chargement des bibliothèques et du fichier
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(readxl)
library(arrow)

bdd <- read_parquet("data/bdd_2015_2024.parquet")
str(bdd)


# travaux sur 1A et 2A
bdd_1a2a <- bdd[bdd$annee_ecole %in% c("1A","2A"),]
# recoder spe_entree pour ECO
bdd_1a2a$spe_entree <- ifelse(bdd_1a2a$concours_origine =="Concours externe : spécialité 'économie et sciences sociales'",
                                  "Eco BL", 
                              ifelse(bdd_1a2a$concours_origine =="concours externe : spécialité 'économie et sciences sociales'",
                                                   "Eco BL",
                                  ifelse (bdd_1a2a$concours_origine == "Concours externe : spécialité 'économie et gestion'",
                                          "Eco D2",
                                          bdd_1a2a$specialite_entree)))

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

# denombrement eleves sur 10 ans
stock_denomb_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_denomb_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_denomb_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_denomb_2a) <-  c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


for (i in unique(bdd_1a2a_net$bloc_an)) {
  #  i <- "2021-2022"
  bdd_1a2a_net_an <- bdd_1a2a_net[bdd_1a2a_net$bloc_an == i, ]
  
  denomb_1a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "1A") %>%
    group_by(spe_entree) %>%
    summarise(nb_eleves_1a = n())
  
  denomb_2a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "2A") %>%
    group_by(spe_entree) %>%
    summarise(nb_eleves_2a = n())
  
  denomb_1a <- as.data.frame(t(denomb_1a))
  colnames(denomb_1a) <- denomb_1a[1,]
  denomb_1a <- as.data.frame(denomb_1a[2,])
  rownames(denomb_1a)[1]  <- paste0("Nb élèves 1A sur ", i)

  denomb_2a <- as.data.frame(t(denomb_2a))
  colnames(denomb_2a) <- denomb_2a[1,]
  denomb_2a <- as.data.frame(denomb_2a[2,])
  rownames(denomb_2a)[1]  <- paste0("Nb élèves 1A sur ", i)
  
  stock_denomb_1a <- rbind(stock_denomb_1a,denomb_1a)
  stock_denomb_2a <- rbind(stock_denomb_2a,denomb_2a)
}

stock_denomb_1a <- stock_denomb_1a[-1,]
stock_denomb_2a <- stock_denomb_2a[-1,]

write.csv2(stock_denomb_1a,"output/denomb_1a.csv")
write.csv2(stock_denomb_2a,"output/denomb_2a.csv")


# resultats eleves sur 10 ans
stock_moy_g_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_moy_g_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_moy_g_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_moy_g_2a) <-  c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


stock_moy_stat_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_moy_stat_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_moy_stat_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_moy_stat_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


stock_moy_eco_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_moy_eco_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_moy_eco_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_moy_eco_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


stock_moy_info_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_moy_info_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_moy_info_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_moy_info_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


stock_moy_hum_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_moy_hum_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_moy_hum_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_moy_hum_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")



for (i in unique(bdd_1a2a_net$bloc_an)) {
  #  i <- "2021-2022"
  bdd_1a2a_net_an <- bdd_1a2a_net[bdd_1a2a_net$bloc_an == i, ]
  
  denomb_1a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "1A") %>%
    group_by(spe_entree) %>%
    summarise(moy_g = round(mean(moyenne_generale),1),
              moy_stat = round((mean(MSS1,na.rm = T)+mean(MSS2,na.rm = T))/2,1),
              moy_eco = round((mean(MES1,na.rm = T)+mean(MES2,na.rm = T))/2,1),
              moy_info = round((mean(MIS1,na.rm = T)+mean(MIS2,na.rm = T))/2,1),
              moy_hum =round((mean(MHS1,na.rm = T)+mean(MHS2,na.rm = T))/2,1))
  
  denomb_2a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "2A") %>%
    group_by(spe_entree) %>%
    summarise(moy_g = round(mean(moyenne_generale),1),
              moy_stat = round((mean(MSS1,na.rm = T)+mean(MSS2,na.rm = T))/2,1),
              moy_eco = round((mean(MES1,na.rm = T)+mean(MES2,na.rm = T))/2,1),
              moy_info = round((mean(MIS1,na.rm = T)+mean(MIS2,na.rm = T))/2,1),
              moy_hum =round((mean(MHS1,na.rm = T)+mean(MHS2,na.rm = T))/2,1))
  
  
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

write.csv2(stock_moy_g_1a,"output/moy_g_1a.csv")
write.csv2(stock_moy_g_2a,"output/moy_g_2a.csv")
write.csv2(stock_moy_stat_1a,"output/moy_stat_1a.csv")
write.csv2(stock_moy_stat_2a,"output/moy_stat_2a.csv")
write.csv2(stock_moy_eco_1a,"output/moy_eco_1a.csv")
write.csv2(stock_moy_eco_2a,"output/moy_eco_2a.csv")
write.csv2(stock_moy_info_1a,"output/moy_info_1a.csv")
write.csv2(stock_moy_info_2a,"output/moy_info_2a.csv")
write.csv2(stock_moy_hum_1a,"output/moy_hum_1a.csv")
write.csv2(stock_moy_hum_2a,"output/moy_hum_2a.csv")


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

stock_p12_g_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_p12_g_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_p12_g_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_p12_g_2a) <-  c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


stock_p12_stat_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_p12_stat_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_p12_stat_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_p12_stat_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


stock_p12_eco_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_p12_eco_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_p12_eco_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_p12_eco_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


stock_p12_info_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_p12_info_1a) <- c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_p12_info_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_p12_info_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


stock_p12_hum_1a <- as.data.frame(matrix(NA,1,7))
colnames(stock_p12_hum_1a) <-  c("AST 1A", "But-Stid","ERASMUS - Contractuels","Eco BL", "Eco D2","Interne","Mathématiques")

stock_p12_hum_2a <- as.data.frame(matrix(NA,1,8))
colnames(stock_p12_hum_2a) <- c("AST 1A", "AST 2A","ERASMUS - Contractuels", "But-Stid","Eco BL", "Eco D2","Interne","Mathématiques")


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
              p12_hum =sum(p12_hum,na.rm = T))
              
  
  denomb_2a <- bdd_1a2a_net_an %>%
    filter(annee_ecole == "2A") %>%
    group_by(spe_entree) %>%
    summarise(p12_g = sum(p12_g,na.rm = T),
              p12_stat = sum(p12_stat,na.rm = T),
              p12_eco = sum(p12_eco,na.rm = T),
              p12_info = sum(p12_info,na.rm = T),
              p12_hum =sum(p12_hum,na.rm = T))
  
  
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
  for (j in 1:7) {
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
  for (j in 1:8) {
    # j<-8
stock_p12_g_2a[i,j]<-round(as.numeric(stock_p12_g_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
stock_p12_stat_2a[i,j]<-round(as.numeric(stock_p12_stat_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
stock_p12_eco_2a[i,j]<-round(as.numeric(stock_p12_eco_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
stock_p12_info_2a[i,j]<-round(as.numeric(stock_p12_info_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
stock_p12_hum_2a[i,j]<-round(as.numeric(stock_p12_hum_2a[i,j])/as.numeric(stock_denomb_2a[i,j])*100,1)
}
}

write.csv2(stock_p12_g_1a,"output/p12_g_1a.csv")
write.csv2(stock_p12_g_2a,"output/p12_g_2a.csv")
write.csv2(stock_p12_stat_1a,"output/p12_stat_1a.csv")
write.csv2(stock_p12_stat_2a,"output/p12_stat_2a.csv")
write.csv2(stock_p12_eco_1a,"output/p12_eco_1a.csv")
write.csv2(stock_p12_eco_2a,"output/p12_eco_2a.csv")
write.csv2(stock_p12_info_1a,"output/p12_info_1a.csv")
write.csv2(stock_p12_info_2a,"output/p12_info_2a.csv")
write.csv2(stock_p12_hum_1a,"output/p12_hum_1a.csv")
write.csv2(stock_p12_hum_2a,"output/p12_hum_2a.csv")
# pareil mais en dissociant ingé/attaché


# # redoublement eleves sur 10 ans
# 
# table(bdd_1a2a$annee,bdd_1a2a$redoublement)
# bb<-bdd[bdd$redoublement==1,]
# for (i in 1:10) {
#   #  i <- 3
# 
# summarise(moy_g = mean(moyenne_generale),
#           moy_stat = (mean(MSS1,na.rm = T)+mean(MSS2,na.rm = T))/2,
#           moy_eco = (mean(MES1,na.rm = T)+mean(MES2,na.rm = T))/2,
#           moy_info = (mean(MIS1,na.rm = T)+mean(MIS2,na.rm = T))/2,
#           moy_hum =(mean(MHS1,na.rm = T)+mean(MHS2,na.rm = T))/2,
#           nb_redoub = sum(as.numeric(redoublement))
# )
  
# }






# Choix 3A ingé selon filiere origine

# travaux sur 3A
bdd_3a <- bdd[bdd$annee_ecole=="3A" & !is.na(bdd$filiere_3A) & bdd$att_ing=="ingénieur",]

#  conserver une ligne par eleve et par année
bdd_3a <- bdd_3a[!duplicated(cbind(bdd_3a$id_etudiant,bdd_3a$annee)),]


# recoder filiere 3A pour pb sur autres et pb sur nom filiere (regroupement)
bdd_3a$fil_3a <- ifelse(bdd_3a$filiere_3A == "Autres" & bdd_3a$voie_lib %in% c("3A,3A GDRIF",
                                                                               "3A,3A GDRIF,OFPR",
                                                                               "3A,3A GR",
                                                                               "3A,3A GR,3A Ing,OFPR,T00000",
                                                                               "3A,3A GR,3A Ing,T00000",
                                                                               "3A,3A GR,3A Ing,T01850",
                                                                               "3A,3A GR,3A Ing,T02650",
                                                                               "3A,3A GR,CVEC",
                                                                               "3A,3A GR,CVEC,OFPR",
                                                                               "3A,3A GR,CVEC,OFPR,Tpoursuite",
                                                                               "3A,3A GR,CVEC,Tpoursuite",
                                                                               "3A,3A GR,OFPR"),
                        "GDR",
                        ifelse(bdd_3a$filiere_3A == "Autres" & bdd_3a$voie_lib %in% c("3A,3A Ing,3A SBio,OFPR,T00000",
                                                                                       "3A,3A Ing,3A SBio,OFPR,T01850",
                                                                                       "3A,3A Ing,3A SBio,T00000",
                                                                                       "3A,3A Ing,3A SBio,T01850",
                                                                                       "3A,3A SBio",
                                                                                       "3A,3A SBio,CVEC",
                                                                                       "3A,3A SBio,CVEC,OFPR",
                                                                                       "3A,3A SBio,CVEC,OFPR,Tpoursuite",
                                                                                       "3A,3A SBio,CVEC,Tpoursuite",
                                                                                       "3A,3A SBio,OFPR",
                                                                                       "3A,3A SBio,OFPR,Tpoursuite",
                                                                                       "3A,3A SBio,T00000"),
                                "SBio",
                               ifelse(bdd_3a$filiere_3A == "M",
                                      "MKT",
                                      ifelse(bdd_3a$filiere_3A == "SV",
                                                   "SBio",
                                             ifelse(bdd_3a$filiere_3A == "ISTS",
                                                    "MES",
                                                     bdd_3a$filiere_3A)))))

# creation boucles sortie stat par bloc annee

bdd_3a$bloc_an<- ifelse(bdd_3a$annee %in% c(2015,2016,2017),
                              "2015-2017",
                              ifelse (bdd_3a$annee %in% c(2018,2019,2020),
                                      "2018-2020",
                                      ifelse(bdd_3a$annee %in% c(2021,2022),
                                             "2021-2022",
                                             "2023-2024")))

# recoder spe_entree pour ECO
bdd_3a$spe_entree <- ifelse(bdd_3a$concours_origine =="Concours externe : spécialité 'économie et sciences sociales'",
                              "Eco BL", 
                              ifelse(bdd_3a$concours_origine =="concours externe : spécialité 'économie et sciences sociales'",
                                     "Eco BL",
                                     ifelse (bdd_3a$concours_origine == "Concours externe : spécialité 'économie et gestion'",
                                             "Eco D2",
                                             bdd_3a$specialite_entree)))

bdd_3a$spe_entree <- ifelse(bdd_3a$concours_origine =="Admission sur titres (dossier + entretien) : niveau L3",
                              "AST 1A", 
                              ifelse(bdd_3a$concours_origine =="Admission sur titres (dossier + entretien) : niveau M1 ou plus, admission en 1ère année",
                                     "AST 1A",
                                     ifelse (bdd_3a$concours_origine == "Admission sur titres (dossier + entretien) : niveau M1 ou plus, admission en 2e année",
                                             "AST 2A",
                                             ifelse (bdd_3a$concours_origine == "Concours externe",
                                                     "ERASMUS - Contractuels",
                                                     ifelse (bdd_3a$concours_origine == "Contractuel",
                                                             "ERASMUS - Contractuels",
                                                             ifelse (bdd_3a$concours_origine == "Erasmus",
                                                                     "ERASMUS - Contractuels",
                                                                     bdd_3a$spe_entree))))))

# denombrement eleves sur 10 ans

  denomb_3a <- bdd_3a %>%
    group_by(bloc_an, spe_entree,fil_3a) %>%
    summarise(nb_eleves = n())
  
  
write.csv2(denomb_3a,"output/denomb_3a.csv")



