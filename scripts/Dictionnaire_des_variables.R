install.packages("labelled")
library(labelled)
library(arrow)

bdd <- read_rds("data/bdd_2015_2024.rds")

dictionnaire <- look_for(bdd)[,2:4]
dictionnaire

library(dplyr)
library(stringi)

dictionnaire_utf8 <- dictionnaire |>
  mutate(across(where(is.character),
                ~ stri_encode(., from = "latin1", to = "UTF-8"))) 

library(writexl)

write_xlsx(dictionnaire_utf8, path = "data/dictionnaire.xlsx")
