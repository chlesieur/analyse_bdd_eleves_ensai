install.packages("renv")
renv::init()

renv::restore()      # installe ce qui est déjà dans renv.lock mais pas présent
renv::snapshot()     # enregistre dans renv.lock tous les packages "used" (crosstalk, DBI, etc.)

# 4) Vérifier que tout est propre
renv::status()

install.packages(c("DBI", "sp"))

renv::snapshot()

renv::status()

renv::remove("terra")
renv::snapshot()


renv::restore(prompt = FALSE)  # réinstalle les deps pour R 4.5
renv::rebuild()                # (optionnel) reconstruit proprement
renv::snapshot()               # met à jour renv.lock (Version R = 4.5.1)

renv::install("readxl")
renv::snapshot()

quarto::quarto_render()


pkgs <- c(
  "arrow","dplyr","here","DT","readr","readxl",
  "knitr","kableExtra","scales","forcats","tidyr","ggplot2"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Packages manquants dans l'env renv: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

invisible(lapply(pkgs, library, character.only = TRUE))

renv::install(missing)   # si le message te dit lesquels manquent
renv::snapshot()

renv::install("arrow")
renv::install("DT")
renv::install("leaflet")

renv::status()

tools::package_dependencies("leaflet", recursive = TRUE, reverse = TRUE)
