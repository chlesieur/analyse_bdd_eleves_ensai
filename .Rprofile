# --- renv ---
source("renv/activate.R")

# --- Options d'installation rapides & robustes ---
options(
  repos  = c(CRAN = "https://packagemanager.posit.co/cran/latest"),
  pkgType = if (.Platform$OS.type == "windows") "binary" else getOption("pkgType"),
  Ncpus   = max(1L, parallel::detectCores() - 1L),
  download.file.method = "libcurl"
)

# Compilation parallèle (à l'intérieur d'un package)
Sys.setenv(MAKEFLAGS = paste0("-j", max(1L, parallel::detectCores() - 1L)))

# (optionnel) Assurer l'usage du cache renv
try(renv::settings$use.cache(TRUE), silent = TRUE)
