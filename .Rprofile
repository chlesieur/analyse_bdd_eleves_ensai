source("renv/activate.R")

options(
  repos = c(
    CRAN = "https://cloud.r-project.org",
    PPM  = "https://packagemanager.posit.co/cran/latest"
  ),
  pkgType = if (.Platform$OS.type == "windows") "binary" else getOption("pkgType"),
  Ncpus   = max(1L, parallel::detectCores() - 1L),
  download.file.method = "libcurl"
)

Sys.setenv(MAKEFLAGS = paste0("-j", max(1L, parallel::detectCores() - 1L)))

try(renv::settings$use.cache(TRUE), silent = TRUE)

