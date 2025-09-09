install.packages("renv")
renv::init()

renv::restore()      # installe ce qui est déjà dans renv.lock mais pas présent
renv::snapshot()     # enregistre dans renv.lock tous les packages "used" (crosstalk, DBI, etc.)

# 4) Vérifier que tout est propre
renv::status()

install.packages(c("DBI", "sp"))

renv::snapshot()

renv::status()
