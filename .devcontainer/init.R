library(tinytex)

# Install tinytex
tinytex::install_tinytex()

tinytex::tlmgr_install(c("babel-english","apa6"))

packages <- c("babel-english", "apa6", "threeparttable", "apa6", "caption", "fancyhdr", "endfloat", "was", "multirow", "threeparttablex", "trimspaces", "xpatch", "lineno", "csquotes", "bookmark", "environ")

tinytex::tlmgr_install(packages)

# Restore renv packages
# renv::restore(prompt = FALSE)

# install.packages("pak")
# pak::pkg_install(c("languageserver","usethis","conig/stop",
# "conig/snipe"))