# Installs the packages used by the EPApp (run once).
pkgs <- c("shiny", "bslib", "reactable", "rmarkdown", "knitr", "markdown",
          "dplyr", "tidyr", "purrr", "stringr", "readxl", "jsonlite",
          "irr", "irrCAC", "fmsb")
install.packages(setdiff(pkgs, rownames(installed.packages())))
