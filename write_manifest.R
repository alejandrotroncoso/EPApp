# Creates manifest.json, which Posit Connect Cloud needs to install the R
# packages of the app. Run it from this folder, with the app working on your
# computer, and again whenever you add or update packages. Then commit the
# file to GitHub.
if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R",
                         appFiles = c("app.R", "R/epa_core.R", "report/epa_report.Rmd", "README.md",
                                      list.files("examples", recursive = TRUE, full.names = TRUE)))
