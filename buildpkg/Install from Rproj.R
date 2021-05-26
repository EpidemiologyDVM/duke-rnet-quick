library(devtools)
#devtools::install_github('klutometis/roxygen')
#devtools::install_github('gustavdelius/roxygen') #8.24.2018 - klutometis version of roxygen currently broken. Use gustavdelius' fix.
library(roxygen2)
library(rmarkdown)
devtools::install(build_vignettes = T)
