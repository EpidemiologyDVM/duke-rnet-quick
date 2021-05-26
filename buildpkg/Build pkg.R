rm(list = ls())

VERSION = "1.3.1"
#BUILD_NUM = "9001"
BUILD_NUM = NULL

DONT_TEST = F  #SET TO TRUE TO **NOT** RUN EXAMPLES MARKED FOR NOT TESTING

{
  library(devtools)
  #devtools::install_github('klutometis/roxygen')
  #devtools::install_github('gustavdelius/roxygen') #8.24.2018 - klutometis version of roxygen currently broken. Use gustavdelius' fix.
  library(roxygen2)
  library(rmarkdown)
}

{
  desc <- readLines('DESCRIPTION')
  desc[3] = if(is.null(BUILD_NUM)) paste("Version:", VERSION) else desc[3] = paste("Version:", paste(VERSION, BUILD_NUM, sep = "."))
  desc[4] = paste("Date:", Sys.Date())
  writeLines(desc, 'DESCRIPTION')
  
}
#break
for(file.name in list.files('data')) load(file = file.path('data', file.name))

use_data(NARMS_EC_DATA, V_ATTRS, E_ATTRS, EC_COORDS, overwrite = T)

file.remove('.Rbuildignore')
file.create('.Rbuildignore')
writeLines(text = '^.*\\.tar\\.gz$', con = './.Rbuildignore')
usethis::use_build_ignore(
  files = c(
    "Rnets.Rproj",
    "Archive",
    "buildpkg",
    ".gitignore",
    "prototypes",
    "doc" #ADDED TO AVOID NOTE
  )
)
#ver_num <- '1.0.1.9001'
#inc_major <- F
#inc_minor <- F
#source('./buildpkg/.v_update.R')

devtools::install(build_vignettes = T)
devtools::document() #IGNORE warning about no defined signature for igraph
devtools::build(path = '.')
#sink(file = './buildpkg/check.txt')
devtools::check(run_dont_test = DONT_TEST)
#sink()

#Ignore BootstrapEdgeDistn no visible binding for global variables
