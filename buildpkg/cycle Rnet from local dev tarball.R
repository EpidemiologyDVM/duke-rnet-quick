latest <- '1.1.0.9005'

remove.packages('Rnets')
detach('package:Rnets')

install.packages(
  paste('Rnets_', latest, '.tar.gz', sep = ''), 
  repos = NULL, type = 'source'
  )
library(Rnets)
