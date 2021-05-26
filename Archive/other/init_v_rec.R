versionRec <- data.frame(
  v = 1,
  major = 0,
  minor = 0,
  build = 9004,
  timestamp = as.character(Sys.time()),
  stringsAsFactors = F
)

save(versionRec, file = '.v_rec.rda')
