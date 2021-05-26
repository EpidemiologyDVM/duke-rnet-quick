library(data.table)
desc <- readLines("DESCRIPTION")

load('./buildpkg/.v_rec.rda')

build_date <- as.character(Sys.time())
n <- dim(versionRec)[1]
versionRec <- rbind(versionRec, versionRec[n,])

if(exists("ver_num")) {
  v_new <- unlist(strsplit(ver_num, split = '[.]'))
  versionRec$v[n + 1] <- as.numeric(v_new[1])
  versionRec$major[n + 1] <- as.numeric(v_new[2])
  versionRec$minor[n + 1] <- as.numeric(v_new[3])
  versionRec$build[n + 1] <- if(length(v_new) == 4) v_new[4] else NA 
} else if(inc_major) {
  versionRec$major[n + 1] <- versionRec[n + 1, 2] + 1
  versionRec$minor[n + 1] <- 0
  versionRec$build[n + 1] <- NA
  inc_major <- FALSE
} else if(inc_minor) {
  versionRec$minor[n + 1] <- versionRec[n + 1, 3] + 1
  versionRec$build[n + 1] <- NA
  inc_minor <- FALSE
} else if (is.na(last(versionRec$build))) {versionRec$build[n + 1] <- 9001
} else versionRec$build[n + 1] <-  versionRec$build[n + 1] + 1

versionRec[n + 1, 5] <- as.character(build_date)
row.names(versionRec) <- 1:(n+1)
v_new <- if(is.na(last(versionRec$build))) paste('Version:', paste(versionRec[n + 1, 1:3], collapse = '.')) else paste('Version:', paste(versionRec[n + 1, 1:4], collapse = '.')) 

save(versionRec, file = './buildpkg/.v_rec.rda')

desc[3] <- v_new
desc[4] <- paste('Date:', Sys.Date())

rm(versionRec, v_new, n)

writeLines(desc, "DESCRIPTION")