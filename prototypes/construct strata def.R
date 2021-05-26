strata = c('class_x', 'year', 'STATE')
x <- mrsa

ConstructStrataDefs <- function(x, strata_vars) {
  ones <- rep(1, dim(x)[1])
  
  strata_count <- eval(parse(text = paste(
    "aggregate(ones ~",
    paste(strata_vars, collapse = " + "),
    ", data = x, FUN = length)"
  )))
  
  criteria <- vector(mode = 'list', length = dim(strata_count)[1])

  for(i in 1:dim(strata_count)[1]) {
    criteria[[i]]$strata_def <- character(length(strata))
    criteria[[i]]$n <- strata_count[i, length(strata)+1]

    #browser()
    
    for (column in 1:length(strata)) {
      
      if(is.numeric(x[[column]])) {
        criteria[[i]]$strata_def[column] <- paste(names(x)[column], "==", x[i, column])
      } else {
        criteria[[i]]$strata_def[column] <- paste(names(x)[column], " == '", as.character(x[[column]][i]), "'", sep = '')
      }
    }
  }
  
  return(criteria)
  
}

ConstructStrataDefs(x = mrsa, strata = c('class_x', 'STATE'))
