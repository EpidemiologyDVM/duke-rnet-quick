#'Convert_MICs - Processing raw AST data
#'
#'Antimicrobial susceptibility testing (AST) data from surveillance programs is typically determined via serial microdilution and reported as interval censored minimum inhibitory concentrations (MIC). This function converts MIC values reported as a combination of a comparison sign characters (e.g., > or <=) and numerical characters (e.g., 0.5 or 8) to numerical values prior to analysis. For example, an entry of '= 2' indicates the bacteria was inhibited by 2 ug/mL of drug, but was not inhibited by 1 ug/mL. In most cases, the comparison character can be ignored, but entries with '>' indicate the value exceeds the highest tested drug concentration. To represent the increased resistance of these isolates, the reported concentration must be adjusted upwards by one concentration, even though that concentration was not tested. For example the entry '> 4' (growth was not inhibited at 4 ug/mL, the highest tested concentration) should be adjusted to 8 to represent these results indicated greater resistance than '= 4' (growth was inhibited at 4 ug/mL, but not at 2 ug/mL). This function removes the comparison signs and adjusts the values as neccessary, returning a more conveniently labeled data.frame containing numeric values and selected metadata. Two formats are currently supported: 1) The standard format where the sign is held in the same field as the concentration, and 2) the format used by the NARMS program where comparison sign is stored in a seperate field.
#'@param x A matrix-like object containing MIC data.
#'@param drugs A character vector of column names corresponding to the drug names/abbreviations to be converted.
#'@param mic_text The character string used to identify columns containing MICs.
#'@param sign_text The character string used to identify columns containing the comparison signs. Ignored where mic_format = 'Standard'.
#'@param append_orig If true, formatted columns are appended to original data. If false, only 
#'@param sep The character used to seperate the drug names from the mic_text and sign_text in column names.
#'@param mic_format A character string indicating the format of the data. Currently accepts 
#'
#'@export
#'@return a data.frame containing numeric MIC data, and metadata if selected.

Convert_MICs <- function(
  x, 
  drugs, 
  mic_text = NULL, 
  sign_text = 'SIGN', 
  append_orig = FALSE, 
  sep = '_', 
  mic_format = NULL
  )
{
  FORMAT_OPTIONS <- c('standard', 'NARMS')
  #browser()
  mic_format <- if(is.null(mic_format)) 'standard' else if(mic_format == '') 'standard' else FORMAT_OPTIONS[suppressWarnings(charmatch(mic_format, table = FORMAT_OPTIONS, nomatch = 'standard'))]
  if(mic_format == 'standard') return(
    .convert_MICs_std(
      x = x, 
      drugs = drugs, 
      mic_text = mic_text, 
      sign_text = sign_text,
      append_orig = append_orig,
      sep = sep
      #ids = ids,
      #metadata = metadata
      ))
  
  if(mic_format == 'NARMS') return(
    .convert_MICs_NARMS(
      x = x, 
      drugs = drugs, 
      mic_text = mic_text, 
      sign_text = sign_text,
      append_orig = append_orig,
      sep = sep
      #ids = ids,
      #metadata = metadata
      ))
  
  stop('Invalid MIC format declared.')
  
}

.convert_MICs_std <- function(
  x, 
  drugs, 
  mic_text, 
  sign_text = NULL, 
  append_orig, 
  sep
  #ids,
  #metadata
)
{
  raw.data <- x
  
  .sign_adj_MICs <- function(x) {
    num.val <- as.numeric(stringr::str_extract(x, "\\d+\\.*\\d*"))
    num.val[grepl(">", x)] <- num.val[grepl(">", x)] * 2
    return(num.val)
  }
  
  if(is.null(mic_text)|mic_text == '') test.mic.names <- drugs else test.mic.names <- paste(drugs, mic_text, sep = sep)
  if(!all(test.mic.names%in%names(raw.data))) {
    missing.names <- test.mic.names[!test.mic.names%in%names(raw.data)]
    for(x in missing.names) stop('"', x, '" not in columns names')
  }
  
  cleaned.list <- sapply(
                    X = raw.data[,if(is.null(mic_text)|mic_text == '') x else paste(drugs, mic_text, sep = sep)],
                    FUN = .sign_adj_MICs
                    )
  #browser()  
  cleaned.list <- as.data.frame(cleaned.list)
  #if(!rlang::is_empty(metadata)) {
  #  if(!rlang::is_empty(intersect(metadata, names(x)))) {
  #    metadata.list <- x[,intersect(metadata, names(x))]
  #    cleaned.list <- cbind(metadata.list, cleaned.list)
  #  }
  #}
  
  #if(!is.null(ids)) {
  #  if(!ids%in%names(x)) stop('ID variable', ids, 'not in column names of x.')
  #  if(length(unique(x[[ids]])) == dim(x)[1]) row.names(cleaned.list) <- x[[ids]] else warning('Values in x$', ids, ' not unique', sep = '')
  #}
  
  if(append_orig) return(cbind(x, as.data.frame(cleaned.list))) else return(as.data.frame(cleaned.list))
}


.convert_MICs_NARMS <- function(
  x, 
  drugs, 
  mic_text, 
  sign_text, 
  append_orig, 
  sep
  #ids,
  #metadata
  )
{
  raw.data <- x
  
  .sign_adj_MICs <- function(x) {
    if (is.na(x[2])) return(as.numeric(NA))
    if (is.na(x[1])) x[1] <- '='
    if (x[1] == '>'| x[1] == '>=') new.val <- as.numeric(x[2]) * 2 else new.val <- as.numeric(x[2])
    return(new.val)
  }
  
  if(is.null(mic_text)|mic_text == '') test.mic.names <- drugs else test.mic.names <- paste(drugs, mic_text, sep = sep)
  if(!all(test.mic.names%in%names(raw.data))) {
    missing.names <- test.mic.names[!test.mic.names%in%names(raw.data)]
    for(x in missing.names) stop('"', x, '" not in columns names')
  }
  
  test.sign.names <- paste(drugs, sign_text, sep = sep)
  if(!all(test.sign.names%in%names(raw.data))) {
    missing.names <- test.sign.names[!test.sign.names%in%names(raw.data)]
    for(x in missing.names) stop('"', x, '" not in columns names')
  }
  
  cleaned.list <- sapply(drugs, 
                         FUN = function(x) apply(raw.data[,c(paste(x, sign_text, sep = sep), if(is.null(mic_text)|mic_text == '') x else paste(x, mic_text, sep = sep))], 1, .sign_adj_MICs),
                         USE.NAMES = TRUE
  )
  #cleaned.list <- as.data.frame(cleaned.list)

  #  if(!rlang::is_empty(metadata)) {
  #  if(!rlang::is_empty(intersect(metadata, names(x)))) {
  #    metadata.list <- x[,intersect(metadata, names(x))]
  #    cleaned.list <- cbind(metadata.list, cleaned.list)
  #  }
  #}
  
  #if(!is.null(ids)) {
  #  if(!ids%in%names(x)) stop('ID variable', ids, 'not in column names of x.')
  #  if(length(unique(x[[ids]])) == dim(x)[1]) row.names(cleaned.list) <- x[[ids]] else warning('Values in x$', ids, ' not unique', sep = '')
  #}
  
  if(append_orig) return(cbind(x, as.data.frame(cleaned.list))) else return(as.data.frame(cleaned.list))
}
