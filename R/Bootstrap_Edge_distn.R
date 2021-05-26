#' Penalized Partial Correlation Distribution Estimates 
#' 
#' Estimates MRFs from bootstrapped subsamples of data to approximate penalized partial correlation distibutions
#'
#' 
#' @param x dataset for estimating MRFs. Supplied as data.frame.
#' @param L1 Regularization penalty for inducing sparsity in networks
#' @param vertices Vertices to include in MRF. Must be a subset of names(x)
#' @param subset An expression to select a subset of records/rows
#' @param B The number of subsamples to draw
#' @param n_b The size of the subsamples. May be provided as an integer less than the number of rows in x, or as a proportion.
#' @param replace Logical. Is subsampling done with (T) or without (F) replacement.
#' @param seed Random seed value for reproducibility.
#' @param ... other arguments to be passed to Rnet().
#' 
#' @return A numeric matrix containing the estimated penalized partial correlations corresponding to the MRF edges (column) in each subsample (row).
#' @export
#' @examples 
#' 
#'  BootstrapEdgeDistn(
#'    x = NARMS_EC_DATA,
#'    L1 = 0.25,
#'    vertices = c('AMP', 'AMC', 'AXO', 'FOX', 'TIO', 'TET', 'CHL', 'GEN', 'STR'),
#'    subset = expression(Year == 2009)
#'    )

BootstrapEdgeDistn <- function(
  x,
  L1,
  vertices = NULL,
  subset = NULL,
  B = 500,
  n_b = 0.5,
  replace = TRUE,
  seed = NULL,
  ...
  )
{
  #utils::globalVariables(c('V1','V2'))
  #browser()
  if(is.null(vertices)) vertices <- names(x)
  
  if(!is.null(subset)) x <- subset(x, select = vertices, subset = eval(subset))

  if(n_b < 1) {
    method_b <- 'Proportion'
    pr_b <- n_b
    n_b <- round(dim(x)[1] * pr_b, 0)
    if(!n_b) stop('Subsample proportion of data size too small! Must exceed 1/n')
  } else {
    if(n_b%%1!=0) stop('If n_b > 1, n_b must be a whole number.')
    pr_b <- n_b/dim(x)[1]
    method_b <- 'Set count'
  }
  
  if(n_b > dim(x)[1]) stop('Subsample size exceeds the number of records.')
  
  E_MAX_LEN <- choose(length(vertices), 2)
  
  E_aggr <- list()
  if(is.null(vertices)) vertices <- names(x)
  k <- length(vertices)
  M <- data.frame(b = numeric(0), L1 = numeric(0), t = numeric(0), m = numeric(0))
  
  if(!is.null(seed)) set.seed(seed)
  sets_b <- sapply(rep(n_b, B), sample, x = 1:dim(x)[1], replace = replace)
  
  for(i in 1:B) {
    R_i <- Rnet(
      x[sets_b[,i],],
      L1 = L1,
      vertices = vertices,
      ...
      )
    E_aggr[[i]] <- as.data.frame(as_edgelist(R_i@R))
    E_aggr[[i]]$omega <- E(R_i@R)$omega
    E_aggr[[i]]$b <- i
  }
  
  E_aggr <- rbindlist(E_aggr)
  
  master.frame <- data.frame(
    V1 = rep(utils::combn(sort(vertices),2)[1,], B),
    V2 = rep(utils::combn(sort(vertices),2)[2,], B),
    b = sort(rep(1:B, E_MAX_LEN))
    )
  
  E_aggr_all <- merge(
    x = master.frame,
    y = E_aggr,
    by = c('V1', 'V2', 'b'),
    all.x = TRUE
    )
  V1 = V2 = 0 #meaningless, just added to avoid build notes about V1 & V2 not being "visible bindings"
  E_aggr_all <- within(E_aggr_all, {omega[is.na(omega)] <- 0; E <- paste(V1, V2, sep = '--')})
  rm(V1, V2)
  #browser()

  return.frame <- reshape(
    E_aggr_all,
    direction = 'wide',
    v.names = 'omega',
    timevar = 'E',
    idvar = 'b',
    drop = c('V1', 'V2')
    )
  
  return.frame <- return.frame[, -1]
  names(return.frame) <- gsub('omega.', '', names(return.frame))
  
  return(as.matrix(return.frame))
}

#.BootstrapEdgeMean <- function(x) apply(x[,3:dim(x)[1]], 1, mean)
#.BootstrapEdgeVar <- function(x) apply(x[,3:dim(x)[1]], 1, var)

