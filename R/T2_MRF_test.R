#' Comparison of two Markov Random Fields using Hotellings T2
#'
#' Hotelling's T2 is the multivariate extension of Welch's t-test used to compare two sets of means. The test is used here to test for differences in the penalized partial correlations in two diffrent MRFs. 
#'
#' @param X A dataset to be used to estimate an MRF/Rnet
#' @param Y A dataset to be used to estimate an MRF/Rnet
#' @param L1 The regularization pentalty to use for both networks
#' @param vertices Vertices to include in MRF. Must be a subset of names(x)
#' @param subset An expression for the subsetting X and Y based on columns not used in the MRF. Appled to both data sets
#' @param B The number of subsamples to draw. The same number of subsamples is drawn from both data sets.
#' @param n_b The size of the subsamples. May be provided as an integer less than the number of rows in x, or as a proportion.
#' @param replace Logical. Is subsampling done with (T) or without (F) replacement.
#' @param seed Random seed value for reproducibility. The same seed is applied to sampling from both data sets.
#' @export
#' @importFrom stats pt pf var
#' @importFrom ICSNP HotellingsT2
#' @importFrom rlang is_empty
#' 
#' @return An S3 object of class 'mrf_t2' containing crude test results, adjusted test results, and a table of pair-wise comparisons of unique edges
#'


CompareMRF_T2 <- function(
  X,
  Y,
  L1,
  vertices = NULL,
  subset = NULL,
  B = 500,
  n_b = 0.5,
  replace = TRUE,
  seed = NULL
)
{
  v.orig <- vertices
  #browser()
  
  vertices <- if(is.null(vertices)) intersect(names(X), names(Y)) else intersect(vertices, intersect(names(X), names(Y)))
  if(is_empty(vertices)) stop('No valid vertices/variables identified.')
  
  distn_X <- BootstrapEdgeDistn(
    x = X,
    L1 = L1,
    vertices = vertices,
    subset = subset,
    B = B,
    n_b = n_b,
    replace = replace,
    seed = seed
    )
  
  distn_Y <- BootstrapEdgeDistn(
    x = Y,
    L1 = L1,
    vertices = vertices,
    subset = subset,
    B = B,
    n_b = n_b,
    replace = replace,
    seed = seed
    )
  
  edges.obs <- which(apply(distn_X, 2, function(x) any(x!=0))|apply(distn_Y, 2, function(x) any(x!=0)))
  
  T2_crude.obj <- HotellingsT2(X = distn_X[,edges.obs], Y = distn_Y[,edges.obs])
  
  T2.crude <- T2_crude.obj$statistic
  df1.crude = length(edges.obs)
  df2.crude = 2 * B - df1.crude - 1
  
  df1.adj = dim(X)[2]
  df2.adj = 2 * B - df1.adj - 1
  
  T2.adj <- T2.crude  * df2.adj/df2.crude * df1.crude/df1.adj
  
  p.adj <- 1 - pf(T2.adj, df1 = df1.adj, df2 = df2.adj )
  
  t_table <- data.frame(
    X_bar = apply(distn_X, 2, mean),
    Y_bar = apply(distn_Y, 2, mean),
    d_bar = apply(distn_X, 2, mean) - apply(distn_Y, 2, mean),
    var_X = apply(distn_X, 2, stats::var),
    var_Y = apply(distn_Y, 2, stats::var)
    )
  t_table <- t_table[t_table$var_X!=0 | t_table$var_Y!=0,]
  t_table$t_c <- with(t_table, d_bar/sqrt(var_X/B + var_Y/B))
  t_table$p <- 2 * (1 - stats::pt(abs(t_table$t_c), df = B - 1))
  #t_table$sym <- cut(
  #  t_table$p, 
  #  breaks = c(0, 0.01, 0.05, 0.1, 1.01),
  #  labels = c('**', '* ', ". ", ''),
  #  include.lowest = T
  #)
  
  output.obj <- list(
    crude = T2_crude.obj,
    adj = list(
      T2 = T2.adj[1,1],
      df1 = df1.adj,
      df2 = df2.adj,
      pval = p.adj[1,1]
      ),
    t_table = t_table
    )
  class(output.obj) <- 'mrf_t2'
  return(output.obj)
}