#' Comparison of two MRFs using Hotellings T2
#'
#' Hotelling's T2 is the multivariate extension of Welch's t-test used to compare two sets of means. The test is used here to test for differences in the penalized partial correlations in two diffrent MRFs. 
#'
#' @param X A dataset to be used to estimate an MRF/Rnet
#' @param Y A dataset to be used to estimate an MRF/Rnet
#' @param L1 The regularization pentalty to use for both networks
#' @param vertices Vertices to include in MRF. Must be a subset of names(x)
#' @param B The number of subsamples to draw. The same number of subsamples is drawn from both data sets.
#' @param n_b The size of the subsamples. May be provided as an integer less than the number of rows in x, or as a proportion.
#' @param replace Logical. Is subsampling done with (T) or without (F) replacement.
#' @param seed Random seed value for reproducibility. The same seed is applied to sampling from both data sets.
#'
#' @return An S3 object of class 'mrf_t2' containing crude test results, adjusted test results, and a table of pair-wise comparisons of unique edges
#'
#' @example 
#' test.MRF.T2 <- CompareMRF_T2(
#'    X = data_X,
#'    Y = data_Y,
#'    L1 = 0.25,
#'    vertices = V_SET,
#'    B = 30
#'    )
#'

CompareMRF_T2 <- function(
  X,
  Y,
  L1,
  vertices = NULL,
  subset = NULL,
  B = 500,
  n_b = 0.5,
  replace = T,
  seed = NULL
)
{
  v.orig <- vertices
  #browser()
  
  vertices <- if(is.null(vertices)) intersect(names(X), names(Y)) else intersect(vertices, intersect(names(X), names(Y)))
  if(rlang::is_empty(vertices)) stop('No valid vertices/variables identified.')
  
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
  
  T2_crude.obj <- ICSNP::HotellingsT2(X = distn_X[,edges.obs], Y = distn_Y[,edges.obs])
  
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
    var_X = apply(distn_X, 2, var),
    var_Y = apply(distn_Y, 2, var)
    )
  t_table <- t_table[t_table$var_X!=0 | t_table$var_Y!=0,]
  t_table$t_c <- with(t_table, d_bar/sqrt(var_X/B + var_Y/B))
  t_table$p <- 2 * (1 - pt(abs(t_table$t_c), df = B - 1))
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
setMethod(
  name = 'summary',
  signature(object = 'mrf_t2')      
  
)

summary.mrf_t2 <- function(x)
{
  cat(
    "\nMRF Hotellings's T2 Comparison\n\nCrude\nT2 = ", x$crude$statistic, "; p-val = ", x$crude$p.val,
    " (df1 = ", x$crude$parameter[1], ", df2 = ", x$crude$parameter[2],
    ")\n\n Adjusted\nT2 = ", x$adj$T2, "; p-val = ", x$adj$pval,
    " (df1 = ", x$adj$df1, ", df2 = ", x$adj$df2, ")\n\n Pair-wise Edge Tests\n",
    sep = ''
  )
  display.frame <- data.frame(
    X_bar = round(x$t_table$X_bar,3),
    Y_bar = round(x$t_table$Y_bar,3),
    d_bar = round(x$t_table$d_bar,3),
    var_X = round(x$t_table$var_X,4),
    var_Y = round(x$t_table$var_Y,4),
    t_c = round(x$t_table$t_c,2),
    p = paste(
      round(x$t_table$p, 2),
      cut(
        x$t_table$p, 
        breaks = c(0, 0.01, 0.05, 0.1, 1.01),
        labels = c('**', '* ', ". ", '  '),
        include.lowest = T
      ),
      sep = ''
    )
  )
  print(display.frame)
  cat("---\nSignif. codes:  0 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")
}

summary(test.MRF.T2)
library(Rnets)
library('ICSNP')
V_SET <- c('AMP', 'AMC', 'AXO', 'FOX', 'TIO', 'TET', 'CHL', 'GEN', 'STR')
#B = 50
#n_b = 0.5

data_X <- NARMS_EC_DATA[NARMS_EC_DATA$Year == 2008,]
data_Y <- NARMS_EC_DATA[NARMS_EC_DATA$Year == 2009,]


test.MRF.T2 <- CompareMRF_T2(
  X = data_X,
  Y = data_Y,
  L1 = 0.25,
  vertices = V_SET,
  B = 30
  )


#data_X <- as.matrix(
#  BootstrapEdgeDistn(
#    x = NARMS_EC_DATA,
#    L1 = 0.2,
#    vertices = V_SET,
#    subset = 'Year == 2008',
#    B = B,
#    n_b = n_b,
#    replace = T
#    )
#  )

#data_Y <- as.matrix(
#  BootstrapEdgeDistn(
#    x = NARMS_EC_DATA,
#    L1 = 0.2,
#    vertices = V_SET,
#    subset = 'Year == 2009',
#    B = B,
#    n_b = n_b,
#    replace = T
#    )
#  )

no_null <- union(dimnames(data_X)[[2]][as.logical(apply(data_X, 2, var))], dimnames(data_Y)[[2]][as.logical(apply(data_Y, 2, var))])

icsnp_result <- ICSNP::HotellingsT2(X = data_X[,no_null],Y = data_Y[,no_null])
T2.crude <- icsnp_result$statistic
df1.crude = length(no_null)
df2.crude = 2 * B - df1.crude - 1

df1.adj = choose(length(V_SET), 2)
df2.adj = 2 * B - df1.adj - 1

T2.adj <- T2.crude  * df2.adj/df2.crude * df1.crude/df1.adj

p.adj <- 1 - pf(T2.adj, df1 = df1.adj, df2 = df2.adj )

t_table <- data.frame(
  mean_X = round(apply(data_X, 2, mean),4),
  mean_Y = round(apply(data_Y, 2, mean),4),
  d_bar = round(apply(data_X, 2, mean) - apply(data_Y, 2, mean),4),
  var_X = round(apply(data_X, 2, var),6),
  var_Y = round(apply(data_Y, 2, var),6)
  )
t_table <- t_table[t_table$var_X!=0 | t_table$var_Y!=0,]

t_table$t_c <- with(t_table, round(d_bar/sqrt(var_X/B + var_Y/B), 3))
t_table$p <- round(2 * (1 - pt(abs(t_table$t_c), df = B - 1)), 3)
t_table$sym <- cut(
  t_table$p, 
  breaks = c(0, 0.01, 0.05, 0.1, 1.01),
  labels = c('**', '* ', ". ", ''),
  include.lowest = T
  )
  
t_table

