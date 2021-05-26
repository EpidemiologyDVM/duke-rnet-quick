#' L1 Selection for Rnets
#'
#' An implementation of the Stability Approach to Regularization Selection (StARS) method for L1 penalty selection for use with Rnets method.
#' @param x The dataset containing the MICs
#' @param L1_values The set of candidate L1 penalties to be evaluated for creating a sparse precision matrix. Must be non-negative.
#' @param B The number of subsamples to evaluate network stability. Defaults to 100 subsamples.
#' @param n_b The size of the subsample to be drawn from the data. If 0 < n_b < 1, this is interpreted as a proportion of the data set; if n_b > 1, it is interpreted as a set sample size. Defaults to 50\% of sample size.
#' @param vertices A character vector corresponding to the names of the antibiotics to include in the Rnet. Defaults to an empty list, in which case all columns in 'MIC_data' will be included in the Rnet
#' @param n_min The minimum number of observations required for an an estimated correlation to be valid. Defaults to 0, in which case any number of observations will be sufficient to estimate a valid correlation
#' @param cor_method The method used to estimate the correlation matrix. Must be 'pearson', 'spearman', or 'kendall'. Partial matches allowed. Defaults to 'spearman'.
#' @param cor_pairing The method used to determine how NAs are handled when determining which pairs are used to estimate correlations. See 'cor' function documentation for additional information.
#' @param subset The rule for stratifying the data, if desired. 
#' @param forced_zeros Edges to be omitted from the Rnet.
#' @param D_thresh Maximum tolerated D value. Suggested D_thresh = 0.05.
#' @param verbose Logical that tells the function to list progress in estimating Rnets from subsets.
#' @return A vector of D statistics, corresponding the tested L1 values.
#' @import igraph 
#' @import data.table
#' @export
#' @examples 
#' \donttest{
#'  ABX_LIST <- c('AMP', 'AMC', 'AXO', 'TIO', 'NAL', 'CIP', 'STR', 'GEN', 'COT', 'FIS')
#' 
#'  EC_all_L1Selection <- L1Selection(
#'                              x = NARMS_EC_DATA, 
#'                              L1_values = seq(0.05, 0.50, 0.05),
#'                              n_b = 1500,
#'                              vert = ABX_LIST
#'                              )

#' print(EC_all_L1Selection)
#' }

setGeneric('L1Selection',
	function(
		x,
		L1_values,
		B = 100,
		n_b = 0.5,
		vertices = NULL,
		n_min = 1,
		cor_method = 's',
		cor_pairing = 'pair',
		forced_zeros = NULL,
		subset = NULL,
		D_thresh =0.05,
		verbose = TRUE
		)
	{
	  L1_values <- sort(L1_values)
	  
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
		
		if(n_b > dim(x)[1]) stop('Subsample size exceeds the number of records!')
		E_aggr <- list(NA)
		if(is.null(vertices)) vertices <- names(x)
		k <- length(vertices)
		M <- data.frame(b = numeric(0), L1 = numeric(0), t = numeric(0), m = numeric(0))

		sets_b <- sapply(rep(n_b, B), sample, x = 1:dim(x)[1])
		x_b <- array(0, dim = c(n_b, k, B),  dimnames = list(1:n_b, vertices, 1:B))
		W_aggr <- array(0, dim = c(k, k, B, length(L1_values)), dimnames = list(vertices, vertices, 1:B, as.character(L1_values)))

		iter <- 1
		L1_N <- length(L1_values)
		t_0 <- proc.time()[3]

		for(b in 1:B) {
			x_b[,,b] <- as.matrix(x[sets_b[,b],vertices])
			L1_n <- 1
			m_i <- 1
      if(!b%%10 & verbose) message(b, ' simulations completed, ', proc.time()[3] - t_0, ' seconds elapsed.')
			while(m_i > 0 & L1_n <= L1_N) {
			  ti_0 <- proc.time()[3]
				R_i <- Rnet(
					as.data.frame(x_b[,,b]), 
					L1 = L1_values[L1_n], 
					cor_method = cor_method, 
					cor_pairing = cor_pairing,

					n_min = n_min,  
					forced_zeros = forced_zeros,
					subset = subset
					)
				m_i <- ecount(R_i@R)
				
				if(m_i > 0) {
					E_i <- as_edgelist(R_i@R)
					E_aggr[[iter]] <- data.frame(
						b = b, 
						L1 = L1_values[L1_n], 
						V1 = E_i[,1], 
						V2 = E_i[,2], 
						E = paste(E_i[,1], E_i[,2], sep = '--'),
						omega = E(R_i@R)$omega
						)
					
					M[iter,] <- c(b, L1_values[L1_n], proc.time()[3] - ti_0, ecount(R_i@R))
					
					if(dim(R_i@Omega)[1] == dim(W_aggr)[1]) {
					  
					  W_aggr[,,b, as.character(L1_values[L1_n])] <- R_i@Omega 
					
					} else {
					  
					  pair_set <- t(combn(colnames(W_aggr), 2))
					  for(pos in 1:dim(pair_set)[1]) if(pair_set[pos, 1]%in%colnames(R_i@Omega) & pair_set[pos, 2]%in%colnames(R_i@Omega)) W_aggr[pair_set[pos, 1], pair_set[pos, 2], b, as.character(L1_values[L1_n])] <- W_aggr[pair_set[pos, 2], pair_set[pos, 1], b, as.character(L1_values[L1_n])] <- R_i@Omega[pair_set[pos, 1], pair_set[pos, 2]]

					}
					iter <- iter + 1
					L1_n <- L1_n + 1
				}
			}
		}
		E_aggr <- as.data.frame(rbindlist(E_aggr))
		Comp_Stab <- aggregate(Count ~ L1 + E, data = cbind(E_aggr[c('L1', 'E')], Count = 1), FUN = sum)
		Comp_Stab$Pr <- Comp_Stab$Count/B
		Comp_Stab <- within(Comp_Stab, Eta <- 2 * Pr * (1-Pr)/choose(k, 2) )
		StARS <- aggregate(Eta ~ L1, Comp_Stab, FUN = sum)
		StARS.vec <- StARS$Eta
		names(StARS.vec) <- StARS$L1
		message('\nL1Selection completed ', iter - 1, ' loops over ', B,  ' subsets. (', round(sum(M$t), 0), 's elapsed).', sep = '')
		
		if(min(StARS.vec) > D_thresh) warning('No D values below suggested threshold of ', D_thresh)
		
		return(new('rnet.L1.set',
			x = x,
			L1_values = L1_values,
			B = B,
			method_b = method_b,
			sets_b = sets_b,
		  array_b = x_b,
			pr_b = pr_b,
			n_b = n_b,
			W_aggr = W_aggr,
			E_aggr = E_aggr,
			M = M,
			stability = Comp_Stab,
			D = StARS.vec,
			D_thresh = D_thresh
			))
	})

