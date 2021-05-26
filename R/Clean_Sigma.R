#' Clean_Sigma function
#'
#' Primarily a convenience function, Clean_Sigma estimates a correlation (Sigma) matrix that may contain substantial amounts of missing data and returns it in an easily usable form for the glasso function.
#' @param x Data used to estimate the correlation matrix.
#' @param cor_method Character indicating method used to estimate correlations. Defaults to Spearman's method. See help file for 'cor' function.
#' @param cor_pairing Character indicating the method used to determine how NA entries are handled. See help file for 'cor' function.
#' @param replace.na Logical. Will replace NAs with 0s if TRUE.
#' @importFrom stats cor
#' @export

Clean_Sigma <- function(x,
				cor_method = 's',
				cor_pairing = 'pair',
				replace.na = TRUE
				)
{
	CorrMat <- 	cor(x, 
				method = cor_method, 
				use = cor_pairing
				)
	if(replace.na) CorrMat[is.na(CorrMat)] <- 0
	diag(CorrMat) <- 1
	return(CorrMat)
}