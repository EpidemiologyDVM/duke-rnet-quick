#' Estimation of partial correlation matrix from precision matrix
#' 
#' An inverse precision matrix (Theta), either penalized or unpenalized, of a dataset can be used to estimate the partial correlation matrix (Omega), which describes the correlations between members of the dataset, adjusted for correlation with all other members of the set. Lemma 1 from Peng, et al, is used to here to estimate partial correlations (w): w_ij = -t_ij*(t_ii*t_jj)^-0.5, where t_ij denotes the element at the ith row and jth column in Theta, when i != j. The main diagnonal of Omega (w_ii) is typically 1, but here is set to 0 to remove loops from the resultant. Note that w_ij = 0 wherever t_ij is 0.
#' @param theta A precision matrix, typically produced by the graphical LASSO procedure.
#' @return A partial correlation matrix produced by transforming theta.
#' @examples
#' data('mtcars')
#' Sigma <- cor(mtcars[,c(1, 3:7)])
#' Theta <- Sigma^-1
#' Omega <- Estimate_pCorrs(Theta)
#' @export
Estimate_pCorrs <- function(theta)
{
	mat_size <- dim(theta)[1]
	pCorr_mat <- matrix(0, mat_size, mat_size)	
	for(x in 1:mat_size) for(y in 1:mat_size) pCorr_mat[x, y] <- -theta[x,y]*(theta[x,x]*theta[y,y])^-.5
	diag(pCorr_mat) <- 0
	dimnames(pCorr_mat) <- dimnames(theta)
	return(pCorr_mat)
}