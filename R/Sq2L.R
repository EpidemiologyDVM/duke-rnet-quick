#' Matrix reshaping function
#'
#' This function resphapes all or parts of a square matrix into a 3 column table.
#' @param x A square matrix to reshape into a dataframe.
#' @param col.names A vector indicating how output columns are named.
#' @param keep A logical vector of length 3 indicating which of the three respective elements to keep: upper triangle, main diagonal, and lower triangle.
#' @param drop.values A vector of values to remove from the output.
#' @param drop.NA Logical value indicating if NAs should be removed.
#' @return a data.frame with three columns. Columns 1 & 2 contain the row & column names, and third contains corresponding matrix values.
#' @export
#' @examples
#' demo_mat <- matrix(c(1, 5, 2, 5, 1, 0, 2, 0, NA), nrow = 3)
#' Sq2Long(demo_mat, c('A', 'B', 'value'))
#' Sq2Long(demo_mat, c('A', 'B', 'value'), drop.values = 0, drop.NA = FALSE)


Sq2Long <- function(x, col.names = NULL, keep = c(TRUE, TRUE, TRUE), drop.values = 0, drop.NA = TRUE) {
	if(!is.matrix(x)) stop('x argument must be a matrix')
	if(nrow(x)!=ncol(x)) stop('x argument must be a square matrix')
	if(is.null(dimnames(x))) dimnames(x) <- list(1:dim(x)[[1]], 1:dim(x)[[1]])
	long <- reshape(
		as.data.frame(x, stringsAsFactors = FALSE),
		ids = rownames(x),
		idvar = 'i',
		direction = 'long',
		varying = colnames(x),
		v.names = 'val_ij',
		times = colnames(x),
		timevar = 'j'
		)[, c(3, 1, 2)]
	if(drop.NA) long <- long[!is.na(long$val_ij),]
	if(length(drop.values) > 0) for(val in drop.values) long <- long[long$val_ij != val,]
	if(!keep[1]) long <- long[match(long[['i']], rownames(x)) >= match(long[['j']], colnames(x)),]
	if(!keep[2]) long <- long[match(long[['i']], rownames(x)) != match(long[['j']], colnames(x)),]
	if(!keep[3]) long <- long[match(long[['i']], rownames(x)) <= match(long[['j']], colnames(x)),]
	if(length(col.names) == 2) names(long) <- c(paste(col.names[1], '1', sep = ''), paste(col.names[1], '2', sep = ''), col.names[2])
	if(length(col.names) == 3) names(long) <- col.names

	if(!any(length(col.names) == 2, length(col.names) == 3, is.null(col.names))) warning('No names provided for output columns. Default names used')
	return(long)
}