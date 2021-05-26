#' Robust Estimator of modularity (Gomez, et al 2009)
#'
#' Newman's method of estimating graphical modularity based on vertex can accomodate edge weights, but cannot incorporate signed edges, e.g. edges with both positive and negative. Gomez, et al, proposed a similar estimator of modularity estimated in two parts corresponding to positive (Q+) and negative (Q-) edges, and the latter is subtracted from the former. The 'Modularity_Signed' function implements this method of modularity estimation, and returns a scalar.
#' @param x A graph presented in of the forms discussed below.
#' @param membership Defines vertex membership to determine if vertices are similar. May be provided as a string that matches an attribute of x or a vector of length equal to the number of vertices in the graph.
#' @param weight Edge weights. Like 'membership', this argument can be defined as a string matching an edge attribute of 'x' or a vector of length equal to the number of edges, but may also be left as NULL which will return an unweighted modularity estimate.
#' @description For flexibility, x may be provided as any of the following formats: an edgelist (data.frame), a weighted adjacency matrix (square numeric matrix), an igraph object, or an rnet.* object (e.g., rnet.basic, rnet.strata.multi, etc.).
#' @return a numeric value estimating the weighted, signed modularity of x, or a numeric vector containing respective modularity estimates if x contained multiple network.
#' @import igraph
#' @export

setGeneric('Modularity_Signed',
	function(x,	membership, weight = NULL) {
	if(is.null(weight)) {
		x$w_ij <- rep(1, dim(x)[1])
		wt_attr <- 'None'
	} else {
		if(is.character(x[weight])) stop("When defined as an index, 'weight' must refer to a numeric vector.") else x['w_ij'] <- x[weight] 
		x['w_ij'] <- x[weight]
		if(is.character(weight)) wt_attr <- weight else wt_attr <- names(x)[wt_attr]
	}
	
	if(!all(c('i', 'j')%in%names(x))) names(x)[c(1, 2)] <- c('i', 'j')

	if(length(membership) == 1 & sum(grepl(membership[1], names(x))) == 2) {
		x[c('Attr_i', 'Attr_j')] <- x[which(grepl(membership, names(x)))]
		attr(x, 'membership_factor') <- membership
	} else if(length(membership) == 2) {
		x['Attr_i'] <- x[membership[1]]
		x['Attr_j'] <- x[membership[2]]
	} else stop('Invalid "membership" argument; must partially match exactly 2 columns (if length(membership) = 1) OR must exactly match 2 columns (if length(membership) = 2).')
	Q <- .sign.Q.internal(x)
	attr(Q, 'weight') <- wt_attr
	attr(Q, 'membership') <- membership_attr
	return(Q)
})

#' @rdname Modularity_Signed
#' 
setMethod('Modularity_Signed',
	signature(x = 'matrix'),
	function(x, membership, weight = NULL) {
		if(!all(dimnames(x)[[1]] == dimnames(x)[[2]])) stop('Adjacency matrix row & columnn names must be symmetric.')
		v.set <- dimnames(x)[[1]]
		membership_attr <- 'Undeclared'
		if(length(membership) == 1) if(membership%in%names(attributes(x))) {
			membership_attr <- membership
			membership <- attr(x, membership) 
		} else stop(membership, 'is not a valid attribute of x.') 
		if(length(membership)!=length(v.set)) stop("Length of 'membership' vector must match dimensions of 'x'.")
		
		x <- Sq2L(x, keep = c(T, F, F), output.col.names = c('i', 'j', 'w_ij'), drop.values = 0)
		x$Attr_i <- membership[match(x$i, v.set)]
		x$Attr_j <- membership[match(x$j, v.set)]
		Q <- .sign.Q.internal(x)
		attr(Q, 'weight') <- weight
		attr(Q, 'membership') <- membership_attr
		return(Q)
})

#' @rdname Modularity_Signed
#' 
setMethod('Modularity_Signed',
	signature(x='igraph'),
	function(x, membership, weight = NULL) {

		x.frame <- data.frame(igraph::as_edgelist(x, F))
		names(x.frame) <- c('i', 'j')

		#ASSIGN ATTR_I & ATTR_J
		if(length(membership) == 1) if(membership%in%igraph::vertex_attr_names(x)) {
			membership_attr <- membership
			membership <- vertex_attr(x, membership) 
		} else stop(membership, 'is not a valid attribute of x.') 

		if(length(membership)!=length(V(x))) stop("Length of 'membership' vector must match dimensions of 'x'.")

		x.frame$Attr_i <- membership[x.frame$i]
		x.frame$Attr_j <- membership[x.frame$j]

		#ASSIGN W_IJ
		if(is.null(weight)) {
			x.frame$w_ij <- 1
			warning('No edge attribute declared; Unweighted signed modularity will be returned.')

		} else if(is.character(weight)) {
			if(!weight%in%igraph::edge_attr_names(x)|!is.numeric(edge_attr(x, weight))) stop('Edge attribute', weight, 'not found or is not numeric.') else x.frame$w_ij <- edge_attr(x, weight)
		
		} else if(is.numeric(weight)) {
			if(length(weight) == igraph::ecount(x)) x.frame$w_ij <- edge_attr(x, weight) else stop('Numeric weight argument length must ecount(x).')
			weight <- "Unspecified"
		} else stop(weight, ' is not a valid weight argument.')

		Q <- .sign.Q.internal(x.frame)
		attr(Q, 'weight') <- weight
		attr(Q, 'membership') <- membership_attr
		return(Q)
})

#' @rdname Modularity_Signed
#' 
setMethod('Modularity_Signed',
	signature(x='rnet.basic'),
	function(x, membership = NULL, weight = 'omega') Modularity_Signed(x@R, membership, weight)
	)

#' @rdname Modularity_Signed
#' 
setMethod('Modularity_Signed',
	signature(x = 'rnet.strata.multi'),
	function(x, membership, weight = 'omega') {
		Q <- sapply(x@R_Strata, Modularity_Signed, membership, weight)
		return(Q)
})

.sign.Q.internal <- function(x) {
	x$K_delta <- x$Attr_i == x$Attr_j
	x$w_ij_pos <- sapply(x$w_ij, max, 0)
	x$w_ij_neg <- sapply(-x$w_ij, max, 0)

	w <- data.frame(i = c(x$i, x$j), w_ij_pos = x$w_ij_pos, w_ij_neg = x$w_ij_neg)
	w_pos <- aggregate(w_ij_pos ~ i, data = w, FUN = sum)
	w_neg <- aggregate(w_ij_neg ~ i, data = w, FUN = sum)

	x$w_i_pos <- w_pos[match(x$i, w_pos$i),2]
	x$w_j_pos <- w_pos[match(x$j, w_pos$i),2]
	W_pos <- sum(w_pos$w_ij_pos)/2
	Q_pos <- if(W_pos ==0) 0 else sum((x$w_ij_pos - (x$w_i_pos * x$w_j_pos)/(2*W_pos))*x$K_delta)/(2*W_pos)

	x$w_i_neg <- w_neg[match(x$i, w_neg$i),2]
	x$w_j_neg <- w_neg[match(x$j, w_neg$i),2]
	W_neg <- sum(w_neg$w_ij_neg)/2
	Q_neg <- if(W_neg ==0) 0 else sum((x$w_ij_neg - (x$w_i_neg * x$w_j_neg)/(2*W_neg))*x$K_delta)/(2*W_neg)

	return(Q_pos * 2*W_pos /(2*W_pos + 2*W_neg) -  Q_neg * 2*W_neg /(2*W_pos + 2*W_neg))	
}
