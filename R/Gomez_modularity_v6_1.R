#' Robust Estimator of modularity (Gomez, et al 2009)
#'
#' Newman's method of estimating graphical modularity based on vertex can accomodate edge weights, but cannot incorporate signed edges, e.g. edges with both positive and negative. Gomez, et al, proposed a similar estimator of modularity estimated in two parts corresponding to positive (Q+) and negative (Q-) edges, and the latter is subtracted from the former. The 'signed_modularity' function implements this method of modularity estimation, and returns a scalar.
#' @param x A graph presented in of the forms discussed below.
#' @param membership Defines vertex membership to determine if vertices are similar. May be provided as a string that matches an attribute of x or a vector of length equal to the number of vertices in the graph.
#' @param weight Edge weights. Like 'membership', this argument can be defined as a string matching an edge attribute of 'x' or a vector of length equal to the number of edges, but may also be left as NULL which will return an unweighted modularity estimate.
#' @description For flexibility, x may be provided as any of the following formats: an edgelist (data.frame), a weighted adjacency matrix (square numeric matrix), an igraph object, or an rnet.* object (e.g., rnetBasic, rnetMultiStrata, etc.).
#' @return a numeric value estimating the weighted, signed modularity of x, or a numeric vector containing respective modularity estimates if x contained multiple network.
#' @import igraph
#' @importFrom stats aggregate
#' @export
#' @examples 
#' \donttest{
#' #Signed modularity in a random graph with 10 vertices
#' 
#' x <- sample_gnp(5, 0.4)  #Creates a random graph with 10 vertices and density ~ 40%
#' x <- set_edge_attr(x, 'weight', value = runif(gsize(x), -0.5, 0.5))  
#'      #Randomly assign edge weights to edge attribute 'weight', both positive and negative
#' x <- set_vertex_attr(x, name = 'group', value = sample(c('red', 'blue'), size = 5, replace = TRUE))
#' 
#' signed_modularity(x, membership = 'group', weight = 'weight')
#' signed_modularity(x, membership = 'group')
#' 
#' }

setGeneric('signed_modularity',
  function(x, membership, weight = NULL)
  {
  UseMethod("signed_modularity", x)
})


#' @title signed_modularity-matrix
#' @description signed_modularity for class(x) = 'matrix'
#' @rdname signed_modularity-matrix
#' @param x A weighed adjacency matrix
#' @param membership Defines vertex membership to determine if vertices are similar. May be provided as a string that matches an attribute of x or a vector of length equal to the number of vertices in the graph.
#' @param weight Edge weights. Like 'membership', this argument can be defined as a string matching an edge attribute of 'x' or a vector of length equal to the number of edges, but may also be left as NULL which will return an unweighted modularity estimate.
#' 
setMethod('signed_modularity',
  signature(x = 'matrix'),
  function(x, membership, weight = NULL) 
    {
		  if(!is.null(dimnames(x))) {
        if(!all(dimnames(x)[[1]] == dimnames(x)[[2]])) stop('Adjacency matrix row & columnn names must be symmetric.')
		    v.set <- dimnames(x)[[1]]
		  } else v.set <- 1:dim(x)[[1]]
	    membership_attr <- 'Undeclared'
		  if(length(membership) == 1) {
		    if(membership%in%names(attributes(x))) {
			    membership_attr <- membership
			    membership <- attr(x, membership) 
		    } else stop(membership, ' is not a valid attribute of x.') 
		  } else if(length(membership)!=length(v.set)) stop("Length of 'membership' vector must match dimensions of 'x'.")

	    Q <- .sign.Q.internal(x, membership)
    
		  attr(Q, 'weight') <- weight
		  attr(Q, 'membership') <- membership_attr
		  return(Q)
  })


#' @title signed_modularity-igraph
#' @description signed_modularity for class(x) = 'igraph'
#' @rdname signed_modularity-igraph
#' @param x An igraph object
#' @param membership Defines vertex membership to determine if vertices are similar. May be provided as a string that matches an attribute of x or a vector of length equal to the number of vertices in the graph.
#' @param weight Edge weights. Like 'membership', this argument can be defined as a string matching an edge attribute of 'x' or a vector of length equal to the number of edges, but may also be left as NULL which will return an unweighted modularity estimate.
#' 
setMethod('signed_modularity',
  signature(x = 'igraph'),
  function(x, membership, weight = NULL) 
    {
		  if(length(membership) == 1) if(membership%in%igraph::vertex_attr_names(x)) {
			  membership_attr <- membership
			  membership <- vertex_attr(x, membership) 
		  } else stop(membership, 'is not a valid attribute of x.') 

		  if(length(membership)!=length(V(x))) stop("Length of 'membership' vector must match dimensions of 'x'.")

		  x.igraph <- as_adjacency_matrix(x, attr = weight, sparse = FALSE)
	
		  Q <- .sign.Q.internal(x.igraph, membership)
		  attr(Q, 'weight') <- weight
		  attr(Q, 'membership') <- membership_attr
		  return(Q)
    })


#' @title signed_modularity-rnetBasic
#' @description signed_modularity for class(x) = 'rnetBasic'
#' @rdname signed_modularity-rnetBasic
#' @param x An object of class 'rnetBasic'
#' @param membership Defines vertex membership to determine if vertices are similar. May be provided as a string that matches an attribute of x or a vector of length equal to the number of vertices in the graph.
#' @param weight Edge weights. Like 'membership', this argument can be defined as a string matching an edge attribute of 'x' or a vector of length equal to the number of edges, but may also be left as NULL which will return an unweighted modularity estimate.
#' 
setMethod('signed_modularity',
  signature(x = 'rnetBasic'), 
  function(x, membership = NULL, weight = 'omega') signed_modularity(x@R, membership, weight)
  )


#' @title signed_modularity-rnetMultiStrata
#' @description signed_modularity for class(x) = 'rnetMultiStrata'
#' @rdname signed_modularity-rnetMultiStrata
#' @param x An object of class 'rnetMultiStrata'
#' @param membership Defines vertex membership to determine if vertices are similar. May be provided as a string that matches an attribute of x or a vector of length equal to the number of vertices in the graph.
#' @param weight Edge weights. Like 'membership', this argument can be defined as a string matching an edge attribute of 'x' or a vector of length equal to the number of edges, but may also be left as NULL which will return an unweighted modularity estimate.
#' 
setMethod('signed_modularity',
  signature(x='rnetStrata'),
  function(x, membership, weight = 'omega')  sapply(x@R_Strata, signed_modularity, membership, weight)
  )

.sign.Q.internal <- function(x, membership) {
  w_pos <- apply(x, c(1, 2), max, 0)
  w_neg <- apply(x, c(1, 2), min, 0)
  
  Q_pos <- 0
  Q_neg <- 0
  
  for(i in 1:nrow(x)) {
    for(j in 1:ncol(x)) {
      if(membership[i] == membership[j] & sum(w_pos)!= 0) Q_pos <- Q_pos + (w_pos[i,j]-sum(w_pos[i,])*sum(w_pos[,j])/sum(w_pos))/sum(w_pos)
      if(membership[i] == membership[j] & sum(w_neg)!= 0) Q_neg <- Q_neg + (w_neg[i,j]-sum(w_neg[i,])*sum(w_neg[,j])/sum(w_neg))/sum(w_neg) 
    }
  }

  return(sum(w_pos)*Q_pos/(sum(w_pos) + sum(w_neg)) - sum(w_neg)*Q_neg/(sum(w_pos) + sum(w_neg)))
  
}
