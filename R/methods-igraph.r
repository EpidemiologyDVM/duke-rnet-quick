#' Extended igraph methods
#' 
#' The Rnets package extends the following igraph functions
#' * modularity
#' * degree
#' * gsize
#' * gorder
#' * edge_density
#' 
#' degree generic 
#' @param graph an igraph class object
#' @param ... other arguments (see igraph documentation)

setGeneric(
  name = 'degree',
  def = function(graph, ...) igraph::degree(graph, ...)
  )


#' gsize generic 
#' @param graph an igraph class object
#' @param ... other arguments (see igraph documentation)

setGeneric(
  name = 'gsize',
  def = function(graph, ...) igraph::gsize(graph, ...)
  )


#' gorder generic 
#' @param graph an igraph class object
#' @param ... other arguments (see igraph documentation)

setGeneric(
  name = 'gorder',
  def = function(graph, ...) igraph::gorder(graph, ...)
  )


#' edge_density generic 
#' @param graph an igraph class object
#' @param ... other arguments (see igraph documentation)

setGeneric(
  name = 'edge_density',
  def = function(graph, ...) igraph::edge_density(graph, ...)
  )


#' tkplot generic
#' @param graph an igraph class object
#' @param canvas.width tkplot window width
#' @param canvas.height tkplot window width
#' @param ... other arguments

setGeneric(
  name = 'tkplot',
  def = function(graph, canvas.width = 450, canvas.height = 450, ...) igraph::tkplot(graph, canvas.width, canvas.height, ...)
  )




#' modularity.RnetBasic() - See igraph documentation for full documentation.
#' @param x an 
#' @param membership Numeric vector, for each vertex it gives its community. The communities are numbered from one.
#' @param weights If not NULL then a numeric vector giving edge weights. 
#' @param ... Additional arguments, none currently
#' @return A numeric scalar, the modularity score of the given configuration.
#' @aliases modularity
#' @rdname modularity-rnetBasic
#' @export

modularity.rnetBasic <- function(x, membership, weights = NULL, ...){
  return(modularity(x@R, membership, weights = NULL, ...))
}



#' degree.RnetBasic() - See igraph documentation for full documentation.
#' #' Gives degrees for all vertices.
#' @param graph an RnetBasic object
#' @param ... additional arguments passed to igraph::degree()
#' @rdname degree-rnetBasic
#' @export

setMethod('degree',
  signature(graph = 'rnetBasic'),
  function(graph, ...) igraph::degree(graph@R, ...)
  )



#' gsize.RnetBasic() - See igraph documentation for full documentation.
#' @param graph an RnetBasic object
#' @param ... additional arguments passed to igraph::gsize()
#' @rdname gsize-rnetBasic
#' @export

setMethod('gsize',
          signature(graph = 'rnetBasic'),
          function(graph, ...) igraph::gsize(graph@R, ...)
)



#' gorder.RnetBasic() - See igraph documentation for full documentation.
#' @param graph an RnetBasic object
#' @param ... additional arguments passed to igraph::gorder()
#' @rdname gorder-rnetBasic
#' @export

setMethod(
  'gorder',
  signature(graph = 'rnetBasic'),
  function(graph, ...) igraph::gorder(graph@R, ...)
  )



#' edge_density.RnetBasic() - See igraph documentation for full documentation.
#' @param graph an RnetBasic object
#' @param ... additional arguments passed to igraph::gorder()
#' @rdname edge_density-rnetBasic
#' @export

setMethod(
  'edge_density',
  signature(graph = 'rnetBasic'),
  function(graph, ...) igraph::edge_density(graph@R, ...)
)


#' tkplot.RnetBasic() - See igraph documentation for full documentation.
#' @param graph an RnetBasic object
#' @param canvas.width tkplot window canvas width
#' @param canvas.height tkplot window canvas height
#' @param ... additional arguments passed to igraph::tkplot()
#' @rdname tkplot-rnetBasic
#' @export

setMethod(
  'tkplot',
  signature(graph = 'rnetBasic'),
  function(graph, canvas.width = 450, canvas.height = 450, ...) igraph::tkplot(graph@R, canvas.width, canvas.height, ...)
)


#XXX TEMPLATE
#setGeneric(
#  name = 'XXX',
#  def = function(graph, ...) igraph::XXX(graph, ...)
#)
#
#setMethod(
#  'XXX',
#  signature(graph = 'rnetBasic'),
#  function(graph, ...) igraph::XXX(graph@R, ...)
#)


