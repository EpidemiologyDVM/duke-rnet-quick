#' Plot methods for R-nets
#'
#' A plot method for R-nets, and incorporates vertex and edge metadata and layout, if assigns. Only vertex and edge metadata with names that match igraph decoration options (without 'vertex.' or 'edge.' appended to the attribute name; see plot.igraph). layout is pulled from 'layout_master' in the rnet object, if it exists. The layout frame can contain 3 columns, with the first column used to match the coordinates in the next two columns to graph vertices OR can contain 2 columns with the same number of vertices in the graph.
#' @param x an rnet object of class 'rnetBasic'
#' @param draw_plot logial. Will produce plot if TRUE. Only returns function call when FALSE.
#' @param ... additional arguments passed to plot.igraph(). Currently, partial matches are not allowed.
#' @details Extends generic plot() to rnetBasic objects to avoid needing to use plot.igraph(rnetbasic.obj@R). igraph plotting arguments (see ?igraph.plotting) can still be declared and will override the attributes used by default in V(x@R) and E(x@r). Other standard arguments from plot.igraph() are also used.\cr The plot can be drawn automatically, or just the function call to draw the plot later using eval(parse(text = 'plot.call.string')).
#' @return A character string containing the function call to plot the graph (used inside the function generate plot).
#' @import igraph
#' @importFrom rlang call_args
#' @rdname plot-RnetBasic
#' @aliases plot
#' @export
#' @examples 
#' 
#' #Create Rnet object
#' R_EC_08 <- Rnet(x = NARMS_EC_DATA,
#'   L1 = 0.15,
#'   vertices = c('AMP', 'AMC','FOX', 'TIO', 'AXO', 'CIP', 'NAL', 'TET', 'COT', 'FIS'),
#'   subset = expression(Year == 2008)
#'   )
#' 
#' #Plot the network without decoration
#' plot(R_EC_08)
#' 
#' #View the function call and use it to plot the network with plot.igraph() 
#' plot_call <- plot(R_EC_08, draw_plot = FALSE)
#' plot_call
#' eval(parse(text = plot_call))
#' 
#' #Decorate the graph using igraph plotting arguments
#' plot_call_decorated <- plot(R_EC_08, vertex.shape = 'square', vertex.color = 'cyan', edge.width = 3)
#' plot_call_decorated
#' 
#' #Decorate the graph using Assign_Vmetadata() and Assign_Emetadata()
#' Assign_Emetadata(R_EC_08, E_ATTRS, 'omega', e_cuts = c(0, 0.05, 0.10, 0.20, 1))
#' Assign_Vmetadata(R_EC_08, V_ATTRS, 'Code')
#' 
#' plot_call_metadata <- plot(R_EC_08, vertex.frame.color = NA)
#' plot_call_metadata
#' 
#' #Override a previously assigned graphical attribute (vertex.color)
#' plot_call_metadata <- plot(R_EC_08, vertex.frame.color = NA, vertex.color = c('red', 'green'))
#' plot_call_metadata

setMethod(
  f = 'plot',
  signature = signature(x = 'rnetBasic'),
  function (x, draw_plot = TRUE, ...) {
    VERT.PARAMS <- c('size','size2','color','frame.color','shape','label','label.family','label.font','label.cex','label.dist','label.degree','label.color')
    EDGE.PARAMS <- c('color','width','lty','label','label.family','label.font','label.cex','label.color','label.x','label.y','curved')
    OTHER.PARAMS <- c('axes', 'add', 'xlim', 'ylim', 'mark.groups', 'mark.shape', 'mark.col', 'mark.border', 'mark.expand')
    
    OPEN.ARGS <- list(...)
    OPEN.PARAMS <- names(OPEN.ARGS)
    
    calls.src <- sys.calls()
    calls.list <- lapply(calls.src, deparse)
    call.found <- F
    call.search.pos <- length(calls.list) + 1
    while(!call.found) {
      call.search.pos <- call.search.pos - 1
      call.found <- any(grepl('plot', calls.list[[call.search.pos]]))
    }
    args.src <- call_args(sys.call(call.search.pos))
    
    #args.src <- rlang::call_args(sys.call(call.search.pos))
    
    obj.src <- if('x'%in%names(args.src)) deparse(args.src[['x']], width.cutoff = 500L) else deparse(args.src[[min(which(names(args.src)==''))]], width.cutoff = 500L)
    
    plot.args <- character(0)
    for(i in VERT.PARAMS) {
      param.name <- paste('vertex',i, sep = '.')
      if(param.name%in% OPEN.PARAMS) {
        plot.args <- c(plot.args, paste(param.name, '=', deparse(args.src[[param.name]], width.cutoff = 500L)))
        
      } else if(param.name%in% vertex_attr_names(x@R)) {
        plot.args <- c(plot.args, paste(param.name, ' = ', obj.src, '@', param.name, sep = ''))
        
      } else if(i%in%vertex_attr_names(x@R)) {
        plot.args <- c(plot.args, paste(param.name, ' = V(', obj.src, '@R)$', i, sep = ''))
      }
    }
    
    for(i in EDGE.PARAMS) {
      param.name <- paste('edge', i, sep = '.')
      if(param.name%in% OPEN.PARAMS) {
        plot.args <- c(plot.args, paste(param.name, '=', deparse(args.src[[param.name]], width.cutoff = 500L)))
        
      } else if(param.name%in% vertex_attr_names(x@R)) {
        plot.args <- c(plot.args, paste(param.name, ' = ', obj.src, '@', param.name, sep = ''))
        
      } else if(i%in%vertex_attr_names(x@R)) {
        plot.args <- c(plot.args, paste(param.name, ' = E(', obj.src, '@R)$', i, sep = ''))
      }
    }
    
    for(i in OTHER.PARAMS) if(i%in%OPEN.PARAMS) plot.args <- c(plot.args, paste(i, "=", deparse(args.src[[i]], width.cutoff = 500L)))
    
    if('layout'%in%OPEN.PARAMS) plot.args <- c(plot.args, paste('layout = ', deparse(args.src$layout, width.cutoff = 500L))) else plot.args <- c(plot.args, paste('layout = x@layout', sep = ''))
    
    call.int <- paste('plot.igraph(x@R, ', paste(plot.args, collapse = ', '), ')', sep = '')
    
    if(draw_plot) eval(parse(text = call.int))
    call.ext <- gsub('x@', paste(obj.src, '@', sep = ''), call.int)
    invisible(call.ext)
  })


#' Hidden function for assigning layout matrix
#' @param x an Rnet object
#' @rdname dot-Assign_Layout_Matrix
#' 
.Assign_Layout_Matrix <- function(x){ 
  if(is.null(x@layout_master)) return(layout_with_fr(x@R))
	if(dim(x@layout)[1]==0) return(layout_with_fr(x@R))
  
  if(dim(x@layout_master)[2] == 3) {
    coord_ref.vec <- x@layout_master[,1]
    coord_x.vec <- x@layout_master[,2]
    coord_y.vec <- x@layout_master[,3]
  } else if (dim(x@layout_master)[2] == 2){
    coord_ref.vec <- NULL
    coord_x.vec <- x@layout_master[,1]
    coord_y.vec <- x@layout_master[,2]
  } else stop('x@layout_master invalid! Must have 2 or 3 columns')

  if(!is.numeric(coord_x.vec)|!is.numeric(coord_y.vec)) stop('layout_master not valid: Coordinate columns must be numeric')
  
  if(is.factor(coord_ref.vec)) coord_ref.vec <- as.character(coord_ref.vec)
  if(is.null(coord_ref.vec)) {
    if(length(x@vertices)!= length(coord_x.vec)) stop('x@layout_master not valid: Number of rows in layout frame must match number of vertices in graph OR a column for vertex matching must be provided.')
    layout.mat <- cbind(coord_x.vec, coord_y.vec)

    dimnames(layout.mat) <- list(V(x@R)$name,c('x', 'y'))
    } else {
      if(!all(V(x@R)$name%in%coord_ref.vec)) stop('x@layout_master not valid: Vertex names are missing from the first column.')
      seq.vec <- match(V(x@R)$name,coord_ref.vec)
	    layout.mat <- cbind(coord_x.vec[seq.vec], coord_y.vec[seq.vec])
	    dimnames(layout.mat) <- list(V(x@R)$name[seq.vec],c('x', 'y'))
    }   
	return(layout.mat)
}

#' image() method for plotting Rnet heatmaps
#' @param x An object with class(x) = 'edge_heatmap', produced by Rnet_Heatmap()
#' @param axes logical; if TRUE add axes to the heatmap plot. 
#' @param ... graphical parameters to be passed to image.default() inside this method.
#' @rdname image.edge_heatmap
#' @aliases image
#' @export

image.edge_heatmap <- function(x, axes = TRUE, ...) 
{
  pal_colors <- attr(x, 'palette')
  image.default(x = x, col = pal_colors, axes = FALSE, breaks = 0:length(pal_colors), ...)
  if(axes) {
    axis(1, at = seq(0, 1, 1/(dim(x)[1]-1)), labels = rownames(x), tck = -0.02)
    axis(2, at = seq(0, 1, 1/(dim(x)[2]-1)), labels = colnames(x), tck = -0.015, las = 2, cex.axis = 10/12)
  }  
}

#' plot() method for summarizing L1 StARS section objects 
#' 
#' @param x An rnet.L1.set object
#' @param ylim plot(ylim) for stability plot
#' @param ylim_m plot(ylim) for density plot
#' @param col plot(col) for stability plot
#' @param col_m plot(col) for density plot#' 
#' @param lty plot(lty) for stability plot
#' @param lty_m plot(lty) for density plot, defaults to lty
#' @param lwd plot(lwd) for stability plot
#' @param lwd_m plot(lwd) for density plot, defaults to lwd
#' @param mar margins for plot.
#' @param ... Other arguments passed to plot
#' @rdname plot-rnet.L1.set
#' @import graphics
#' @aliases plot.rnet.L1.set
#' @export
#' 
#' 
#' 

setMethod(
  'plot',
  signature(x = 'rnet.L1.set'),
  function (
    x, 
    ylim = c(0, 0.25), 
    ylim_m = c(0, 0.5),
    col = 'blue',
    col_m = 'red',
    lty = 1,
    lty_m = NULL,
    lwd = 2,
    lwd_m = NULL,
    mar = c(5, 4, 4, 4),
    ...
  ) {
    if(is.null(lty_m)) lty_m <- lty
    if(is.null(lwd_m)) lwd_m <- lwd
    
    par(mar = mar)
    plot(
      x = as.numeric(names(x@D)),
      y = x@D,
      xlim = c(0, max(as.numeric(names(x@D)))),
      ylim = ylim,
      xlab = expression(L[1]),
      ylab = expression("Stability "(D[b])),
      col = col,
      axes = F,
      ...
      )
    
    lines(
      x = as.numeric(names(x@D)),
      y = x@D,
      xlim = c(0, max(as.numeric(names(x@D)))),
      col = col,
      ylim = ylim,
      lty = lty,
      lwd = lwd
      )
    
    abline(h = x@D_thresh, lty = 2, lwd = 2, col = 'darkgrey')
    
    axis(1)
    axis(2)
    par(new = TRUE)
    
    plot(
      x = as.numeric(names(x@D)),
      y = aggregate(m/choose(dim(x@W_aggr)[[1]],2) ~ L1, data =  x@M, FUN = mean)[,2],
      xlim = c(0, max(as.numeric(names(x@D)))),
      ylim = ylim_m,
      xlab = expression(L[1]),
      ylab = '',
      axes = F,
      col = col_m,
      type = 'l',
      lwd = lwd
      )
    
    axis(4)
    mtext(
      text = expression(bar(m)),
      side = 4,
      line = 3
      )
    
    legend(
      x = 'topright',
      legend = c('Stability (D_b)', 'Mean Density (m-bar)'),
      col = c(col, col_m),
      lty = lty,
      lwd = lwd
      )
    
  })
