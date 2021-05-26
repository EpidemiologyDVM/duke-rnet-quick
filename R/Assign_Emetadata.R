#'Assign_Emetadata - Methods for assigning network edge metadata
#'
#' This method assigns metadata to edges in an igraph object based on a continuous edge attribute already present in object, typically called with E(graph)$attr. This method is designed to be a convenient alternative to repeated calls to 'E(graph)$attr <-  value' or 'set_edge_attr' to apply multiple attributes that vary on a single edge attribute. The attribute used to assign the other metadata attributes is typically assumed to be the value used to define the edge to begin with, so when the attribute is 0, it is typically assumed the edge doesn't exist and is omitted from the edge set.
#'
#' This method also works with all rnet objects (currently class 'rnet.basic', 'rnet.strata', and 'rnet.multi.strata'), and also adds the names of the metadata attributes to the 'E_metadata' slot.

#' @param x The network to which the edge metadata will be applied.
#' @param E_metadata A dataframe containing one column for each attribute to be assigned to the network edges.
#' @param match_attr The continuous attribute assigned to the network's edges (typically by E(x)$attr or set_edge_attr(x, attr)) that will be used to categorize edges. Values in match_attr will be binned using cut(value, E_ctpts), and matched to the rows in E_metadata, i.e. edges binned into the first category will be assigned attribute values from the first row in the data set, those binned into the second category will be assigned the second row values, etc.
#' @param e_cuts A vector of values used to define how the values of match_attr will be binned. See 'cut' for more information.
#' @param sign_col By default, a two-element vector containing colors for edges representing positive and negative attribute values(black and red, respectively). This behavior will be overriden if this argument is set to FALSE, NA, or NULL. Note, edge_attr('color') will ALWAYS be assinged to network edges when this method is called.
#' @param attr_abs_val A logical argument determining if the absolute value of match_attr is used when binning values.
#' @param reassign A logical argument controling if the function should overwrite x in the parent environment. Defaults to TRUE for brevity.
#' @import igraph
#' @include classes.R
#' @return An object of the same type as x, with the new edge attributes assigned by binning the 'match_attr' in the igraph and assigning the matching rows in 'e.metadata'.

#' @export
#' @examples
#'
#' #'E_ATTRS' is a data.frame included in the package containing line type and weight  
#' #     for plotting edges ('lty' & 'width', respectively) that represent partial
#' #     correlation magnitude and sign held in attribute 'omega'. 
#' 
#' ABX_LIST <- c('AMP', 'AMC', 'AXO', 'TIO', 'NAL', 'CIP', 'STR', 'GEN', 'COT', 'FIS')
#' 
#' EC08_rnet <- Rnet(NARMS_EC_DATA, 
#'   L1 = 0.25, 
#'   vert = ABX_LIST, 
#'   subset = NARMS_EC_DATA$Year == 2008
#'   )
#' 
#' #Attributes prior to additions
#' edge_attr_names(EC08_rnet@R)
#' edge_attr(EC08_rnet@R)
#' 
#' OMEGA_CUTS <- c(0, 0.05, 0.10, 0.20, 1) #Cutpoints to sort abs(omega) into 4 bins
#' 
#' Assign_Emetadata(EC08_rnet, 
#'                  E_metadata = E_ATTRS,
#'                  match_attr = 'omega',
#'                  e_cuts = OMEGA_CUTS
#'                  )
#' 
#' #NOTE: EC08_rnet does not need to be reassigned for brevity. Returns data.frame of assigned data.
#' #      Reassignment can be performed, if desired. data.frame not returned in such a case.
#' 
#' EC08_withAttrs <- Assign_Emetadata(EC08_rnet, 
#'                  E_metadata = E_ATTRS,
#'                  match_attr = 'omega',
#'                  e_cuts = OMEGA_CUTS
#'                  )
#' 
#' #Atrributes after edges assigned.
#' edge_attr(EC08_rnet@R)
#' 
#' #NOTE: Edge color assigned as per default behavior.
#' @rdname Assign_Emetadata
setGeneric('Assign_Emetadata',
	function(x, E_metadata, match_attr, e_cuts = NULL, sign_col = c('black', 'red'), attr_abs_val = TRUE, reassign = TRUE)
	{
	  calls.src <- sys.calls()
	  calls.list <- lapply(calls.src, deparse)
	  call.found <- F
	  call.search.pos <- length(calls.list) + 1
	  while(!call.found) {
	    call.search.pos <- call.search.pos - 1
	    call.found <- any(grepl('Emetadata', calls.list[[call.search.pos]]))
	  }
	  
	  args.src <- rlang::call_args(sys.call(call.search.pos))
	  obj.src <- if('x'%in%names(args.src)) deparse(args.src[['x']], width.cutoff = 500L) else deparse(args.src[[min(which(names(args.src)==''))]], width.cutoff = 500L)
	  source.env <- parent.frame()
	  
		if(!match_attr%in%edge_attr_names(x)) stop(paste("Edge attribute '", match_attr, "' not found in x", sep = ''))
		if(!is.numeric(edge_attr(x, match_attr))) stop("match_attr is not numeric")
		if(attr_abs_val) match_attr_vals <- abs(edge_attr(x, match_attr)) else match_attr_vals <- edge_attr(x, match_attr)
		if(is.null(e_cuts)) e_cuts <- seq(min(0, min(match_attr_vals)), max(match_attr_vals), max(match_attr_vals)/dim(E_metadata)[1])
		E_cat <- cut(match_attr_vals, e_cuts)
		for(attr in names(E_metadata)) x <- set_edge_attr(x, attr, value = E_metadata[E_cat,attr])
		if(!'color'%in%names(E_metadata)) if(sign_col[1] == FALSE|is.null(sign_col)[1]|is.na(sign_col)[1]) {} else {
			E(x)$color <- sign_col[1]; E(x)$color[sign(edge_attr(x, match_attr))==-1] <- sign_col[2]
			}

		if(reassign){
		  assign(obj.src,	x, source.env)
			return(as.data.frame(edge_attr(x@R)))
		}
		return(x)
	})

#' @rdname Assign_Emetadata
#' 
setMethod('Assign_Emetadata',
	signature(x = 'rnetBasic'),
	function(x, E_metadata, match_attr, e_cuts = NULL, sign_col = c('black', 'red'), attr_abs_val = TRUE, reassign = TRUE) 
	{	  
	  calls.src <- sys.calls()
  	calls.list <- lapply(calls.src, deparse)
	  call.found <- F
	  call.search.pos <- length(calls.list) + 1
	  while(!call.found) {
	    call.search.pos <- call.search.pos - 1
	    call.found <- any(grepl('Emetadata', calls.list[[call.search.pos]]))
	  }
	
  	args.src <- rlang::call_args(sys.call(call.search.pos))
  	obj.src <- if('x'%in%names(args.src)) deparse(args.src[['x']], width.cutoff = 500L) else deparse(args.src[[min(which(names(args.src)==''))]], width.cutoff = 500L)
  	source.env <- parent.frame()
	
		x@R <- Assign_Emetadata(x = x@R, E_metadata = E_metadata, match_attr, e_cuts, sign_col, attr_abs_val, F)
		x@E_metadata <- names(E_metadata)

		if(reassign){
		  assign(obj.src,	x, source.env)
		  return(as.data.frame(edge_attr(x@R)))
		}
		return(x) 

	})

#' @rdname Assign_Emetadata
#' 
setMethod('Assign_Emetadata',
	signature(x = 'rnetStrata'),
	function(x, E_metadata, match_attr, e_cuts = NULL, sign_col = c('black', 'red'), attr_abs_val = TRUE, reassign = TRUE) 
	{
	  calls.src <- sys.calls()
	  calls.list <- lapply(calls.src, deparse)
	  call.found <- F
	  call.search.pos <- length(calls.list) + 1
	  while(!call.found) {
	    call.search.pos <- call.search.pos - 1
	    call.found <- any(grepl('Emetadata', calls.list[[call.search.pos]]))
	  }
	  
	  args.src <- rlang::call_args(sys.call(call.search.pos))
	  obj.src <- if('x'%in%names(args.src)) deparse(args.src[['x']], width.cutoff = 500L) else deparse(args.src[[min(which(names(args.src)==''))]], width.cutoff = 500L)
	  source.env <- parent.frame()
	  
		slot(x, "R_set") <- lapply(slot(x, "R_set"), Assign_Emetadata, E_metadata, match_attr, e_cuts, sign_col, attr_abs_val, FALSE)
		
		if(reassign){
		  assign(obj.src,	x, source.env)
		  message('Edge attributes assigned to original stratified R-net object')
		}
		return(x) 
	})