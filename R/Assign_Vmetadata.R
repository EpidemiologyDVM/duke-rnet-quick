#'Assign_Vmetadata - Methods for assigning network vertex metadata
#'
#' This method assigns metadata to vertices in an igraph object based on an existing vertex attibute, typically vertex name, but can be done with other attributes including those assigned with previous calls with this method. 
#'
#' This method also works with all rnet objects (currently class 'rnetBasic', 'rnetStrata', and 'rnet.multi.strata'), and also adds the names of the metadata attributes to the 'V_metadata' slot.
#'
#' @param x The network to which the vertex metadata will be applied.
#' @param V_metadata A dataframe containing the vertex metadata to be assigned. A vertex attribute will be assigned for every column in the frame, except the column used to match V_metadata to existing vertex attribues.
#' @param match_attr The name of the column in V_metadata used to match metadata to vertices. Defaults to the first column of V_metadata.
#' @param V_match_attr the name of the vertex attribute used to match metadata. Defaults to 'name' (V(x)$name), which is typically assigned when the network is created with igraph functions.
#' @param reassign A logical argument controling if the function should overwrite x in the parent environment. Defaults to 'True' for brevity.
#' @import igraph
#' @return An object of the same type as x, with the new vertex attributes assigned by matching 'match_attr' to 'V_match_attr'.
#' @rdname Assign_Vmetadata
#' @include classes.R
#' @examples 
#' # V_ATTRS' is a data.frame included in the package containing vertex metadata
#' #     regarding antimicrobial class and a color scheme for the vertices. These 
#' #     attributes are useful for plotting and determining modularity.
#' 
#' ABX_LIST <- c('AMP', 'AMC', 'AXO', 'TIO', 'NAL', 'CIP', 'STR', 'GEN', 'COT', 'FIS')
#' 
#' EC08_rnet <- Rnet(NARMS_EC_DATA, 
#'   L1 = 0.25, 
#'   vertices = ABX_LIST, 
#'   subset = NARMS_EC_DATA$Year == 2008
#'   )
#' 
#' #Attributes prior to additions
#' vertex_attr_names(EC08_rnet@R)
#' vertex_attr(EC08_rnet@R)
#' 
#' Assign_Vmetadata(EC08_rnet, 
#'                  V_metadata = V_ATTRS,
#'                  match_attr = 'Code',
#'                  V_match_attr = 'name'
#'                  )
#' 
#' #NOTE: EC08_rnet does not need to be reassigned for brevity. Returns data.frame of assigned data.
#' #      Reassignment can be performed, if desired. data.frame not returned in such a case.
#' 
#' EC08_withAttrs <- Assign_Vmetadata(EC08_rnet, 
#'                  V_metadata = V_ATTRS,
#'                  match_attr = 'Code',
#'                  V_match_attr = 'name'
#'                  )
#' 
#' #Atrributes after edges assigned.
#' vertex_attr(EC08_rnet@R)

#' @export

setGeneric('Assign_Vmetadata',

	function(x, V_metadata, match_attr = NULL, V_match_attr = 'name', reassign = TRUE)
	{
	  calls.src <- sys.calls()
	  calls.list <- lapply(calls.src, deparse)
	  call.found <- F
	  call.search.pos <- length(calls.list) + 1
	  while(!call.found) {
	    call.search.pos <- call.search.pos - 1
	    call.found <- any(grepl('Vmetadata', calls.list[[call.search.pos]]))
	  }
	  
	  args.src <- rlang::call_args(sys.call(call.search.pos))
	  obj.src <- if('x'%in%names(args.src)) deparse(args.src[['x']], width.cutoff = 500L) else deparse(args.src[[min(which(names(args.src)==''))]], width.cutoff = 500L)
	  source.env <- parent.frame()
	  
		if(is.null(match_attr)) {
			match_attr <- names(V_metadata)[1]
			warning("No column containing vertex names declared, first column of V_metadata is assumed to contain vertex names")
		}								#Assigns first column of metadata table as matching column if none defined and returns warning. 
		if(any(duplicated(V_metadata[match_attr]))) stop("Elements of matching column 'match_attr' in must be unique")
		if(!match_attr%in%names(V_metadata)) stop(paste('Column', match_attr, 'not found in V_metadata'))
										#Returns error if assigned matching column in met

		if(!all(vertex_attr(x, V_match_attr)%in%unlist(V_metadata[match_attr]))) stop('Not all vertices appear in V_metadata$', match_attr, sep = '')
		match.vec <- match(vertex_attr(x, V_match_attr), V_metadata[[match_attr]])

		attr.frame <- data.frame(V = vertex_attr(x, 'name'))

		for(attrib in names(V_metadata)[!names(V_metadata)%in%match_attr]) {
			x <- set_vertex_attr(x, attrib, value = V_metadata[match.vec, attrib])
			attr.frame[[attrib]] <- V_metadata[match.vec, attrib]
		}
		
		if(reassign) {
		  assign(obj.src,	x, source.env)
			return(as.data.frame(vertex_attr(x)))
		}
		return(x)
	})




#' @rdname Assign_Vmetadata
#'
setMethod('Assign_Vmetadata',
	signature(x = 'rnetBasic'),

	function(x, V_metadata, match_attr = NULL, V_match_attr = 'name', reassign = TRUE)
	{
	  calls.src <- sys.calls()
	  calls.list <- lapply(calls.src, deparse)
	  call.found <- F
	  call.search.pos <- length(calls.list) + 1
	  while(!call.found) {
	    call.search.pos <- call.search.pos - 1
	    call.found <- any(grepl('Vmetadata', calls.list[[call.search.pos]]))
	  }
	  
	  args.src <- rlang::call_args(sys.call(call.search.pos))
	  obj.src <- if('x'%in%names(args.src)) deparse(args.src[['x']], width.cutoff = 500L) else deparse(args.src[[min(which(names(args.src)==''))]], width.cutoff = 500L)
    source.env <- parent.frame()
    
	  x@R <- Assign_Vmetadata(x = slot(x, "R"), V_metadata, match_attr, V_match_attr, F)
		x@V_metadata <- names(V_metadata)

		if(reassign) {
		  assign(obj.src,	x, source.env)
		  return(as.data.frame(vertex_attr(x@R)))
		}
		return(x)
	})

#' @rdname Assign_Vmetadata
#'	
setMethod('Assign_Vmetadata',
	signature(x = 'rnetStrata'),

	function(x, V_metadata, match_attr = NULL, V_match_attr = 'name', reassign = TRUE)
	{
	  calls.src <- sys.calls()
	  calls.list <- lapply(calls.src, deparse)
	  call.found <- F
	  call.search.pos <- length(calls.list) + 1
	  while(!call.found) {
	    call.search.pos <- call.search.pos - 1
	    call.found <- any(grepl('Vmetadata', calls.list[[call.search.pos]]))
	  }
	  
	  args.src <- rlang::call_args(sys.call(call.search.pos))
	  obj.src <- if('x'%in%names(args.src)) deparse(args.src[['x']], width.cutoff = 500L) else deparse(args.src[[min(which(names(args.src)==''))]], width.cutoff = 500L)
	  source.env <- parent.frame()
		slot(x, "R_set") <- lapply(slot(x, "R_set"), Assign_Vmetadata, V_metadata, match_attr, V_match_attr, reassign = FALSE)
		if(reassign) {
		  assign(obj.src,	x, source.env)
		  message('Vertex attributes assigned to original stratified R-net object')
		}
		return(x)
	})