#' R-net Methods
#'
#' This method takes an dataset (typically containing MICs values, AMR phenotypes, and presence abscence of genes) and returns an rnet object. The specific object class that is returned varies by what is provided to the L1 and subset arguments. The networks are Markov random fields (MRFs), a type of undirected graph. The network structure/topology is estimated using the graphical least absolute shrinkage and selection operator (glasso) as implemented in the R package of the same name developed by Friedman, Hastie, & Tibshirani (maintained by the latter) 
#' @param x The dataset used to estimate the structure of the Rnet. 
#' @param L1 The L1 penalty used by the graphical LASSO to create a sparse precision matrix, also referred to as 'rho'. Must be non-negative.
#' @param vertices A character vector corresponding to the names of the antibiotics to include in the Rnet. Defaults to an empty list, in which case one vertex will be included for each column in x. If declared, only variables declared in 'vertices' will be represented with vertices in the MRF. 
#' @param n_min The minimum number of observations required for an an estimated correlation to be valid. Defaults to 0, in which case any number of observations will be sufficient to estimate a valid correlation. If a vertex/variable has fewer valid observations than n_min, the vertex will be fully omitted from the network.
#' @param cor_method The method used to estimate the correlation matrix. Must be 'pearson', 'spearman', or 'kendall'. Partial matches allowed. Defaults to 'spearman'.
#' @param cor_pairing The method used to determine how NAs are handled when determining which pairs are used to estimate correlations. See 'cor' function documentation for additional information.
#' @param forced_zeros The set of edges to be omitted from the Rnet. These partial correlations are forced to zero. Additional edges and vertices may be set to zero if n_min is employed.
#' @param plot_layout A dataframe of two or three columns. See plot methods for more information.
#' @param subset Either a character variable of length one or expression. If a character value is supplied, it must match a column name in x and an object of type 'rnetMultiStrata' with a network for each level of the declared variable. If an expression is supplied, an object of 'rnetStrata' will be returned with the network estimated from a subset of x defined by the expression. If no value is supplied, an object of 'rnetBasic' will be returned with the network estimated from all observations in x.
#' @return An rnet object containing the graphical LASSO results. The specific type of object is determined by the 'subset' argument.
#' @import methods
#' @import data.table
#' @import igraph
#' @export
#' @examples
#' #Create a single R-net for all E. coli isolates in the provided dataset. 
#' #Vertices to be used defined by 'ABX_LIST' below.
#' #Edges require at least 20 observations to be valid.
#' 
#' ABX_LIST <- c('AMP', 'AMC', 'AXO', 'TIO', 'NAL', 'CIP', 'STR', 'GEN', 'COT', 'FIS')
#' 
#' EC_Rnet_ALL <- Rnet(x = NARMS_EC_DATA, 
#' 						L1 = 0.3, 
#' 						vertices = ABX_LIST, 
#' 						n_min = 20
#' 						)
#' class(EC_Rnet_ALL)[1]	#EC_Rnet_ALL is a 'rnetBasic' object
#' print(EC_Rnet_ALL)	#Basic Rnet information
#' summary(EC_Rnet_ALL) 	#More detailed information
#' 
#' #Create a single R-net for only E. coli isolates collected during 2008
#' EC_Rnet_2008 <- Rnet(x = NARMS_EC_DATA, 
#' 						L1 = 0.3, 
#' 						vertices = ABX_LIST, 
#' 						n_min = 20,
#'						subset = expression(Year == 2008)
#' 						)
#' class(EC_Rnet_2008)[1]	#EC_Rnet_ALL is an 'rnet.stratum' object
#'
#' #Create a set of R-nets, one for each year of E.coli isolates.
#' EC_Rnet_byYear <- Rnet(x = NARMS_EC_DATA, 
#' 						L1 = 0.3, 
#' 						vertices = ABX_LIST, 
#' 						n_min = 20,
#'						subset = 'Year'
#' 						)
#' class(EC_Rnet_byYear)[1]	#EC_Rnet_ALL is an 'rnetMultiStrata' object

#' @rdname Rnet
#' 
setGeneric('Rnet',
	function(								#Cornerstone Function to generate an Rnet from data
			x, #WAS MIC_data
			L1,
			vertices = NULL,
			n_min = 1,
			cor_method = 's',
			cor_pairing = 'pair',
			forced_zeros = NULL,				
			plot_layout	= NULL,
			subset = NULL		
			)
	{
		rnet.obj <- new("rnetBasic",
			raw_data = x,
			cor_method = cor_method,
			cor_pairing = cor_pairing,
			n_min = n_min,
			L1_orig = L1,
			V_orig = if(is.null(vertices)) vertices = names(x) else vertices,
			forced_zeros = if(is.null(forced_zeros)) matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("V1", "V2"))) else forced_zeros,
			layout_master = plot_layout
			)
		return(.Gen_R(rnet.obj))
	})

#' @rdname Rnet
#' 
setMethod('Rnet',
	signature(subset = 'expression'),
	function(							
			x,
			L1,
			vertices = NULL,
			n_min = 1,
			cor_method = 's',
			cor_pairing = 'pair',
			forced_zeros = NULL,				
			plot_layout	= NULL,
			subset		
			)
	{
		rnet.obj <- new("rnetSubset",
			raw_data = x,
			cor_method = cor_method,
			cor_pairing = cor_pairing,
			n_min = n_min,
			L1_orig = L1,
			V_orig = if(is.null(vertices)) vertices = names(x) else vertices,
			forced_zeros = if(is.null(forced_zeros)) matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("V1", "V2"))) else forced_zeros,
			layout_master = plot_layout,
			subset = subset
			)
		return(.Gen_R(rnet.obj))
	})

#' @rdname Rnet
#' 
setMethod('Rnet',
	signature(subset = 'character'),
	function(							
			x,
			L1,
			vertices = NULL,
			n_min = 1,
			cor_method = 's',
			cor_pairing = 'pair',
			forced_zeros = NULL,				
			plot_layout	= NULL,
			subset		
			)
	{
	  if(!subset%in%names(x)) stop(paste("Invalid stratification: '",  subset, "' does not appear in dataset"), sep = '')
	  V_orig <- vertices
		if(subset%in%vertices) {vertices <- vertices[-match(subset, V_orig)]; warning(paste("Stratification variable cannot appear in declared vertex set.", subset,"was removed from vertices"))}
		rnet.obj <- new("rnetStrata",
			raw_data = x,
			cor_method = cor_method,
			cor_pairing = cor_pairing,
			n_min = n_min,
			L1_orig = L1,
			V_orig = if(is.null(vertices)) vertices = names(x) else vertices,
			forced_zeros = if(is.null(forced_zeros)) matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("V1", "V2"))) else forced_zeros,
			layout_master = if(is.null(plot_layout)) data.frame(V1 = numeric(0), V2 = numeric(0) ) else plot_layout,
			stratify_by = subset
			)

		strata_vals <- unique(x[[subset]])
		src.obj <- as(rnet.obj, 'rnetInput')
		strat.obj <- as(src.obj, 'rnetSubset')
		rnet.obj@R_set <- lapply(strata_vals, function(x) {strat.obj@subset <- parse(text = paste(subset, '==', x)); .Gen_R(strat.obj)})
		names(rnet.obj@R_set) <- strata_vals
		rnet.obj@E_aggr <- .Aggregate_Edges(rnet.obj, 'omega')
		return(rnet.obj)
	})


.Valid_Strat_Expr <- function(rnet.obj) 
{
	expr.str <- as.character(rnet.obj@subset)
	if(grepl("\\|", expr.str)|grepl('&', expr.str)) stop("Stratification on multiple criteria not yet supported")
	expr.parts <- unlist(strsplit(expr.str, ' '))
	if(!expr.parts[1]%in%names(rnet.obj@raw_data)) stop(paste("Stratification variable '", expr.parts[1], "' does not appear in data set."), sep = '')
	if(!any(eval(rnet.obj@subset, rnet.obj@raw_data))) stop(paste("Declared value does not appear in Stratification variable", expr.parts[1], "."), sep = '')
}

.Aggregate_Edges <- function(rnet.list, e_attr, round_output = 3) {
	strata.var <- rnet.list@stratify_by
	rnet.list <- rnet.list@R_set
	edgelist <- as.data.frame(data.table::rbindlist(lapply(rnet.list, function(x) {
		edges <- as.data.frame(as_edgelist(x@R))
		edges[[e_attr]] <- round(edge_attr(x@R, e_attr), round_output)
		def.string <- unlist(strsplit(as.character(x@subset), ' == '))
		edges[[strata.var]] <- def.string[2]
		return(edges)}
		)))
	edgematr <- stats::reshape(edgelist,
		timevar = strata.var,
		idvar = c('V1', 'V2'),
		dir = 'wide'
		)
	edgematr[is.na(edgematr)] <- 0
	row.names(edgematr) <- paste(edgematr$V1, edgematr$V2, sep = '--')
	names(edgematr) <- gsub(e_attr, strata.var, names(edgematr))
	
	for(i in 1:length(rnet.list)) {
		edgematr[edgematr[[1]]%in%rnet.list[[i]]@V_omitted|edgematr[[2]]%in%rnet.list[[i]]@V_omitted,i+2] <- NA
		if(dim(rnet.list[[i]]@zeros$Invalid)[1]>0) invalid.edges <- with(data = rnet.list[[i]]@zeros$Invalid, expr = paste(V1, V2, sep = '--')) else invalid.edges <- character(0)
		edgematr[row.names(edgematr)%in%invalid.edges,i+2] <- NA
		}

	return(as.matrix(edgematr[,-c(1, 2)]))
}