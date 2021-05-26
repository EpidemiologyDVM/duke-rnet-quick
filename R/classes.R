#' An S4 class for accepting input data common for generating all rnet objects. These objects are used to handle rnet objects and need not be called by the user.
#' 
#' @slot raw_data A dataframe containing the original dataset.
#' @slot cor_method The type of correlation matrix to use. Must be a partial match to one of the following strings: 'pearson', 'spearman', 'kendall'.
#' @slot cor_pairing Method for handling how missing data is handled in pairs. See 'pair' argument in function 'cor' for more information.
#' @slot n_threshold The minimum number of valid pair-wise observations that must exist for an edge to be estimated. Vertex pairs with fewer valid pair-wise observations are assumed to be conditiontally independent.
#' @slot L1_orig The declared L1 penalty to be used when estimating rnet topology
#' @slot V_orig The declared set of k variables to be included in the rnet as vertices
#' @slot forced_zeros A matrix with 2 columns containing pairs of vertices to force to be conditionally independent in the rnet
#' @slot layout_master A k x 2 matrix x & y coordinates of each vertex in the graph.
#' @rdname rnetInput

rnetInput <- setClass(Class = "rnetInput",
	slots = c(
		raw_data = 'data.frame',
		cor_method = 'character',
		cor_pairing = 'character',
		n_min = 'numeric',
		L1_orig = 'numeric',
		V_orig = 'character',
		forced_zeros = 'matrix',
		layout_master = 'ANY'
		)
	)



#' An S4 class containg the information of a basic rnet. These objects are used to handle rnet objects and need not be called by the user.
#' 
#' Inherits from 'rnetInput'
#' @slot x A dataframe containing the dataset.
#' @slot L1 The L1 penalty
#' @slot vertices The vertex set
#' @slot n A square matrix of n_ij, the number of valid pairs used to estimate the edge between i and j.
#' @slot V_omitted The names of vertices removed from the rnet due to low sample size or forced to 0.
#' @slot V_metadata A Vector containing the attribute names assigned to the rnet's vertices.
#' @slot E_metadata A vector containing the attribute names assinged to the rnet's edges.
#' @slot zeros A list of edges forced to zero due to small sample size or declared by user.
#' @slot Sigma The empirical correlation matrix
#' @slot Theta The penalized precision matrix (Sigma^-1)
#' @slot Omega The penalized partial correlation matrix. Also the weighted adajacency matrix used for the network.
#' @slot A The adjacency matrix (omega_ij != 0)
#' @slot R An igraph object derived from Omega
#' @slot layout A k' x 2 matrix containing the x & y coords for the vertices for plotting. k' indicates that some of the k vertices declared may have been removed, typically due to a variable having fewer than n_min observations.
#' @rdname rnetBasic
#' 
rnetBasic <- setClass(Class = "rnetBasic",
	slots = list(
		x = 'data.frame',
		L1 = 'numeric',
		vertices = 'character',
		n = 'matrix',
		V_omitted = 'character',
		V_metadata = 'character',
		E_metadata = 'character',
		zeros = 'list',
		Sigma = 'matrix',
		Theta = 'matrix',
		Omega = 'matrix',
		A = 'matrix',
		loglik = 'numeric',
		R = 'ANY',
		layout = 'matrix'
		),
	contains = 'rnetInput'
	)

#' An S4 class containing the information of an rnet representing one stratum of data. These objects are used to handle rnet objects and need not be called by the user.
#'
#' Inheirits from 'rnetBasic'
#' @slot subset The expression used to define the stratum of data represented by the rnet, e.g., if Statra_def = as.expression(Year == 2008), the rnet is estimated from the subset of data collected in the Year 2008.
#' @rdname rnetSubset
rnetSubset <- setClass(Class = "rnetSubset",
	slots = list(subset = 'expression'),
	contains = 'rnetBasic'
	)



#' An S4 class containing multiple Rnets, each the information of an rnet representing one stratum of data. These objects are used to handle rnet objects and need not be called by the user.
#'
#' Inheirits from 'rnetInput'
#' @slot stratify_by The name of the variable used to the original data, e.g. if Stratify_by = 'Year', the object contains 1 rnetSubset object for each unique value of the variable "year" in the dataset.
#' @slot E_aggr A matrix containing every edge found in the entire set of rnets, and the stratum in which it was found.
#' @slot R_set A list of rnetStrata objects for each strata of the declared Stratify_by variable.
#' @rdname rnetStrata

rnetStrata <- setClass(Class = "rnetStrata",
	slots = list(
		stratify_by = 'character',
		E_aggr = 'matrix',
		R_set = 'list'
		),
	contains = 'rnetInput'
	)



#' An S4 class for tracking multiple rnets created from one data set over multiple L1 penalties for comparison and selection.
#' 
#' See "L1_selection" documentation for more information.
#' @slot x A dataframe containing the dataset
#' @slot L1_values a numeric vector containing the candidate L1 penalties
#' @slot B The number of subsamples to draw from the data to evaluate topologic stability 
#' @slot method_b Assigned either "proportionate" or "Total number" depending on how subsample size is determined.
#' @slot sets_b A matrix (n_B x B) containing the rownumbers of the subsamples
#' @slot array_b An arrary (n_b x k x B) containing the data from the B_sets matrix
#' @slot pr_b The size of the subsample B as a proportion of the complete dataset
#' @slot n_b The size of the subsample
#' @slot W_aggr An array (k x k x B x L1) containg all the weighted adjacency matrices generated by the all of subsamples over the L1 penalties
#' @slot A_aggr An array (k x k x B x L1) containg all the adjacency matrices generated by the all of subsamples over the L1 penalties
#' @slot M A dataframe with with graphical density data over the set of generated networks.
#' @slot stability A dataframe showing edge stability over the set of generated networks.
#' @slot D A vector of D_b values used for L1 selection.
#' @slot D_thresh The suggested maximum D value for selection.
#' @rdname rnetInput
#' 
rnet.L1.set <- setClass(Class = 'rnet.L1.set',
	slots = list(
		x = 'data.frame',
		L1_values = 'numeric',
		B = 'numeric',
		method_b = 'character',
		sets_b = 'matrix',
		array_b = 'array',
		pr_b = 'numeric',
		n_b = 'numeric',
		W_aggr = 'array',
		E_aggr = 'data.frame',
		M = 'data.frame',
		stability = 'data.frame',
		D = 'numeric',
		D_thresh = 'numeric'
		)
	)