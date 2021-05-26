#'Print methods of rnet objects.
#'
#'print methods for classes 'rnetBasic', 'rnetStrata', 'rnet.multi.strata'.
#' @param x An rnet object
#' @rdname print
#' @export
#' 
setMethod(f = 'print',
          signature(x = 'rnetBasic'),
          function(x) {
            cat(  '\nVertex set:',	paste(x@vertices, collapse = ' '),
                  '\n',
                  '\n  Edge set:',
                  '\n')
            edge.frame <- data.frame(V1 = as_edgelist(x@R)[,1],
                                     V2 = as_edgelist(x@R)[,2],
                                     Omega = round(E(x@R)$omega, 3)	
            )
            print(edge.frame)
          })

#' @rdname print
#' 
setMethod(f = 'print',
          signature(x = 'rnetSubset'),
          function(x) {
            cat(	'\nRnet conditioned on', as.character(x@subset), '\n')
            print(as(x, 'rnetBasic'))
          })

#' @rdname print
#' 
setMethod(f = 'print',
          signature(x = 'rnetStrata'),
          function(x) {
            cat(	"\nRnet stratified by ", x@stratify_by, " (", length(x@R_set), " Strata)\n", sep = '')
            cat(  '\nVertex set:',	paste(unique(unlist(lapply(x@R_set, function(x) x@vertices ))), collapse = ' '), "\n")
            cat(  '\nEdge set:\n')
            print(.Aggregate_Edges(x, 'omega'))
            
          })
#' @rdname print
#' 
setMethod(f = 'print',
          signature(x = 'rnet.L1.set'),
          function(x) {
            cat('\nStability Approach for Regularization Selection (StARS) results\n\nD_b by L1\n')
            output <- round(x@D, 4)
            print(output)
          })
