#'Summary - rnetBasic
#'
#'Gives more information than 'print'.
#' @param object an rnet object of class 'rnetBasic'
#' @rdname summary-rnetBasic
#' @importFrom stringr str_pad
#' @aliases summary
#' @export

setMethod(f = 'summary',
          signature(object = 'rnetBasic'),
          function(object) {
            cat(	'\nBasic R-net',
                 '\n',
                 '\n Sample:', dim(object@x)[1], 'isolates,', length(object@vertices), 'vertices ')
            if(length(object@V_omitted)==1) cat('(', length(object@V_omitted), ' vertex omitted)', sep = '') 		
            if(length(object@V_omitted) > 1) cat('(', length(object@V_omitted), ' vertices omitted)', sep = '') 		
            cat(	'\n',
                 '\n     L1:', object@L1,
                 '\n  Edges:', ecount(object@R),
                 '\nDensity:', round(100*edge_density(object@R), 1),'%',
                 '\n')
            print(object)
            if(length(object@V_omitted)>0) cat('\nOmitted Vertices:', paste(object@V_omitted, collapse = ', '), '\n')
            cat('\n')
          })	



#'Summary - rnetStrata
#'
#'Gives more information than 'print'.
#' @param object an rnet object of class 'rnetStrata'
#' @rdname summary-rnetStrata

setMethod(f = 'summary',
          signature(object = 'rnetStrata'),
          function(object) {
           Edges <- ifelse(object@E_aggr == 0, '', stringr::str_pad(abs(object@E_aggr), width = 5, side = 'right', pad = '0'))
           Edges <- ifelse(object@E_aggr < 0, 
                           stringr::str_pad(Edges, width = 6, side = 'left', pad = '-'),
                           stringr::str_pad(Edges, width = 6, side = 'left', pad = ' ')
                            )
            colnames(Edges) <- paste(' ', gsub(paste(object@stratify_by, '.', sep = ''), '', colnames(Edges)), sep = '')
            summary_table <- rbind(
                  sapply(object@R_set, function(x) dim(x@x)[1]),
                  sapply(object@R_set, function(x) vcount(x@R)),
                  sapply(object@R_set, function(x) ecount(x@R))
                  )
            dimnames(summary_table) <- list(c('Total n', 'Vertices', 'Edges'), colnames(Edges))

            cat( '\n   Stratfied R-net',
                 '\n',
                 '\n Stratified by:', object@stratify_by,
                 '\n    L1 Penalty:', object@R_set[[1]]@L1,
                 '\n',
                 '\nStrata Summary:\n'
                 )
            print(summary_table)
            cat('\n\nEdges:\n')
            print(Edges, quote = FALSE)
            cat('\n\nNote: The "Total n" row in the summary refers to the size of dataset.',
              '\n  The number observations used to estimate partial correlations may vary by edge within each stratum.',
              '\n  Stratum-sepcific details can be displayed by calling: summary(rnet.obj@R_set[[1]])\n')
          })



#'Summary - rnetSubset
#'
#'Gives more information than 'print'.
#' @param object an rnet object of class 'rnetSubset'

#' @rdname summary-rnetSubset

setMethod(f = 'summary',
          signature(object = 'rnetSubset'),
          function(object) {
            cat(  '\nR-net (subset data)',
                  '\n',
                  '\n Sample:', dim(object@x)[1], 'isolates,', length(object@vertices), 'vertices ')
            if(length(object@V_omitted)==1) cat('(', length(object@V_omitted), ' vertex omitted)', sep = '') 		
            if(length(object@V_omitted) > 1) cat('(', length(object@V_omitted), ' vertices omitted)', sep = '') 		
            cat(	'\n',
                 '\n     L1:', object@L1,
                 '\n  Edges:', ecount(object@R),
                 '\nDensity:', round(100*edge_density(object@R), 1),'%',
                 '\n')
            print(object)
            if(length(object@V_omitted)>0) cat('\nOmitted Vertices:', paste(object@V_omitted, collapse = ', '), '\n')
            cat('\n')
          })

#'Summary - L1 selection object
#'
#'In addition to 'print', this method also shows a table of component D_b values for each edge.
#' @param object an object of class 'rnet.L1.set'

#' @rdname summary-L1selection

setMethod(f = 'summary',
          signature(object = 'rnet.L1.set'),
          function(object) {
            E.long <- object@stability
            E.long$Percent <- paste('  ',as.character(E.long$Pr * 100), "%", sep = '')
            E.table <- reshape(
              E.long,
              direction = 'w',
              idvar = 'E', 
              timevar = 'L1',
              v.names = 'Percent',
              drop = c('Eta', 'Count', 'Pr'),
              new.row.names = 1:length(unique(E.long$E))
            )
          
            names(E.table)<- gsub('Percent.', '', names(E.table))
            for(i in 2:dim(E.table)[2])  E.table[[i]][is.na(E.table[[i]])] <- ''

            table.col.width <- max(max(nchar(names(E.table))[-1]), 6)
            E.max.str <- paste('\n  Max(|E|)\n  ', strrep(' ', max(nchar(as.character(E.table$E)))))
            for(i in 2:dim(E.table)[2]) {
              E.max.i <- sum(E.table[[i]]!= '')
              val <- switch(trunc(log10(E.max.i)) + 1,
                          paste('     ', as.character(E.max.i), sep = ''),
                          paste('    ', as.character(E.max.i), sep = ''),
                          paste('   ', as.character(E.max.i), sep = ''),
                          paste(' ',sprintf("%.1f", round(E.max.i/1000, 1)), ' k', sep = ''),
                          paste(as.character(round(E.max.i/1000, 1)), ' k', sep = ''),
                          paste(' ',as.character(round(E.max.i/1000, 0)), ' k', sep = ''),
                          ' > 1 M'
              )
              if(is.null(val)) val <- ' > 1 M'
              E.max.str <- paste(
                E.max.str, 
                strrep(' ', table.col.width - 5),
                val,
                sep = ''
              )
            }
          
          max.loc <- which(object@D == max(object@D))
          under.crit.loc <- which(object@D < object@D_thresh)
          suggest.L1 <- names(under.crit.loc[min(which(under.crit.loc >= max.loc))] )  
          
          print(object)
          cat('\nSuggested Penalty:', suggest.L1, '\n(Minimum penalty that produced D < 0.05 for penalties > L1_max(D) )\n\n\nPresences of Edges\n')
          print(E.table)
          cat(E.max.str, '\n')
          })



#'Summary - MRF comparison with Hotellings T2
#'
#'Displays crude and adjusted T2 statistics in addition to comparison of the edges in two MRFs
#' @param object an object of class 'mrf_t2'
#' 
#' @rdname summary-mrf_t2
#' 
#setMethod(
#  f = 'summary',
#  signature(object = 'mrf_t2'),
#  function(object) {
summary.mrf_t2 <- function(object) {
    cat(
      "\nMRF Hotellings's T2 Comparison\n\nCrude\nT2 = ", object$crude$statistic, "; p-val = ", object$crude$p.val,
      " (df1 = ", object$crude$parameter[1], ", df2 = ", object$crude$parameter[2],
      ")\n\n Adjusted\nT2 = ", object$adj$T2, "; p-val = ", object$adj$pval,
      " (df1 = ", object$adj$df1, ", df2 = ", object$adj$df2, ")\n\n Pair-wise Edge Tests\n",
      sep = ''
      )
  
    p.vec <- paste(
      round(object$t_table$p, 2), 
      cut(object$t_table$p, breaks = c(0, 0.01, 0.05, 0.1, 1.01), labels = c('**', '* ', ". ", '  '), include.lowest = TRUE),
      sep = ''
      )
  
    output.frame <- data.frame(
      X_bar = round(object$t_table$X_bar,3),
      Y_bar = round(object$t_table$Y_bar,3),
      d_bar = round(object$t_table$d_bar,3),
      var_X = round(object$t_table$var_X,4),
      var_Y = round(object$t_table$var_Y,4),
      t_c = round(object$t_table$t_c,2),
      p = p.vec
      )
    
    print(x = output.frame)
    return(output.frame)
    #cat("Signif. codes:  0 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    
  }


