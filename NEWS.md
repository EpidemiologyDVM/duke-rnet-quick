#Rnets v 0.9.9

## Major Changes

* 0.9.5 
    + Added news.md
    + Added README.md
    + check() produces 2 'notes'
  
* 0.9.6
    + Changed 'Modularity_Signed' to 'signedModularity'
    + Revised signedModularity
    + Changed S4 naming convention from 'rnet.basic' to 'rnetBasic'
    + `check()` produces 0 errors/warnings/notes (other than UTF-8 warning)

* 0.9.7
    + Removed signedModularity.data.frame 
    + Revised signedModularity to matrix-based algorithm instead of list-based
    
* 0.9.8
    + Initial 'open beta' relase
    + 'L1Selection' no longer has .random.seed option
    
* 0.9.9
    + Resolved bug with 'plot' method. Was calling hidden function as external, now called as internal.
    + Added graph.layout argument to plot method to allow manual definition of graph layout.
    + Added summary method for Rnet objects with multiple strata
    
* 1.0.3
    + Fixed error with Assign_Vmetadata.RnetStrata and Assign_Emetadata.RnetStrata. Courtesy return when reassign == TRUE was calling slot R which does nto exist in RnetStrata objects.
    