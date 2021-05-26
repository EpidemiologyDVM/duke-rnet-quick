# Rnets
The `Rnets` package for mapping relationships in antimicrobial resistances (AMR) in bacterial populations. The sets of estimated relationships are treated as networks; The name of the package, core function, and analysis result, 'Rnet', is derived from the phrase "Resistance relationship network." AMR surveillance programs have produced huge amounts of data, but new methods are needed to interpret and study this volume of data. The `Rnets` package applies the graphical least absolute selection and shrinkage operator, also referred to as the 'graphical LASSO', to determine which resistances are correlated, and which are conditionally independent. Our goal in developing this package was to make the Rnets method easily accessible to all research. Therefore the core function in the package, `Rnet` accepts input in a commonly used format, with isolates data stored in rows and respective minimum inhibitory concentration (MIC) data stored in columns, and directly and quickly produces useful analyses with the mudane data handling taken care of behind the scenes. A suite of additional functions is included to interact with and visualize the analysis results.

A more in-depth description of the methods employed by the package are available in the included vignette.

# Use
The process of estimating a network from raw MIC data can be broadly divided into 3 primary phases:

1. Selection of $\lambda$
2. Induction of sparsity using the graphical LASSO
3. Visualization and interpretation

Briefly, $\lambda$ is a penalty applied to the correlation matrix; Larger $\lambda$ values tend to reduce more parrtial correlations to 0, resulting in sparser networks with fewer edges. To produce informative networks, $\lambda$ should be between smallest and largest absolute values of the correlation matrix's elements. Several methods have been described to select L~1~. The `L1_selection` function employs the Stability Approach to Regularization Selection (StARS) proposed by [CITATION NEEDED]. 

The following code will use the _E. coli_ isolates' from 2008 in `NARMS_EC_DATA` MIC results for 15 antimicrobials. This function evaluates 100 subsets of size 1200 with $\lambda$ equal to 0.05, 0.10,  ..., 0.45, 0.50 for all E. coli s (takes ~ 5 minutes on an i7-6700 4.0 GHz with 16 Gb RAM).

```
EC.L1.results <- L1Selection(
                    Data = NARMS_EC_DATA, 
                    L1_set = seq(0.05, 0.50, 0.05), 
                    n_b = 1200, 
                    V_set = ABX_LIST, 
                    Stratify = NARMS_EC_DATA$Year == 2008
                    )
print(EC.L1.results@StARS_D)
```
Since the StARS method is based on random subsamples without replacement, running this code will produce slightly different results. You can get consistent results between runs by setting your random.seed before running this code. Our results were as follows:
```
> round(EC.L1.results@StARS_D, 4)
  0.05    0.1   0.15    0.2   0.25    0.3   0.35    0.4   0.45    0.5 
0.1812 0.0736 0.0448 0.0508 0.0373 0.0172 0.0123 0.0031 0.0136 0.0098 
```
The suggested penalty will be the smallest $\lambda$ for which StARS_D < 0.05. Here, it is 0.25, which We have typically found to be reasonable penalty for estimating R-nets. With the $\lambda$ value selected, we can estimate the network for the 15 MICs from the full set of _E. coli_ from 2008:
```
EC08_Rnet <- Rnet(
                Data = NARMS_EC_DATA, 
                L1 = 0.25, 
                V_set = ABX_LIST,
                Stratify = NARMS_EC_DATA$Year == 2008
                )

summary(EC08_Rnet)
```

The estimated network can be plotted with the plot method.
```
plot(EC08_Rnet)
```

# Installation
The latest stable version of ```Rnets``` is available on the author's GitHub and can be installed using the following code:
```
library(devtools)
install-github('EpidemiologyDVM/Rnets')
```

The latest development branch of the project, which is not guarunteed to be stable, can also be accessed from the author's GitHub using:
```
library(devtools)
install-github('EpidemiologyDVM/Rnets', branch = 'dev')
```

The `Rnets` package has the following dependancies:

* The graphical LASSO method is performed using the `glasso` function in the eponymous package maintained by Rob Tibshiriani
* Networks are handled and plotted using a variety of functions in the `igraph` package maintained by Gábor Csárdi.
* Data aggregation over multiple strata is handled using the efficient `rbindlist` function in the `data.table` package maintained by Matt Dowle. 

All three dependencies are available on CRAN as of 1.Dec.2017. 

# Bug Reporting
If you find parts of the package are not working as intended, please submit the issue on our project's GitHub site at: https://GitHub.com/EpidemiologyDVM/Rnets/issues or contact the author at wjlove@ncsu.edu