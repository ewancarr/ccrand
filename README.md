# R implementation of covariate-constrained randomisation 

*Ewan Carr*  
*Department of Biostatistics and Health Informatics, King's College London*  
*September 5, 2019*

This script (`randomisation.R`) implements covariate-constrained randomisation,
as described in Carter and Hood (2008). It is intended for use in
cluster-randomised trials where blocks of clusters (of varying size; 4, 6, 8)
are allocated sequentially. Please see `example.R` for details.

This work will be presented at the [Brighton Conference]

> Abstract


![](Poster)

# Don't use this software

This software was developed for a single trial and was not intended for wider
use. You are welcome to adapt/reuse this code. However, if you are thinking of
using covariate-constrained randomisation we recommend you refer to one of the
following existing packages, which are easier to use and offer many more
features:

* *R*
    * [cvcrand: Efficient Design and Analysis of Cluster Randomized Trials](https://cran.r-project.org/package=cvcrand)
    * [The Shiny Balancer - software and imbalance criteria for optimally balanced treatment allocation in small RCTs and cRCTs](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-018-0551-5)
* *Stata*
    * [CVCRAND: Stata module for efficient design and analysis of Cluster Randomized Trials](https://ideas.repec.org/c/boc/bocode/s458377.html)

### References 

Carter BR, Hood K. Balance algorithm for cluster randomized trials. *BMC Medical Research Methodology* 2008;8. [doi:10.1186/1471-2288-8-65](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-8-65)
