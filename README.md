# R implementation of covariate-constrained randomisation 

*Ewan Carr*  
*Department of Biostatistics and Health Informatics, King's College London*  
*September 5, 2019*

This script (`randomisation.R`) implements covariate-constrained randomisation,
as described in Carter and Hood (2008). It is intended for use in
cluster-randomised trials where blocks of clusters (of varying size; 4, 6, 8)
are allocated sequentially. Please see `example.R` for details.

***
This work will be presented at the [5th International Clinical Trials
Methodology Conference](https://ictmc2019.com/) in Brighton, UK (6-9 October, 2019):

>**Covariate constrained block randomisation for a cluster randomised trial**  
>*Kirsty James, Sabine Landau, Ewan Carr, Ben Carter (2019)*
>
>**Introduction**  Cluster randomised controlled trials require randomisation at the level of the cluster as opposed to the level of the participant. As there are fewer units being randomised than in an individually randomised trial the risk of baseline covariate imbalance is high. Standard methods of stratified randomisation can be employed but are limited to categorical covariates. In an ongoing trial we used stratified covariate constrained randomisation in order to accommodate continuous covariates.  
>
>**Methods**  Clusters were identified within catchment areas, 4-6 within each. We required balance in the trial arms for characteristics of the area's service user populations hence the randomisation was stratified by catchment area. In addition, we balanced trial arms for two continuous cluster level covariates; surgery quality and deprivation. The randomisation algorithm, adapted from the work of Carter and Hood, balanced trial arms within and across catchment areas for these two covariates.   
>
>**Results**  We randomised 28 clusters from 7 catchment areas (strata). All clusters within a stratum were supplied as a set over the course of the randomisation period. Every time the covariate information on clusters of a stratum became available the algorithm worked out all possible cluster assignments within the stratum and constructed a balancing index based on the clusters that have been randomised so far. An assignment is then chosen at random from the best performing allocations in terms of the balancing index to avoid the algorithm becoming deterministic.  
>
>**Discussion**  There were several added complexities in using this randomisation technique in terms of performing the allocations as it was a bespoke algorithm executed by the statisticians. This method does require all cluster information within a stratum to be provided at once which could be a limitation. Outside of this the algorithm allowed the flexibility that was required to balance on continuous covariates in a reliable way.

***

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
