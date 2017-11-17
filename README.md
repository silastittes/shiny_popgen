Shiny apps for pop gen courses. 
======================================

Created by Silas Tittes and Scott Taylor, for the undergraduate Population Genetics course taught at University of Colorado at Boulder. 
 
This repository consists of multiple sub-directories organized by topic. Each directory has at least one shiny app. Some have multiple. 

Dependencies
-----------

R and RStudio

Additionally, make sure `tidyverse`, `shiny`, and `wesanderson` packages are installed.

```
#get package names
pckgs <- c("tidyverse", "shiny", "wesanderson")

#determine if packages are installed already
miss <- pckgs[!pckgs %in% installed.packages()]

#install missing packages
if(length(miss)) install.packages(miss, dependencies = TRUE)
```

How to run apps
---------------

To run an app, simply open in RStudio and click the "Run App" button. We prefer to use the "Run External" option (see image), which will open the app in your system's default web browser.

![shiny_pic](shiny_run_pic.png)


Details
-------

1. Drift
..* drift_app.R


...Two-allele drift only-model. Shows flucuations in allele frequency over discrete generations. Includes option of imposing a bottleneck (without population size recovery).


2. Coalescence
..* discrete_time_app.R


...Discrete-time forward simulation of sampling alleles. Provides option to show trace of ancestry (useful to see if alleles coalesce in time of simulation).


..* continuous_coalescence_app.R

...Create random geneologies and drop mutations along branches (number of mutations recorded in the small box along branches). Option to hide or show the sequence alignment and geneology (note columns are individuals). 


* FST
  * fst_app.R
    * Two deme visualization. Randomly samples alleles according to frequencies specified by user. Prints HS, HT, and FST. 


* Selection
  * haploid_selection_app.R
  * Shows change in allele frequency over time due to relative fitess of alleles.

..* diploid_selection_app.R

...Produces three plot types, Average fitness versus allele frequencey, change in allele frequency versus allele frequency, and change in allele frequeny over discrete generations.


5. STRUCTURE
..* structure.R


...Not a shiny app, Make plots from STUCTURE output.


6. Quant-Gen
..* additive_alleles_app.R


...A really simple demonstration on how additive alleles contribute to quantitative traits, and rapidly approximate a normal distirbution.



Contribute
---------

I would love to expand and improve these. Please submit pull requests and issues, or email me with suggestions. 
silas(dot)tittes(at)gmail(dot)com

