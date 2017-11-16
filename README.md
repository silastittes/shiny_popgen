Shiny apps for pop gen courses. 
======================================

Created by Silas Tittes and Scott Taylor, for undergraduate Population Genetics course taught at University of Colorado at Boulder. 
 
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

Contribute
---------

I would love to expand and improve these. Please submit pull requests and issues, or email me with suggestions. 
silas(dot)tittes(at)gmail(dot)com

