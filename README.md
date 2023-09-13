# Lipid Mini-On

The Lipid Mining and Ontology (Lipid Mini-On) tool is a Shiny app that uses the Rodin R package.
to create ontology bins and perform lipid enrichment analysis.

## Running Lipid Mini-On

To use Lipid Mini-On, you need to run it as a local Shiny app; required steps:

* Install R 4.x
  * Windows: https://cran.r-project.org/bin/windows/base/
  * Mac: https://cran.r-project.org/bin/macosx/
  * Linux: https://cran.r-project.org/bin/linux/
* Install RStudio Desktop
  * https://posit.co/download/rstudio-desktop/
* Download the LipidMiniOn repository from https://github.com/PNNL-Comp-Mass-Spec/LipidMiniOn
  * Option 1: clone the repository using a Git program
    * Clone into a local directory, for example `C:\R\LipidMiniOn`
  * Option 2: at https://github.com/PNNL-Comp-Mass-Spec/LipidMiniOn click the "Code" button, then choose "Download Zip"
    * Direct link: https://github.com/PNNL-Comp-Mass-Spec/LipidMiniOn/archive/refs/heads/master.zip
    * Extract the files to a local directory, for example `C:\R\LipidMiniOn`
* Start R Studio
* Under the File menu, choose "New Project"
  * Choose "Use existing directory"
  * Select the LipidMiniOn directory
* Load the required packages

```
install.packages(c("shiny", "plotly", "visNetwork", "shinycssloaders", "markdown"))
install.packages(c("ggplot2", "grid", "gridExtra", "gtable", "MASS", "plyr", "reshape2", "scales", "stats", "tibble", "lazyeval", "DT"))

install.packages("devtools")
devtools::install_github('https://github.com/PNNL-Comp-Mass-Spec/Rodin')

# Either load all of the libraries at once (using "invisible()" since the output from lapply() is not useful)
invisible(lapply(c("ggplot2", "grid", "gridExtra", "gtable", "MASS", "plyr", "reshape2", "scales", "stats", "tibble", "lazyeval", "DT"), library, character.only = TRUE))

# Or load each library individually
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(MASS)
library(plyr)
library(reshape2)
library(scales)
library(stats)
library(tibble)
library(lazyeval)
library(DT)

# Load Shiny and required libraries
invisible(lapply(c("shiny", "plotly", "visNetwork", "shinycssloaders", "markdown"), library, character.only = TRUE))

# Start the Shiny app
runApp('C:/R/LipidMiniOn')
```

Lipid Mini-On will appear in a new window
* You can also access it using your browser
  * http://127.0.0.1:5451/


### Testing Lipid Mini-On

Directory `C:\R\LipidMiniOn` has several .csv files that can be used for testing
* The files are also downloadable from https://github.com/PNNL-Comp-Mass-Spec/LipidMiniOn

Steps to load process the data
* Click Browse under "Upload 'Query' Lipid Names (.csv)"
  * Select file `Soil_surface_signif.csv`
* Click Browse under "Upload 'Universe' Lipid Names (.csv)"
  * Select file `Soil_universe.csv`
* Click "Check Data"
* Click "Visualize" to see two stacked bar charts


## Contacts

Written by Sarah Reehl and Kelly Stratton for the Department of Energy (PNNL, Richland, WA) \
E-mail: geremy.clair@pnnl.gov or proteomics@pnnl.gov \
Website: https://www.pnnl.gov/integrative-omics or https://panomics.pnnl.gov/

See the [About.md](About.md) file for more information about the developers

## License

Lipid Mini-On is licensed under the 2-Clause BSD License; 
you may not use this file except in compliance with the License.  You may obtain 
a copy of the License at https://opensource.org/licenses/BSD-2-Clause

Copyright 2018 Battelle Memorial Institute
