
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- # mrdrivers -->
<!-- badges: start -->
<!-- [![lucode2-check](https://github.com/pik-piam/mrdrivers/actions/workflows/lucode2-check.yaml/badge.svg)](https://github.com/pik-piam/mrdrivers/actions/workflows/lucode2-check.yaml) -->
<!-- [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-bright_green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/pik-piam/mrdrivers/branch/main/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrdrivers?branch=main) -->
<!-- badges: end -->
<!-- The goal of **mrdrivers** is to handle the construction of GDP, GDP per capita, Population and Urbanization scenarios: all of which are important drivers to the REMIND and MAgPIE models. -->
<!-- ## Installation -->
<!-- ```{r, eval=FALSE} -->
<!-- # From the PIK rse-server -->
<!-- install.packages("mrdrivers", repos = "https://rse.pik-potsdam.de/r/packages") -->
<!-- # or from Github -->
<!-- remotes::install_github("pik-piam/mrdrivers") -->
<!-- ``` -->

## Framework: madrat and magclass

This package depends upon the
[madrat](https://github.com/pik-piam/madrat#readme) and
[magclass](https://github.com/pik-piam/magclass#readme) packages. It
integrates into the madrat framework and is structured accordingly. It
mainly uses magpie objects (from the magclass package). For more
information on madrat or magclass, please visit the respective github
repositories ([madrat](https://github.com/pik-piam/madrat#readme),
[magclass](https://github.com/pik-piam/magclass#readme)).

The readSource and calcOutput functions provided by this package are
listed below.

readSource:

    #>  [1] "ARIADNE"                   "ARIADNE_ReferenceScenario"
    #>  [3] "EurostatPopGDP"            "IMF"                      
    #>  [5] "James"                     "James2019"                
    #>  [7] "MissingIslands"            "OECD"                     
    #>  [9] "PEAP"                      "PopulationTWN"            
    #> [11] "PWT"                       "SSP"                      
    #> [13] "UN_PopDiv"                 "WDI"

readSource (deprecated?!):

    #> [1] "IIASApop" "SRES"

calcOutput:

    #>  [1] "DefaultDrivers"   "GDP"              "GDPFuture"        "GDPPast"         
    #>  [5] "GDPpc"            "GDPpcFuture"      "GDPpcPast"        "Labour"          
    #>  [9] "Population"       "PopulationFuture" "PopulationPast"   "RatioPPP2MER"    
    #> [13] "Urban"            "UrbanFuture"      "UrbanPast"        "UrbanPop"
