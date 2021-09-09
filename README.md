
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrdrivers

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of **mrdrivers** is to handle the construction of GDP, GDP per
capita, Population and Urbanization scenarios: all of which are
important drivers to the REMIND and MAgPIE models.

## Installation

Install the package from github:

``` r
remotes::install_github("pik-piam/mrdrivers")
```

## Framework: madrat and magclass

This package depends upon the
[madrat](https://github.com/pik-piam/madrat#readme) and
[magclass](https://github.com/pik-piam/magclass#readme) packages. It
integrates into the madrat framework and is structured accordingly. It
mainly uses magpie objects (from the magclass package). For more
information on madrat or magclass, please visit the respective github
repositories ([madrat](https://github.com/pik-piam/madrat#readme),
[magclass](https://github.com/pik-piam/magclass#readme)).

The read and calc functions provided by this package are listed below.

readSource:

    #>  [1] "ARIADNE_ReferenceScenario" "Eurostat"                 
    #>  [3] "IIASApop"                  "IMF"                      
    #>  [5] "James"                     "James2019"                
    #>  [7] "MissingIslands"            "OECD"                     
    #>  [9] "PEAP"                      "PopulationTWN"            
    #> [11] "PWT"                       "SRES"                     
    #> [13] "SSP"                       "UN_PopDiv"                
    #> [15] "WDI"

calcOutput:

    #>  [1] "DefaultDrivers"   "GDP"              "GDPFuture"        "GDPPast"         
    #>  [5] "GDPpc"            "GDPpcFuture"      "GDPpcPast"        "Population"      
    #>  [9] "PopulationFuture" "PopulationPast"   "Urban"            "UrbanFuture"     
    #> [13] "UrbanPast"
