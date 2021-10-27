
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrdrivers

<!-- badges: start -->

[![lucode2-check](https://github.com/pik-piam/mrdrivers/actions/workflows/lucode2-check.yaml/badge.svg)](https://github.com/pik-piam/mrdrivers/actions/workflows/lucode2-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-bright_green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://codecov.io/gh/pik-piam/mrdrivers/branch/main/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrdrivers?branch=main)
<!-- badges: end -->

The goal of **mrdrivers** is to handle the construction of GDP, GDP per
capita, Population and Urbanization scenarios: all of which are
important drivers to the REMIND and MAgPIE models.

## Installation

``` r
# From the PIK rse-server
install.packages("mrdrivers", repos = "https://rse.pik-potsdam.de/r/packages")

# or from Github
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

The readSource and calcOutput functions provided by this package are
listed below.

readSource:

    #>  [1] "ARIADNE"                   "ARIADNE_ReferenceScenario"
    #>  [3] "EurostatPopGDP"            "HYDE"                     
    #>  [5] "IMF"                       "James"                    
    #>  [7] "James2019"                 "MissingIslands"           
    #>  [9] "OECD"                      "PEAP"                     
    #> [11] "PopulationTWN"             "PWT"                      
    #> [13] "SSP"                       "UN_PopDiv"                
    #> [15] "WDI"

readSource (deprecated?!):

    #> [1] "IIASApop" "SRES"

calcOutput:

    #>  [1] "DefaultDrivers"   "GDP"              "GDPFuture"        "GDPPast"         
    #>  [5] "GDPpc"            "GDPpcFuture"      "GDPpcPast"        "Labour"          
    #>  [9] "Population"       "PopulationFuture" "PopulationPast"   "RatioPPP2MER"    
    #> [13] "Urban"            "UrbanFuture"      "UrbanPast"        "UrbanPop"

## Citation

To cite package **mrdrivers** in publications use:

Koch J, Bodirsky B, Baumstark L, Wang X, Chen D, Leip D, Benke F, Eweron
A, Rodrigues R, Giannousakis A, Levesque A, Pehl M, Soergel B, Dietrich
J (2021). *mrdrivers: Create GDP and population scenarios*.
<https://pik-piam.github.io/mrdrivers>,
<https://github.com/pik-piam/mrdrivers>.

A BibTeX entry for LaTeX users is

``` latex
@Manual{,
 title = {mrdrivers: Create GDP and population scenarios},
 author = {Johannes Koch and Benjamin Leon Bodirsky and Lavinia Baumstark and Xiaoxi Wang and David Chen and Deborra Leip and Falk Benke and Araujo Eweron and Renato Rodrigues and Anastasis Giannousakis and Antoine Levesque and Michaja Pehl and Bjoern Soergel and Jan Philipp Dietrich},
 year = {2021},
 note = {https://pik-piam.github.io/mrdrivers, https://github.com/pik-piam/mrdrivers},
}
```
