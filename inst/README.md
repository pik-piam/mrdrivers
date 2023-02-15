
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- # mrdrivers -->
<!-- badges: start -->
<!-- [![lucode2-check](https://github.com/pik-piam/mrdrivers/actions/workflows/lucode2-check.yaml/badge.svg)](https://github.com/pik-piam/mrdrivers/actions/workflows/lucode2-check.yaml) -->
<!-- [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-bright_green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/pik-piam/mrdrivers/branch/main/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrdrivers?branch=main) -->
<!-- badges: end -->
<!-- The goal of **mrdrivers** is to handle the construction of GDP, GDP per capita, Population and Urban Population share scenarios: all of which are important drivers to the REMIND and MAgPIE models. -->
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

## ‘calcOutput’ and ‘readSource’ functions

The key readSource and calcOutput functions provided by this package are
listed below.

### readSource

    #> [1] "readWDI"            "readIMF"            "readEurostatPopGDP"
    #> [4] "readUN_PopDiv"      "readPEAP"           "readSSP"           
    #> [7] "readMissingIslands" "readJames2019"

### calcOutput

    #> [1] "calcGDP"        "calcGDPpc"      "calcPopulation" "calcLabour"    
    #> [5] "calcUrban"

## Default scenarios

The current default scenarios returned for all drivers are currently:

- the SSPs, i.e. SSP1-5

- the SDPs, i.e. SDP, SDP_EI, SDP_RC, and SDP_MC

- SSP2EU
