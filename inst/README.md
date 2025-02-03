
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

## Framework

This package depends upon the
[madrat](https://github.com/pik-piam/madrat#readme) and
[magclass](https://github.com/pik-piam/magclass#readme) packages. It
integrates into the madrat framework and is structured accordingly. It
mainly uses magpie objects (from the magclass package). For more
information on madrat or magclass, please visit the respective github
repositories ([madrat](https://github.com/pik-piam/madrat#readme),
[magclass](https://github.com/pik-piam/magclass#readme)).

## Functions

The key `madrat::readSource()` and `madrat::calcOutput()` functions
provided by this package are listed below.

readSource:

    #> [1] "readSSP"       "readWDI"       "readIMF"       "readUN_PopDiv"
    #> [5] "readPEAP"

calcOutput:

    #> [1] "calcGDP"          "calcGDPpc"        "calcPopulation"   "calcLabour"      
    #> [5] "calcUrban"        "calcRatioPPP2MER"

Then there are number of important and/or useful tool functions:

- The function `tooGetUnitDollar()` is where the default unit for all
  monetary units in REMIND and MAgPIE is defined.
- The function `tooGetScenarioDefinition()` is where the scenarios are
  defined and can be helpful when searching for scenario information.
- The functions `toolGeneralConvert()`, `toolHarmonizePast()` and
  `toolHarmonizeFuture()` handle common tasks and are potentially useful
  outside of mrdrivers.

## Scenarios

See `vignette("scenarios")` for more information on - and references
for - available scenarios.
