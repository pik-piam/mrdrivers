# Create GDP and population scenarios

R package **mrdrivers**, version **0.2.4**

[![CRAN status](https://www.r-pkg.org/badges/version/mrdrivers)](https://cran.r-project.org/package=mrdrivers)  [![R build status](https://pik-piam.github.io/mrdrivers/workflows/check/badge.svg)](https://pik-piam.github.io/mrdrivers/actions) [![codecov](https://codecov.io/gh/mrdrivers/branch/master/graph/badge.svg)](https://codecov.io/gh/mrdrivers) [![r-universe](https://pik-piam.r-universe.dev/badges/mrdrivers)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

Create GDP and population scenarios
    This package takes care of GDP and population scenario construction, which are important drivers
    to both the REMIND and MAgPIE models.

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

## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrdrivers")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Johannes Koch <jokoch@pik-potsdam.de>.

## Citation

To cite package **mrdrivers** in publications use:

Koch J, Bodirsky B, Baumstark L, Wang X, Chen D, Leip D, Benke F, Eweron A, Rodrigues R, Giannousakis A, Levesque A, Pehl M, Soergel B, Dietrich J (2021). _mrdrivers: Create GDP and population scenarios_. https://pik-piam.github.io/mrdrivers, https://github.com/pik-piam/mrdrivers.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrdrivers: Create GDP and population scenarios},
  author = {Johannes Koch and Benjamin Leon Bodirsky and Lavinia Baumstark and Xiaoxi Wang and David Chen and Deborra Leip and Falk Benke and Araujo Eweron and Renato Rodrigues and Anastasis Giannousakis and Antoine Levesque and Michaja Pehl and Bjoern Soergel and Jan Philipp Dietrich},
  year = {2021},
  note = {https://pik-piam.github.io/mrdrivers, https://github.com/pik-piam/mrdrivers},
}
```
