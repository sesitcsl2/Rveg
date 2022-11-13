
# Rveg

<!-- badges: start -->
<!-- badges: end -->

Rveg is database software for creating and managing phytosociological releves.

## Installation

You can install the development version of Rveg like so:

``` r
install.package("devtools")
devtools::install_github("sesitcsl2/Rveg")
```

## Example

This is a basic example which shows you how to work with Rveg:

``` r
library(Rveg)

addRELEVE(SAVE = "Alder_carr_Budweis_2021)
# create your database with dialogue prompts
addRELEVE(SAVE = "Alder_carr_Budweis_2022")
# create your second one database
# end for the day

addRELEVE("Alder_carr_Budweis_2022REL.csv","Alder_carr_Budweis_2022HEAD.csv","Alder_carr_Budweis_2022")
# editing, continuing your last database

RvegMERGE("Alder_carr_Budweis_2021","Alder_carr_Budweis_2021",save = "Alder_carr_Budweis")
# Merging to one database

RvegToJuice("Alder_carr_Budweis")
# exporting REL table to the Juice compatible format

## basic example code
```

