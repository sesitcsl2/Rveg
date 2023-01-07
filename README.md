
# Rveg

<!-- badges: start -->
<!-- badges: end -->

## Rveg - R package

Rveg is a database package for recording phytosociological relevés and
exporting csv database format, usable for data analyses in R
environment. There are a few software solutions for relevé digitization,
but we mostly find them more outdated, more complicated or more
unaffordable then Rveg package. Also R package solution can save users
effort, as significant part of the world science community nowadays use
R for the further analyses.

Rveg creates

## Installation

You can install the development version of Rveg from
[GitHub](https://github.com/) with `devtools` package:

``` r
install.packages("devtools")
devtools::install_github("sesitcsl2/Rveg")
require(Rveg)
```

or with simpler `remotes` package:

``` r
install.packages("remotes")
remotes::install_github("sesitcsl2/Rveg")
require(Rveg)
```

## Usage

Soon you might watch commented videomanual on youtube.

Rveg brings a few functions for working with vegetation occurrence and
abundance data, I.e phytosociological relevés.

``` r
addReleve() # Records your relevés
RvegCheck() # Check your database for duplicities
RvegMerge() # Merge two databases
RvegCombine() # Merge two species in database or merge Layers
RvegToJuice() # Convert Rveg database to JUICE compatible format 
tvToRveg() # Convert Turboveg csv export to Rveg database format
```

For more detailed informations about their usage use `help(fun)`

## starting example

``` r
require(Rveg)
addReleve(SAVE = "My_first_database")
```

That will start dialoque communication in console for writing the
relevés. Rveg database consist of Header csv file and Relevé csv file.
First you have to fill header DATA for your first relevé.

``` console
DATE?(Y/M/D): 2023/1/5
SPRINGDATE?(Y/M/D):
LOCALITY?: Yellowstone
.
.
.
```

After you fill all header columns, loop dialoque will allow you to start
writing species.

``` console
AddNewLayer?(Y/N): Y
Select layer (3,2,1,J,0): 1
P - percentage, BB - Braun B. scale: P
AddSpecies?(GenuSpe/N): ...
```

This way we started writing species in the first (herb) layer with
covers noted in percentage. Now its important to start filling species
using 7 letter code, consisting of 4 letters of Genus names and 3
letters of species name.

``` console
AddSpecies?(GenuSpe/N): TrifPra
Abundance?(%): 25
                FullName
32391 Trifolium pratense
CorrectName?Y/F(search for name) Y
      ShortName  0
32391 TRIFPRA_1 25
AddSpecies?(GenuSpe/N): ...
```

We have added our first species *Trifolium pratense* with cover 25%.
This process should be repeated for every species in first layer, and
then for all other layers (if you sampled any species in them). For now
we can keep only *Trifolium pratense*.

``` console
AddSpecies?(GenuSpe/N): N
AddNewLayer?(Y/N): N
[1] "Species_richness"
[1] 1
AddReleve?(Y/N/RREL/RHEAD/REMOVEREL/PRINTREL/PRINTHEAD): ...
```

Congratulations! You have completed your first relevé. Now you enter in
the dialoque menu where you can edit and view your database. Also you
can now check your database structure in any application that can read
CSV files separeted by comma. Database is primarly saved in your working
directory

If you want to write more relevés in your database, you can just prompt
“Y” and repeat the process. Other options will be described later. More
important is how you proceed with writing after closing the current
database dialoque.

``` console
AddReleve?(Y/N/RREL/RHEAD/REMOVEREL/PRINTREL/PRINTHEAD): N
>
>
```

Now imagine you come back anytime later to continue with the existing
database. You will simply rerun addReleve function with one more
argument, bringing you back to the dialoque menu.

``` r
addReleve(DATABASE= "My_first_database", SAVE = "My_first_database")
```
