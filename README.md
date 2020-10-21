# VASTWestCoast

The Northwest Fisheries Science Center (NWFSC) uses
[VAST](https://github.com/James-Thorson-NOAA/VAST)
to generate indicies of abundance from survey data.
This package, VASTWestCoast, acts as a wrapper for VAST,
where the input arguments are specific to those needed for the NWFSC.

## Getting Started

### Prerequisites
1. Install [Rtools40](https://cran.r-project.org/bin/).
2. Have IT add c:\rtools40\usr\bin and c:\rtools40\mingw64\bin to your PATH
3. Install devtools from CRAN `install.packages("devtools")`

### Installing
Install issues are always troublesome, for which I am sorry.
To minimize the risk of github issuing you a time out, please run the following
code if you do not already have a token.
```
usethis::browse_github_pat()
usethis::edit_r_environ()
```
A file will open and you should place
```
`GITHUB_PAT='placeyournewtokenhere'`.
```
in it and then save it.

Next, the best guidance I can give you is to close all of your current R sessions and
open a new R session before trying to install any of the following packages.
Lastly, use the internal check function to see if TMB is properly installed
prior to running other VASTWestCoast functions.
```
remotes::install_github("James-Thorson-NOAA/FishStatsUtils")
remotes::install_github("James-Thorson-NOAA/VAST")
remotes::install_github("nwfsc-assess/VASTWestCoast")
library(VASTWestCoast)
check_rtools()
check_TMB()
```

## Using VASTWestCoast
Configuration of the VAST package is complex,
and the intent of VASTWestCoast was to simplify and standardize
the code needed to generate indices of abundance for species managed by the
Pacific Fisheries Management Council (PFMC).
Settings that should be specified the same regardless of species and of survey
are set behind the scenes to ensure their consistency.
For example, all results are corrected for retransformation bias to account for
nonlinear transformation of random effects (Thorson and Kristensen, 2016).
Example scripts are available in
[/inst/examples](https://github.com/nwfsc-assess/VASTWestCoast/tree/master/inst/examples).

The simplest way to run VAST is to use
```
?VASTWestCoast::VAST_spp
VAST_spp(dir = getwd(), species = "sablefish")
```
where the species argument is the common name for the species of interest.

## Issues
Please post all issues to the github issues board
such that other users can benefit from the conversation generated.
The use of appropriately labelled issues is extremely beneficial to developers
in that current needs can be both tracked and prioritized for everyone's benefit.
