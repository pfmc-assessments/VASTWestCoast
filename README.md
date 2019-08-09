# VASTWestCoast

The Northwest Fisheries Science Center (NWFSC) uses 
[VAST](https://github.com/James-Thorson/VAST) to generate 
indicies of abundance from survey data. This package, VASTWestCoast,
acts as a wrapper for VAST, where the input argument values are specific to those
needed for the NWFSC. The package can also perform simulations based
on empirical data. 

## Getting Started
The following instructions will get you a copy of the project up and 
running on your local machine.

### Prerequisites 
1. Install [Rtools](https://cran.r-project.org/bin/).
2. Install devtools from CRAN `install.packages("devtools")`

### Installing
`devtools::install_github("nwfsc-assess/VASTWestCoast")`

## Documentation
Configuration of the VAST package is complex, and the intent of VASTWestCoast was to simplify and standardize the code needed to generate indices of abundance for species managed by the Pacific Fisheries Management Council (PFMC). Settings that should be specified the same regardless of species and of survey are set behind the scenes to ensure their consistency. For example, all results are corrected for retransformation bias to account for nonlinear transformation of random effects (Thorson and Kristensen, 2016). Survey-specific example scripts are available in [/inst/examples](https://github.com/nwfsc-assess/VASTWestCoast/tree/master/inst/examples). 

