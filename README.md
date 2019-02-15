# VASTWestCoast

The Northwest Fisheries Science Center (NWFSC) uses the 
[VAST](https://github.com/James-Thorson/VAST) package to generate 
indicies of abundance from survey data. This package, VASTWestCoast,
acts as a wrapper for VAST, where the arguments are specific to those
needed for the NWFSC. The package can also perform simulations based
on empirical data. 

## Getting Started

The following instructions will get you a copy of the project up and 
running on your local machine.

### Prerequisites 

Please install [Rtools](https://cran.r-project.org/bin/).

You will need to install devtools prior to installing `VASTWestCoast`:

```
install.packages("devtools")
```

### Installing

```
devtools::install_github("nwfsc-assess/VASTWestCoast")
```

## Documentation

Documentation for the functions within the package are available via standard
roxygen comments.

Examples are available for each survey in [/inst/examples](https://github.com/nwfsc-assess/VASTWestCoast/tree/master/inst/examples). 
Some of these examples are stil in development.
