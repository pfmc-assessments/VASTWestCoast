% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VAST_spp.R
\name{VAST_spp}
\alias{VAST_spp}
\title{Run \pkg{VAST} models for a species sampled by west coast surveys}
\usage{
VAST_spp(dir, species, dist = c("lognormal", "gamma"))
}
\arguments{
\item{dir}{An existing directory where you want the new folder that is created
by \code{VAST_spp} to be saved. Here, \code{VAST_spp} will create a folder with
its name based on \code{species} input argument,
then this folder will be populated with a folder called data.
Inside data, there will be individual folders that contain VAST fits.}

\item{species}{A common name for the given species of interest. You can use
spaces as separators or underscores. See the example below for
Pacific ocean perch.}

\item{dist}{A vector of distributional assumptions that you want to include
for the positive catch-rate model. Options are listed in the default call.
Please contact the package maintainer if you wish to use other distributions
because names other than those listed will not work.}
}
\value{
Nothing is returned from this function. Called functions save
information (e.g., plots and RData files) to the disk for viewing, further
research, or inclusion in reports.
}
\description{
\link{VAST_spp} is the highest-level function in \code{VASTWestCoast}.
This function runs \pkg{VAST} for each available data set in the
\href{https://www.webapp.nwfsc.noaa.gov/data}{NWFSC data warehouse}
and is a wrapper for \code{\link[=VAST_condition]{VAST_condition()}}; the function
can take hours to run.
}
\details{
\code{\link[=VAST_spp]{VAST_spp()}} is the highest-level function available in \code{VASTWestCoast}.
It will run combinations of data sets and model configurations that are
seen as standard for a given species.
The input argument \code{dist} is the only argument available to the user to
change which models are run. If you require more user functionality, e.g.,
want to run just a single model or a single survey please see
\code{\link[=VAST_do]{VAST_do()}} or \code{\link[=VAST_condition]{VAST_condition()}}, respectively.
By default, a model with a gamma distribution for positive catch rates and a
model with a lognormal distribution for positive catch rates are both run.
Though this can be changed by altering \code{dist}.

Resulting folders saved to the disk are structured hierarchically
based on the species name. Inside the main folder, in \code{dir},
will be a folder called data that contains one folder for each VAST run.
VAST runs are named according to the survey that collected the data and the
distribution used for positive catch rates.
This structure is thought to match that used by most assessment authors for
a single-species stock assessment.
All R objects are saved in \code{'Save.RData'}
so you can access them to rerun a model or manipulate the output.

This function, as well as others within VASTWestCoast, rely heavily on
\pkg{nwfscSurvey} to find out more information about the species
of interest, get the necessary data, and set various inputs to VAST.
\itemize{
\item Find surveys of interest in the data warehouse.
\code{\link[=VAST_spp]{VAST_spp()}} runs models in the following order:
\itemize{
\item NWFSC Groundfish Slope Survey,
\item NWFSC West Coast Groundfish Bottom Trawl Survey,
\item AFSC Slope Survey, and
\item Triennial Survey.
}
\item Find the strata for the species.
\item Run \code{\link[=VAST_condition]{VAST_condition()}} for each survey x distribution combo.
}
}
\examples{
\dontrun{

# Run both lognormal and gamma for sablefish
VAST_spp(dir = getwd(), species = "sablefish")

# Run just the gamma for POP, either line below will work
VAST_spp(dir = getwd(), species = "pacific_ocean_perch", dist = "gamma")
VAST_spp(dir = getwd(), species = "Pacific ocean perch", dist = "gamma")

# See a list of available species (takes a long time)
sppnames <- nwfscSurvey::PullSpp.fn()
# For Pacific ocean perch, you can use any of these options
sppnames[grep("ocean perch", sppnames[, "common"]), ]
# Vector of all available skates
grep("skate", sppnames[, "common"], value = TRUE)
}

}
\seealso{
See \code{\link[=VAST_condition]{VAST_condition()}} and \code{\link[=VAST_do]{VAST_do()}}, in that
order, for how to run VAST for a survey or a single VAST model, respectively.

See \code{\link[=check_rtools]{check_rtools()}} and \code{\link[=check_TMB]{check_TMB()}} if you cannot
get this function to run because, more than likely, rtools or TMB are improperly
installed. This is mainly relevant for those new to R 4.0+.
}
\author{
Kelli F. Johnson
}
