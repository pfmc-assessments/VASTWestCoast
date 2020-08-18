#' @param overdispersion A vector of two integer values named "eta1" and "eta2"
#' that specify how overdispersion is modeled.
#' \code{"eta1"} is for the encounter model and
#' \code{"eta2"} is for the positive catch-rate model.
#' Typical options include
#' \code{c("eta1"=0, "eta2"=0)}, which is used for the
#' Alaska Fisheries Science Center Slope Survey, and
#' \code{c("eta1"=1, "eta2"=1)}, which is used for the
#' Northwest Fisheries Science Center
#' West Coast Groundfish Bottom Trawl Survey.
#' This vector is passed to \code{"OverdispersionConfig"} within the
#' \code{\link[VAST]{make_data}()} function.
#' The former configuration of zero turns off this feature in VAST.
#' The latter configuration allows for random covariation in catchability
#' (e.g., a vessel effect if v_i is assigned to vessel or a
#' vessel-year effect if v_i is assigned to a vector that combines vessel
#' and year into a factor). Specifically,
#' one random effect will be estimated for each unique value in v_i
#' if this feature is set to one.
#' The eta values can also be set to \code{"AR1"} if you want correlated
#' overdispersion in the available categories. The autoregressive process
#' might be appropriate if you believe there is overdispersion at the tow
#' level and tows closer in time provide more information about other tows than
#' tows that are further apart in time.
#' L_1 and L_2 are the estimated parameters representing the variance of these
#' random effects for each model.
#' The configuration for the overdispersion parameters is automatically set in
#' \code{\link{VAST_condition}} based on the input argument \code{spp},
#' which includes a short-hand version of the survey name. Leaving the input
#' value to the default of \code{NULL} allows the overdispersion parameter to
#' be set behind the scenes. This can be overridden by providing a vector.
#' If there is no default value for overdispersion, then a named vector
#' must be supplied where the first value is for the encounter model and the
#' second value is for the positive catch-rate model as given in the examples
#' above for the different surveys.
