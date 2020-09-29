#' Get Mesh for West Coast
#'
#' Create a mesh for the US West Coast. This method provides
#' a work around for needing a kmeans file.
#'
#' @param data.inner A data frame of points with at least two columns,
#' \code{"Lon"} specifying the Longitude and
#' \code{"Lat"} specifying the Latitude.
#' The locations will be used to define the inner boundary of the
#' triangulation mesh and starting seeds for the knot locations.
#' @param data.outer A data frame of points with at least two columns,
#' \code{"Lon"} specifying the Longitude and
#' \code{"Lat"} specifying the Latitude.
#' The locations will be used to define the outer boundary of the
#' triangulation mesh.
#' @template numknots
#'
#' @export
#' @author Kelli Faye Johnson
#' @return
#' An \code{inla.mesh} list object created with
#' \code{\link[INLA]{inla.mesh.create}()}.
#' The resulting list has 8 default elements from INLA and a few additional
#' elements that are added specifically for VASTWestCoast, including the
#' coordinates used to create the mesh. Of most interest is probably the
#' meta data, which is stored in \code{"meta"} and includes the call to
#' \code{\link[INLA]{inla.mesh.create}()} and the \code{fmesher.args} used
#' internally. The call to \code{get_mesh()} is also stored in \code{"call"}.
#'
#' Users can compare the desired number of knots \code{n.desired} to the
#' actual number of knots \code{n}. The number of knots should always be within
#' plus or minus five percent of desired number unless a solution is not
#' found within 100 tries, which is the current cap on the number of times
#' the code will run through the \code{while} loop.
#'
get_mesh <- function(data.inner, data.outer, numknots) {

  set.seed(2)
  aa <- FishStatsUtils::project_coordinates(
    X = data.outer[, c("Lon")],
    Y = data.outer[, c("Lat")])
  coords <- FishStatsUtils::project_coordinates(
    X = data.inner[, c("Lon")],
    Y = data.inner[, c("Lat")])
  boundaryhull <- INLA::inla.nonconvex.hull(aa, convex = -0.05)

  new <- list(n = Inf)
  cutoff <- 15
  counter <- 0
  percent <- 0.05
  increment <- 1
  while (new$n > numknots * (1 + percent) |
         new$n < numknots * (1 - percent)) {
    cutoff <- ifelse(new$n > numknots * (1 + percent),
      cutoff + increment, cutoff - increment)
    counter <- counter + 1
    new <- INLA::inla.mesh.create(coords,
      boundary = boundaryhull,
      cutoff = cutoff)
    if (counter > 100) break
  }

  new[["data.inner"]] <- data.frame(data.inner, coords)
  new[["data.outer"]] <- data.frame(data.outer, aa)
  new[["n.desired"]] <- numknots
  new[["call"]] <- match.call()

  return(new)
}

#' Get Mesh Using Settings from TMB Example
#'
#' Get a triangulation mesh based on the same settings that are used for the
#' [example](https://github.com/kaskr/adcomp/blob/master/tmb_examples/spde_mesh.R)
#' in Template Model Builder (TMB).
#' This function acts as a way to offer documentation and comparison to other
#' ways to create meshes and is not actually used within the package.
#'
#' @template loc
#'
get_mesh.tmb <- function(loc) {
  new <- INLA::inla.mesh.2d(
    # location seeds
    loc = loc,
    # Encapsulate data region:
    boundary = list(
      INLA::inla.nonconvex.hull(loc, convex=0.05),
      INLA::inla.nonconvex.hull(loc, convex=0.25)),
    # Refined triangulation
    min.angle = 24,
    max.edge = c(0.05, 0.2), # interior and exterior maximal edge length
    cutoff = 0.005, # don't add input points closer than 0.005
    plot.delay = 0.5 # no effect Windows
  )
  return(new)
}

#' Get Mesh Using Settings from \code{\link[INLA]{meshbuilder}()}
#'
#' Get a triangulation mesh based on the same settings that are used for the
#' \code{\link[INLA]{meshbuilder}()} function.
#' This function acts as a way to offer documentation and comparison to other
#' ways to create meshes and is not actually used within the package.
#'
#' @template loc
#'
get_mesh.meshbuilder <- function(loc) {
  set.seed(1416528768)
  new <- INLA::inla.mesh.2d(
    loc = loc,
    boundary = list(
      INLA::inla.nonconvex.hull(sp::coordinates(loc), 0.1),
      INLA::inla.nonconvex.hull(sp::coordinates(loc), 0.3)),
    max.edge = c(0.05, 0.5),
    min.angle = c(35, 21),
    max.n = c(48000, 16000), # Safeguard against large meshes.
    max.n.strict = c(128000, 128000), # Don't build a huge mesh!
    cutoff = 0.10, # Filter away adjacent points.
    offset = c(0.1, 0.3)) # Offset for extra boundaries, if needed.
  return(new)
}
