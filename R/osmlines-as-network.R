#' Convert osm_lines to a data.frame of sequential network connections
#'
#' @param lns An \code{sf} collection of \code{LINESTRING} objects, obtained for
#' example from the \code{osm_lines} component of an \code{osmdata} object
#' @param profileName Name of the used weighting profile
#' @param profileFile path to the weighting profiles
#'
#' @return \code{data.frame} of all pairs of connected nodes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' q <- osmdata::opq ('City of London')
#' q <- osmdata::add_feature (q, key = 'highway')
#' x <- osmdata::osmdata_sf(q)
#' net <- osmlines_as_network (x)
#' }
osmlines_as_network <- function (lns, profileName = "bicycle",
                                 profileFile = "../data/weightingProfiles.rda")
{
    if (is (lns, 'osmdata'))
        lns <- lns$osm_lines
    else if (!is (lns$geometry, 'sfc_LINESTRING'))
        stop ("lns must be an 'sf' collection of 'LINESTRING' objects")

    profiles <- load (profileFile)
    profiles <- get (profiles)
    profiles <- profiles [profiles$name == profileName, ]
    profiles$value <- 1/profiles$value
    res <- rcpp_lines_as_network (lns, profiles)
    data.frame (
                from_id = as.character (res [[2]] [, 1]),
                from_x = res [[1]] [, 1],
                from_y = res [[1]] [, 2],
                to_id = as.character (res [[2]] [, 2]),
                to_x = res [[1]] [, 3],
                to_y = res [[1]] [, 4],
                d = res [[1]] [, 5],
                d_weighted = res [[1]] [, 6],
                stringsAsFactors = FALSE
                )
}
