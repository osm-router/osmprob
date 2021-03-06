#' Convert osm_lines to a data.frame of sequential network connections
#'
#' @param lns An \code{sf} collection of \code{LINESTRING} objects, obtained for
#' example from the \code{osm_lines} component of an \code{osmdata} object
#' @param profile_name Name of the used weighting profile.
#' \code{osmprob::weighting_profiles} contains all available profiles.
#'
#' @return \code{data.frame} of all pairs of connected nodes
#'
#' @noRd
osmlines_as_network <- function (lns, profile_name = "bicycle")
{
    if (is (lns, 'osmdata'))
        lns <- lns$osm_lines
    else if (!is (lns$geometry, 'sfc_LINESTRING'))
        stop ("lns must be an 'sf' collection of 'LINESTRING' objects")

    profiles <- osmprob::weighting_profiles
    profiles <- profiles [profiles$name == profile_name, ]
    profiles$value <- profiles$value / 100
    res <- rcpp_lines_as_network (lns, profiles)
    data.frame (
                edge_id = seq (nrow (res [[1]])),
                from_id = as.character (res [[2]] [, 1]),
                from_lon = res [[1]] [, 1],
                from_lat = res [[1]] [, 2],
                to_id = as.character (res [[2]] [, 2]),
                to_lon = res [[1]] [, 3],
                to_lat = res [[1]] [, 4],
                d = res [[1]] [, 5],
                d_weighted = res [[1]] [, 6],
                highway = as.character (res [[2]] [, 3]),
                stringsAsFactors = FALSE
                )
}
