#' Convert osm_lines to a data.frame of sequential network connections
#'
#' @param lns An \code{sf} collection of \code{LINESTRING} objects, obtained for
#' example from the \code{osm_lines} component of an \code{osmdata} object
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
osmlines_as_network <- function (lns) 
{
    if (is (lns, 'osmdata'))
        lns <- lns$osm_lines
    else if (!is (lns$geometry, 'sfc_LINESTRING'))
        stop ("lns must be an 'sf' collection of 'LINESTRING' objects")

    res <- rcpp_lines_as_network (lns)
    data.frame (
                from_id = as.character (res [[2]] [,1]),
                from_x = res [[1]] [,1],
                from_y = res [[1]] [,2],
                to_id = as.character (res [[2]] [,2]),
                to_x = res [[1]] [,3],
                to_y = res [[1]] [,4],
                stringsAsFactors = FALSE
                )
}

