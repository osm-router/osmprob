#' router test
#'
#' @param netdf \code{data.frame} of network
#' @param start_node Starting node for shortest path route
#' @param end_node Ending node for shortest path route
#' @param eta The parameter controlling the entropy (scale is arbitrary)
#'
#' @note Function called 'osm_router' to avoid ambiguities with all other things
#' that might be called 'router'. \code{netdt} must have just three columns of
#' \code{xfr}, \code{xto}, and \code{d}, where the first two are simple integer
#' indices.
#'
#' @export
#'
#' @examples
#' netdf <- data.frame (
#'                      xfr = c (rep (0, 3), rep (1, 3), rep (2, 4),
#'                               rep (3, 3), rep (4, 2), rep (5, 3)),
#'                      xto = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
#'                               1, 2, 4, 3, 5, 0, 2, 4),
#'                      d = c (7., 9., 14., 7., 10., 15., 9., 10., 11., 2.,
#'                             15., 11., 6., 6., 9., 14., 2., 9.))
#' osm_router (netdf, 1, 5)
osm_router <- function (netdf, start_node, end_node, eta=1)
{
    if (!(is (netdf, 'data.frame') | is (netdf, 'matrix')))
        stop ('netdf must be a data.frame')
    if (ncol (netdf) != 3)
        stop ('netdf must have 3 columns')

    # force names for rcpp call
    cnames <- c ('xfr', 'xto', 'd')
    if (is.data.frame (netdf))
        names (netdf) <- cnames
    else
        colnames (netdf) <- cnames

    eta <- eta * nrow (netdf)
    rcpp_router (netdf, as.integer (start_node), as.integer (end_node), 
                 as.numeric (eta))
}

#' Calculate routing probabilities for a data.frame
#'
#' @param netdf \code{data.frame} of network
#' @param start_node Starting node for shortest path route
#' @param end_node Ending node for shortest path route
#' @param eta The parameter controlling the entropy (scale is arbitrary)
#'
#' @export
getProbability <- function (netdf, start_node, end_node, eta=1)
{
    if (!(is (netdf, 'data.frame') | is (netdf, 'matrix')))
        stop ('netdf must be a data.frame')
    netdf_out <- netdf
    netdf <- data.frame (netdf$from_id, netdf$to_id, netdf$d_weighted)

    # force names for rcpp call
    cnames <- c ('xfr', 'xto', 'd')
    names (netdf) <- cnames
    netdf$xfr <- as.numeric (as.character (netdf$xfr))
    netdf$xto <- as.numeric (as.character (netdf$xto))
    allids <- c (netdf$xfr, netdf$xto)
    allids <- unique (sort (allids))
    netdf$xfr <- vapply (netdf$xfr, function (x) {which (allids == x) }, 0.)
    netdf$xto <- vapply (netdf$xto, function (x) {which (allids == x) }, 0.)

    eta <- as.numeric (eta * nrow (netdf))
    prob <- rcpp_router_prob (netdf, start_node, end_node, eta)
    probability <- prob [prob != 0]
    probability <- probability [-1]
    prb <- cbind (netdf_out, probability)   
}
