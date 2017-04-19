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
#'
#' @examples
#' netdf <- data.frame (
#'                      from_id = c (rep (0, 3), rep (1, 3), rep (2, 4),
#'                               rep (3, 3), rep (4, 2), rep (5, 3)),
#'                      to_id = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
#'                               1, 2, 4, 3, 5, 0, 2, 4),
#'                      d_weighted = c (7., 9., 14., 7., 10., 15., 9., 10.,11.,
#'                               2., 15., 11., 6., 6., 9., 14., 2., 9.))
#' prb <- getProbability (netdf, 1, 5)
getProbability <- function (netdf, start_node, end_node, eta=1)
{
    if (!(is (netdf, 'data.frame')))
        stop ('netdf must be a data.frame')
    if (!all (c ('from_id', 'to_id', 'd_weighted') %in% names (netdf)))
        stop ('netdf must contain columns from_id, to_id and d_weighted')

    netdf_out <- netdf
    netdf <- data.frame (netdf$from_id, netdf$to_id, netdf$d_weighted)

    # force names for rcpp call
    cnames <- c ('xfr', 'xto', 'd')
    names (netdf) <- cnames
    netdf$xfr <- as.numeric (as.character (netdf$xfr))
    netdf$xto <- as.numeric (as.character (netdf$xto))
    allids <- c (netdf$xfr, netdf$xto)
    allids <- unique (sort (allids))
    netdf$xfr <- vapply (netdf$xfr, function (x) { which (allids == x) -1 }, 0.)
    netdf$xto <- vapply (netdf$xto, function (x) { which (allids == x) -1 }, 0.)

    eta <- as.numeric (eta * nrow (netdf))
    prob <- rcpp_router_prob (netdf, start_node, end_node, eta)

    len <- dim (netdf) [1]
    probability <- vector (length = len)
    for (i in seq_len (len))
    {
        ln <- netdf [i, ]
        probability [i] <- prob [ln$xto + 1, ln$xfr + 1]
    }

    prb <- cbind (netdf_out, probability)
    prb
}

#' Calculate the shortest path between two nodes on a graph
#'
#' @param netdf \code{data.frame} of network
#' @param start_node Starting node for shortest path route
#' @param end_node Ending node for shortest path route
#'
#' @return a \code{vector} containing edge IDs of the shortest path
#'
#' @export
#'
#' @examples
#' netdf <- data.frame (
#'                      from_id = c (rep (0, 3), rep (1, 3), rep (2, 4),
#'                               rep (3, 3), rep (4, 2), rep (5, 3)),
#'                      to_id = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
#'                               1, 2, 4, 3, 5, 0, 2, 4),
#'                      d_weighted = c (7., 9., 14., 7., 10., 15., 9., 10.,11.,
#'                               2., 15., 11., 6., 6., 9., 14., 2., 9.))
#' start_node <- 1
#' end_node <- 5
#' shortest <- getShortestPath (netdf, 1, 5)
getShortestPath <- function (netdf, start_node, end_node)
{
    if (!(is (netdf, 'data.frame')))
        stop ('netdf must be a data.frame')
    if (!all (c ('from_id', 'to_id', 'd_weighted') %in% names (netdf)))
        stop ('netdf must contain columns from_id, to_id and d_weighted')
    netdf <- data.frame (netdf$from_id, netdf$to_id, netdf$d_weighted)
    cnames <- c ('from_id', 'to_id', 'd_weighted')
    names (netdf) <- cnames
    netdf$from_id <- as.numeric (as.character (netdf$from_id))
    netdf$to_id <- as.numeric (as.character (netdf$to_id))
    allids <- c (netdf$from_id, netdf$to_id)
    allids <- unique (sort (allids))
    if (is.na (allids [start_node]))
        stop ('start_node is not part of netdf')
    if (is.na (allids [end_node]))
        stop ('end_node is not part of netdf')
    netdf$from_id <- vapply (netdf$from_id, function (x)
                             { which (allids == x) -1 }, 0.)
    netdf$to_id <- vapply (netdf$to_id, function (x)
                           { which (allids == x) -1 }, 0.)
    path <- rcpp_router_dijkstra (netdf, start_node, end_node)
    allids [path]
}
