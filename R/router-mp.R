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
#' \dontrun{
#' netdf <- data.frame (
#'                      xfr = c (rep (0, 3), rep (1, 3), rep (2, 4),
#'                               rep (3, 3), rep (4, 2), rep (5, 3)),
#'                      xto = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
#'                               1, 2, 4, 3, 5, 0, 2, 4),
#'                      d = c (7., 9., 14., 7., 10., 15., 9., 10., 11., 2.,
#'                             15., 11., 6., 6., 9., 14., 2., 9.))
#' osm_router (netdf, 1, 5)
#' }
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
#' @param graphs \code{list} containing the two graphs and a map linking the two
#' to each other.
#' @param start_node Starting node for shortest path route
#' @param end_node Ending node for shortest path route
#' @param eta The parameter controlling the entropy (scale is arbitrary)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' netdf <- data.frame (
#'                      from_id = c (rep (0, 3), rep (1, 3), rep (2, 4),
#'                               rep (3, 3), rep (4, 2), rep (5, 3)),
#'                      to_id = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
#'                               1, 2, 4, 3, 5, 0, 2, 4),
#'                      d_weighted = c (7., 9., 14., 7., 10., 15., 9., 10.,11.,
#'                               2., 15., 11., 6., 6., 9., 14., 2., 9.))
#' getProbability (netdf, 1, 5)
#' }
getProbability <- function (graphs, start_node, end_node, eta=1)
{
    netdf <- graphs$compact
    if (!(is (netdf, 'data.frame')))
        stop ('graphs must contain a data.frame')
    if (!all (c ('from_id', 'to_id', 'd_weighted') %in% names (netdf)))
        stop ('compact graph must contain columns from_id, to_id and
              d_weighted')

    netdf_out <- netdf
    netdf <- data.frame (netdf$from_id, netdf$to_id, netdf$d_weighted)

    # force names for rcpp call
    cnames <- c ('xfr', 'xto', 'd')
    names (netdf) <- cnames
    start_node %<>% as.character %>% as.numeric
    end_node %<>% as.character %>% as.numeric
    netdf$xfr %<>% as.character %>% as.numeric
    netdf$xto %<>% as.character %>% as.numeric
    allids <- c (netdf$xfr, netdf$xto) %>% sort %>% unique 
    if (!start_node %in% allids)
        stop ('start_node is not part of netdf')
    if (!end_node %in% allids)
        stop ('end_node is not part of netdf')

    eta <- as.numeric (eta * nrow (netdf))
    probability <- rcpp_router_prob (netdf, start_node, end_node, eta)

    prb <- cbind (netdf_out, probability)
    graphs$compact <- prb
    mapProbabilities (graphs)
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
#' \dontrun{
#' netdf <- data.frame (
#'                      from_id = c (rep (0, 3), rep (1, 3), rep (2, 4),
#'                               rep (3, 3), rep (4, 2), rep (5, 3)),
#'                      to_id = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
#'                               1, 2, 4, 3, 5, 0, 2, 4),
#'                      d_weighted = c (7., 9., 14., 7., 10., 15., 9., 10.,11.,
#'                               2., 15., 11., 6., 6., 9., 14., 2., 9.))
#' getShortestPath (netdf, 1, 5)
#' }
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
    if (!start_node %in% allids)
        stop ('start_node is not part of netdf')
    if (!end_node %in% allids)
        stop ('end_node is not part of netdf')
    netdf$from_id <- vapply (netdf$from_id, function (x)
                             { which (allids == x) -1 }, 0.)
    netdf$to_id <- vapply (netdf$to_id, function (x)
                           { which (allids == x) -1 }, 0.)
    start_node <- which (allids == start_node) -1
    end_node <- which (allids == end_node) -1
    path <- rcpp_router_dijkstra (netdf, start_node, end_node)
    allids [path + 1]
}

#' Maps probabilities from the compact graph back on to the original graph
#'
#' @param graphs \code{list} containing the two graphs and a map linking the two
#' to each other.
#'
#' @return The original graph with the probabilities from the compact graph
#' mapped on it.
#'
#' @noRd
mapProbabilities <- function (graphs)
{
    orig <- graphs$original
    orig$probability <- 0
    orig$from_id <- as.character (orig$from_id)
    orig$to_id <- as.character (orig$to_id)
    comp <- graphs$compact
    comp$from_id <- as.character (comp$from_id)
    comp$to_id <- as.character (comp$to_id)
    map <- graphs$map
    
    for (i in seq_len (dim (map)[1]))
    {
        prob <- comp$probability [comp$edge_id == map$id_compact [i]]
        orig$probability [orig$edge_id == map$id_original [i]] <- prob
    }
    orig
}
