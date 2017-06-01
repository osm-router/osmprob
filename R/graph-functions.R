#' Removes nodes and edges from a graph that are not needed for routing
#'
#' \code{make_compact_graph} takes a \code{data.frame} containing a graph and
#' removes all vertices not needed for routing as well as all vertices that are
#' not connected to the largest coherent part of the graph.
#'
#' @param graph \code{data.frame} of the graph to be processed.
#' @return \code{data.frame} containing the output graph.
#'
#' @examples
#' \dontrun{
#' q <- osmdata::opq (bbox = c (11.58, 48.14, 11.585, 48.145))
#' q <- osmdata::add_feature (q, key = 'highway')
#' x <- osmdata::osmdata_sf(q)
#' net <- osmlines_as_network (x)
#' graph <- make_compact_graph (net)
#' }
#'
#' @export
make_compact_graph <- function (graph)
{
    if (!is (graph, 'data.frame'))
        stop ('graph must be of type data.frame')
    rcpp_make_compact_graph (graph)
}

#' Maps probabilities from the compact graph back on to the original graph
#'
#' @param graphs \code{list} containing the two graphs and a map linking the two
#' to each other.
#' @param d value of distance returned from r_router_prob
#'
#' @return The original graph with the probabilities from the compact graph
#' mapped on it.
#'
#' @noRd
map_probabilities <- function (graphs, d)
{
    # map is a matrix so must be directly indexed to (id_compact, id_original)
    #indx <- match (graph$map$id_compact, graph$compact$edge_id)
    indx <- match (graphs$map [, 1], graphs$compact$edge_id)
    graphs$original$dens <- graphs$compact$dens [indx]
    graphs$original$prob <- graphs$compact$prob [indx]
    graphs$distance <- d
    return (graphs)
}

#' Maps the shortest path back on to the original graph
#'
#' @param graphs \code{list} containing the two graphs and a map linking the two
#' to each other.
#' @param shortest \code{vector} containing the shortest path.
#'
#' @return \code{data.frame} of the graph elements the shortest path lies on.
#'
#' @noRd
map_shortest <- function (graphs, shortest)
{
    map <- graphs$map
    orig <- graphs$original
    comp <- graphs$compact
    ways <- cbind (utils::head (shortest, -1), shortest [-1])
    nms <- names (orig)
    path <- data.frame (matrix (ncol = length (nms), nrow = dim (map) [1]))
    names (path) <- nms
    n <- 1
    for (i in seq_along (ways [,1]))
    {
        way <- ways [i,]
        eId <- comp$edge_id [comp$from_id == way [1] & comp$to_id == way [2]]
        oIds <- map$id_original [map$id_compact == eId]
        for (oId in oIds)
        {
            orig_edge <- orig [orig$edge_id == oId, ]
            if (dim (orig_edge) [1] == 1)
            {
                path [n,] <- orig_edge
                n <- n + 1
            }
        }
    }
    path [complete.cases (path), ]
}

#' Checks if all necessary data are present in the graphs
#'
#' @param graphs \code{list} containing the two graphs and a map linking the two
#' to each other.
#'
#' @noRd
check_graph_format <- function (graphs)
{
    if (!all (c ('compact', 'original', 'map') %in% names (graphs)))
        stop ('graphs must contain data.frames compact, original and map.')
    netdf <- graphs$compact
    if (!(is (netdf, 'data.frame')))
        stop ('graphs must contain a data.frame')
    if (!all (c ('from_id', 'to_id', 'd_weighted') %in% names (netdf)))
        stop ('compact graph must contain columns from_id, to_id and
              d_weighted')
}
