#' Removes nodes and edges from a graph that are not needed for routing
#'
#' \code{make_compact_graph} takes a \code{data.frame} containing a graph and
#' removes all vertices not needed for routing as well as all vertices that are
#' not connected to the largest coherent part of the graph.
#'
#' @param graph \code{data.frame} of the graph to be processed.
#' @param quiet If FALSE, print progress information to screen.
#' @return \code{data.frame} containing the output graph.
#'
#' @noRd
make_compact_graph <- function (graph, quiet = FALSE)
{
    if (!is (graph, 'data.frame'))
        stop ('graph must be of type data.frame')
    rcpp_make_compact_graph (graph, quiet = quiet)
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
    graphs$d <- d
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
    for (i in seq_along (ways [, 1]))
    {
        way <- ways [i, ]
        e_id <- comp$edge_id [comp$from_id == way [1] & comp$to_id == way [2]]
        o_ids <- map$id_original [map$id_compact == e_id]
        for (o_id in o_ids)
        {
            orig_edge <- orig [orig$edge_id == o_id, ]
            if (dim (orig_edge) [1] == 1)
            {
                path [n, ] <- orig_edge
                n <- n + 1
            }
        }
    }
    path [complete.cases (path), ]
}

#' Checks if all necessary data are present in the graphs
#'
#' @param graph \code{list} containing the two graphs and a map linking the two
#' to each other OR just a plain graph.
#'
#' @noRd
check_graph_format <- function (graph)
{
    if (is (graph, "list"))
    {
        if (!all (c ("compact", "original", "map") %in% names (graph)))
            stop ("graph must contain data.frames compact, original and map.")
        com_graph <- graph$compact
        if (!(is (com_graph, "data.frame")))
            stop ("graph must contain a data.frame")
        if (!all (c ("from_id", "to_id", "d", "d_weighted") %in%
                  names (com_graph)))
            stop (paste0 ("compact graph must contain columns from_id, to_id, ",
                          "d and d_weighted"))
    } else
    {
        if (!all (c ("from_id", "to_id", "d", "d_weighted") %in% names (graph)))
            stop (paste0 ("compact graph must contain columns from_id, to_id, ",
                          "d and d_weighted"))
    }
}
