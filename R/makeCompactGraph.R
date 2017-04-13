#' Removes nodes and edges from a graph that are not needed for routing
#'
#' \code{makeCompactGraph} takes a \code{data.frame} containing a graph and
#' removes all vertices not needed for routing as well as all vertices that are
#' not connected to the largest coherent part of the graph.
#'
#' @param graph \code{data.frame} of the graph to be processed
#' @return \code{data.frame} containing the output graph
#'
#' @export
makeCompactGraph <- function (graph)
{
    if (!is (graph, 'data.frame'))
        stop ('graph must be of type data.frame')
    rcpp_makeCompactGraph (graph)
}
