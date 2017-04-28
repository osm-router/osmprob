#' Removes nodes and edges from a graph that are not needed for routing
#'
#' \code{makeCompactGraph} takes a \code{data.frame} containing a graph and
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
#' graph <- makeCompactGraph (net)
#' }
#'
#' @export
makeCompactGraph <- function (graph)
{
    if (!is (graph, 'data.frame'))
        stop ('graph must be of type data.frame')
    rcpp_makeCompactGraph (graph)
}
