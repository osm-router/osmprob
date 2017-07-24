#' Calculate a distance matrix between all pairs of a given list of points
#'
#' @param graph Graphs extracted from \link{download_graph}.
#' @param xy Matrix of two columns containing latitudes and longitudes of points
#' between which distances are to be calcualted.
#'
#' @note Different points may map on to the same network locations, in which
#' case they are excluded from distance calculation. The function returns an
#' index which can be used to directly extract the \code{xy} points which map on
#' to unique locations.
#'
#' @return A list of two items: A matrix of distances between all pairs of
#' points listed in \code{xy}, and an index of points in \code{xy} which map on
#' to unique graph nodes. The distance from point \code{a} to point \code{b} is
#' `$d [a, b]`.
#'
#' @export
distance_matrix <- function (graph, xy)
{
    edges <- cbind (paste0 (graph$compact$from_id),
                    paste0 (graph$compact$to_id))
    edges <- as.vector (t (edges))
    igr <- igraph::make_directed_graph (edges)
    igraph::E (igr)$weight <- graph$compact$d

    nodes <- snap_to_graph (graph, xy)
    indx <- which (!duplicated (nodes))
    nodes <- nodes [indx]

    list (indx = indx,
          d = igraph::distances (igr, v = nodes, to = nodes, mode = "out"))
}

#' quick and dirty snap xy points to closest graph nodes
#'
#' @return List of OSM ID values of closest nodes to xy
#'
#' @note \code{geosphere::distm} can be used, by takes many times longer.
#'
#' @noRd
snap_to_graph <- function (graph, xy)
{
    xygr <- rbind (cbind (graph$compact$from_lon, graph$compact$from_lat),
                   cbind (graph$compact$to_lon, graph$compact$to_lat))
    rownames (xygr) <- c (paste0 (graph$compact$from_id),
                          paste0 (graph$compact$to_id))
    xygr <- xygr [which (!duplicated (xygr)), ]

    x1mat <- matrix (xy [, 1], nrow = nrow (xy), ncol = nrow (xygr))
    y1mat <- matrix (xy [, 2], nrow = nrow (xy), ncol = nrow (xygr))
    x2mat <- t (matrix (xygr [, 1], nrow = nrow (xygr), ncol = nrow (xy)))
    y2mat <- t (matrix (xygr [, 2], nrow = nrow (xygr), ncol = nrow (xy)))

    d <- sqrt ( (x1mat - x2mat) ^ 2 + (y1mat - y2mat) ^ 2)
    rownames (xygr) [apply (d, 1, which.min)]
}
