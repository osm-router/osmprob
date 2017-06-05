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
#' osm_router (netdf = netdf, start_node = 1, end_node = 5, eta = 1)
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
#' @return \code{list} containing the \code{data.frame} of the graph elements
#' with the routing probabilities and the estimated probabilistic distance.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   graph <- road_data_sample
#'   start_pt <- c (11.603,48.163)
#'   end_pt <- c (11.608,48.167)
#'   pts <- select_vertices_by_coordinates (graph, start_pt, end_pt)
#'   route_start <- pts[1]
#'   route_end <- pts [2]
#'   get_probability (graphs = graph, start_node = route_start,
#'   end_node = route_end, eta = 0.6)
#' }
get_probability <- function (graphs, start_node, end_node, eta=1)
{
    check_graph_format (graphs)
    netdf <- graphs$compact

    start_node %<>% as.character %>% as.numeric
    end_node %<>% as.character %>% as.numeric

    probability <- r_router_prob (graphs, start_node, end_node, eta)

    graphs$compact <- cbind (netdf, 'dens' = probability$dens,
                             'prob' = probability$prob)
    mapped <- map_probabilities (graphs, probability$dist)
    list ('probability' = mapped$original, 'd' = probability$dist)
}

#' Calculate the shortest path between two nodes on a graph
#'
#' @param graphs \code{list} containing the two graphs and a map linking the two
#' to each other.
#' @param start_node Starting node for shortest path route.
#' @param end_node Ending node for shortest path route.
#'
#' @return \code{list} containing the \code{data.frame} of the graph elements
#' the shortest path lies on and the path distance.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   graph <- road_data_sample
#'   start_pt <- c (11.603,48.163)
#'   end_pt <- c (11.608,48.167)
#'   pts <- select_vertices_by_coordinates (graph, start_pt, end_pt)
#'   route_start <- pts[1]
#'   route_end <- pts [2]
#'   get_shortest_path (graphs = graph, start_node = route_start,
#'   end_node = route_end)
#' }
get_shortest_path <- function (graphs, start_node, end_node)
{
    check_graph_format (graphs)
    netdf <- graphs$compact
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
    path_compact <- allids [path + 1]
    mapped <- map_shortest (graphs = graphs, shortest = path_compact)
    distance <- sum (mapped$d)
    list ('shortest' = mapped, 'd' = distance)
}


#' Probabilistic router adapted from \code{gdistance} code
#'
#' @param graphs \code{list} containing the two graphs and a map linking the two
#' to each other.
#' @param start_node Starting node for shortest path route given as OSM ID
#' @param end_node Ending node for shortest path route given as OSM ID
#' @param eta The parameter controlling the entropy (scale is arbitrary)
#'
#' @return A list of three items, the first two of which match the edges in the
#' compact graph:
#' \itemize{
#' \item Vector of edge traversal densities (\code{dens})
#' \item Vector of edge traversal probabilities (\code{prob})
#' \item Single value of total probabilistic distance (\code{d})
#' }
#'
#' @noRd
r_router_prob <- function (graph, start_node, end_node, eta)
{
    netdf <- data.frame ('xfr' = graph$compact$from_id,
                         'xto' = graph$compact$to_id,
                         'd' = graph$compact$d,
                         'd_weighted' = graph$compact$d_weighted)
    netdf$xfr %<>% as.character %>% as.numeric
    netdf$xto %<>% as.character %>% as.numeric
    allids <- c (netdf$xfr, netdf$xto) %>% sort %>% unique 

    # Insert connection to first node
    node1_id <- min (allids) - 1
    netdf <- rbind (c (node1_id, start_node, 0), netdf)
    allids <- c (node1_id, allids)
    origin <- 1
    dest <- which (allids == end_node)

    # And add columns of sequential indices for each node
    netdf <- cbind ('ifr' = match (netdf$xfr, allids),
                    'ito' = match (netdf$xto, allids),
                    netdf)

    # Then begin the actual routing calculation
    nv <- length (allids)
    # The cost matrix for all but the terminal node
    dmat <- cmat <- Matrix::Matrix (0, nv, nv)
    indx <- netdf$ifr + nv * (netdf$ito - 1)
    cmat [indx] <- netdf$d_weighted
    dmat [indx] <- netdf$d

    # Intermediate transition probabilities from each node
    pmat <- cmat
    pmat [indx] <- 1 / cmat [indx]
    pmat [indx [1]] <- 1 # first redundant node
    rs <- Matrix::rowSums (pmat)
    rs [rs > 0] <- 1 / rs [rs > 0]
    pmat <- pmat * rs

    # Weight matrix W
    W <- exp (-eta * cmat) * pmat # Eq.(33) (kinda)
    if (any (!is.finite (W)))
    {
        Wvals <- W [indx]
        i <- which (!is.finite (Wvals))
        W [indx [i]] <- 0
    }


    Id <- Ij <- Matrix::Diagonal (nv)
    Ij [dest, dest] <- 0
    W <- Ij %*% W
    id_minus_w <- Id - W

    e1 <- en <- rep (0, nv)
    e1 [netdf$ito [1]] <- 1
    en [dest] <- 1

    z1 <- solve (Matrix::t (id_minus_w), e1)
    zn <- solve (id_minus_w, en)
    z1n <- sum (e1 * zn)
    Nvec <- Pr <- rep (NA, nv)
    dij <- 0
    if (z1n > 1e-300)
    {
        N <- (Matrix::Diagonal (nv, as.vector (z1)) %*% W %*% 
              Matrix::Diagonal(nv, as.vector (zn))) / z1n

        Nvec <- N [indx] [-1] # rm 1st element

        #not efficient but effective
        n <- pmax (Matrix::rowSums (N), Matrix::colSums (N))

        rn <- rep (0, times = length (nv))
        rn [n > 0] <- 1 / n [n > 0]
        Pr <- N * rn
        Pr <- Pr [indx] [-1] # rm 1st element

        CW <- dmat * W
        dij <- (t (z1) %*% CW %*% zn) / z1n
    }

    list ('dens' = Nvec, 'prob' = Pr, 'dist' = as.numeric (dij))
}
