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
#'   dat <- readRDS ("../compact-ways-munich.Rda") %>% make_compact_graph
#'   st <- dat$compact$from_id [1]
#'   en <- dat$compact$to_id [10]
#'   get_probability (dat, st, en)
#' }
get_probability <- function (graphs, start_node, end_node, eta=1)
{
    check_graph_format (graphs)
    netdf <- graphs$compact
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

    probability <- r_router_prob (graphs, start_node, end_node, eta)

    prb <- cbind (netdf_out, probability$dens)
    graphs$compact <- prb
    map_probabilities (graphs)
}

#' Calculate the shortest path between two nodes on a graph
#'
#' @param graphs \code{list} containing the two graphs and a map linking the two
#' to each other.
#' @param start_node Starting node for shortest path route.
#' @param end_node Ending node for shortest path route.
#'
#' @return \code{data.frame} of the graph elements the shortest path lies on.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dat <- readRDS ("../compact-ways-munich.Rda") %>% make_compact_graph
#'   st <- dat$compact$from_id [1]
#'   en <- dat$compact$to_id [10]
#'   get_shortest_path (dat, st, en)
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
    map_shortest (graphs, path_compact)
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
    # scale eta to size of graph - TODO: Investigate this further!
    eta <- eta / nrow (graph$compact)

    frid <- graph$compact$from_id %>% as.character %>% as.numeric
    start_i <- which (frid == start_node)
    toid <- graph$compact$to_id %>% as.character %>% as.numeric
    end_i <- which (toid == end_node)

    netdf <- data.frame ('xfr' = graph$compact$from_id,
                         'xto' = graph$compact$to_id,
                         'd' = graph$compact$d)
    netdf$xfr %<>% as.character %>% as.numeric
    netdf$xto %<>% as.character %>% as.numeric
    allids <- c (netdf$xfr, netdf$xto) %>% sort %>% unique 

    # Set first node:
    orig_id <- netdf$xfr [start_i]
    dest_id <- netdf$xto [end_i]
    dest <- which (allids == netdf$xto [end_i])
    # Then insert connection to first node
    netdf <- rbind (c (1, orig_id, 0), netdf)
    allids <- c (1, allids)
    origin <- 1
    dest <- dest + 1
    # And add columns of sequential indices for each node
    netdf <- cbind ('ifr' = match (netdf$xfr, allids),
                    'ito' = match (netdf$xto, allids),
                    netdf)

    # Then begin the actual routing calculation
    nv <- length (allids)
    dmat <- array (NA, dim = rep (nv, 2))
    indx <- netdf$ifr + nv * (netdf$ito - 1)
    dmat [indx] <- netdf$d
    dmat_s <- dmat
    dmat_s [is.na (dmat_s)] <- 0
    dmat_s <- as (dmat_s, "dgCMatrix")
    p <- 1 / dmat_s # Eq.(4)
    p [dmat_s == 0] <- 0
    p <- as (p, "dgCMatrix")
    rs <- Matrix::rowSums (dmat_s, na.rm = TRUE)
    rs [rs == 0] <- Inf
    dmat_s <- dmat_s / rs

    W <- exp (-eta * p) * dmat_s # Eq.(33) (kinda)
    W [is.na (W)] <- 0

    Id <- Ij <- Matrix::Diagonal (nv)
    Ij [dest, dest] <- 0
    W <- Ij %*% W
    id_minus_w <- as ((Id - W), "dgCMatrix")

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
        Pr <- Pr [indx] [1] # rm 1st element

        CW <- dmat_s * W
        dij <- (t (z1) %*% CW %*% zn) / z1n
    }

    list ('dens' = Nvec, 'prob' = Pr, 'dist' = as.numeric (dij))
}
