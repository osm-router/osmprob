#' Convert graph stored in \code{data.frame} to \code{sf}
#'
#' @param dat \code{data.frame} containing graph data.
#'
#' @noRd
get_graph <- function (dat)
{
    dat$from_lat %<>% as.character %>% as.numeric
    dat$from_lon %<>% as.character %>% as.numeric
    dat$to_lat %<>% as.character %>% as.numeric
    dat$to_lon %<>% as.character %>% as.numeric

    from <- cbind (dat$from_lon, dat$from_lat)
    to <- cbind (dat$to_lon, dat$to_lat)
    fromTo <- cbind (from, to)
    graphLines <- list ("LINESTRING", dim (fromTo) [1])
    for (i in 1:dim (fromTo) [1])
    {
        pair <- fromTo [i,]
        graphLines [[i]] <- sf::st_linestring (rbind (c (pair [1], pair [2]),
                                                      c (pair [3], pair [4])))
    }

    graph <- sf::st_sfc (graphLines, crs = 4326)
    lt_ln <- c ("from_lat", "from_lon", "to_lat", "to_lon")
    dat [lt_ln] <- NULL
    graph <- sf::st_sf (graph, dat)

    graph
}

#' Convert points stored in \code{data.frame} to \code{sf} points
#'
#' @param dat \code{data.frame} containing graph data.
#'
#' @noRd
points_as_sf <- function (dat)
{
    dat$from_lat %<>% as.character %>% as.numeric
    dat$from_lon %<>% as.character %>% as.numeric
    dat$to_lat %<>% as.character %>% as.numeric
    dat$to_lon %<>% as.character %>% as.numeric

    pts_from <- list ("POINT", dim (dat [1]))
    pts_to <- list ("POINT", dim (dat [1]))
    for (i in 1:dim (dat) [1])
    {
        ln <- dat [i, ]
        pts_from [[i]] <- sf::st_point (c (ln$from_lon, ln$from_lat))
        pts_to [[i]] <- sf::st_point (c (ln$to_lon, ln$to_lat))
    }
    pts_from <- sf::st_sfc (pts_from, crs = 4326)
    pts_to <- sf::st_sfc (pts_to, crs = 4326)
    list (from = pts_from, to = pts_to)
}

#' Select vertices on graph that are closest to the specified coordinates.
#'
#' @param graph \code{data.frame} containing the street network.
#' @param start_coords \code{numeric} coordinates of the start point.
#' @param end_coords \code{numeric} coordinates of the end point.
#'
#' @return \code{list} containing the two rows of the input \code{data.frame}
#' that are closest to the start and end coordinates
#'
#' @export
select_vertices_by_coordinates <- function (graph, start_coords, end_coords)
{
    st <- sf::st_point (start_coords) %>% sf::st_sfc (crs = 4326)
    en <- sf::st_point (end_coords) %>% sf::st_sfc (crs = 4326)
    if (is.list (graph) & "compact" %in% names (graph))
        graph <- graph$compact
    pts <- points_as_sf (graph)

    min_st_distance <- .Machine$integer.max
    min_st_index <- -1
    min_en_distance <- .Machine$integer.max
    min_en_index <- -1
    for (i in seq_len (length (pts$from)))
    {
        dist_st <- sf::st_distance (st, pts$from [i]) %>% as.numeric
        if (dist_st < min_st_distance)
        {
            min_st_distance <- dist_st
            min_st_index <- i
        }
        dist_en <- sf::st_distance (en, pts$to [i]) %>% as.numeric
        if (dist_en < min_en_distance)
        {
            min_en_distance <- dist_en
            min_en_index <- i
        }
    }
    start_id <- graph [min_st_index, "from_id"] %>% as.character
    end_id <- graph [min_en_index, "to_id"] %>% as.character
    c (start_id, end_id)
}
