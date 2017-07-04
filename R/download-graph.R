#' Download OSM road graph and preprocess it
#'
#' This function uses \code{osmdata} to download a OSM street graph. The extent
#' of the downloaded area is defined by the start and end coordinates of the
#' desired path plus a buffer, that extends the bounding box in all directions.
#' The graph is then preprocessed for routing purposes, which includes removing
#' topologically unnecessary parts and calculating edge weights based on the
#' selected weighting profile.
#'
#' @param start_pt Two numeric values (latitude, longitude) as start point
#' coordinates.
#' @param end_pt Two numeric values (latitude, longitude) as end point
#' coordinates.
#' @param weighting_profile Name of the used weighting profile.
#' \code{osmprob::weighting_profiles} contains all available profiles.
#' @param buffer Positive value that defines by how much (in percent) should the
#' downloaded data extend the bounding box defined by \code{start_pt} and
#' \code{end_pt}.
#'
#' @return graphs \code{list} containing the original street graph, a minimized
#' graph map linking the two to each other.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' start_pt <- c (11.580, 48.140)
#' end_pt <- c (11.585, 48.145)
#' graph <- download_graph (start_pt = start_pt, end_pt = end_pt,
#' weighting_profile = "bicycle", buffer = 0)
#' }
download_graph <- function (start_pt, end_pt, weighting_profile = "bicycle",
                            buffer = 0)
{
    bbx <- make_bbox (start_pt, end_pt, buffer)
    dat <- osmdata::opq (bbox = bbx) %>%
        osmdata::add_feature (query, key = 'highway') %>%
        osmdata::osmdata_sf (query)
    osmlines_as_network (dat, profile_name = weighting_profile) %>%
        make_compact_graph ()
}

shiftx180 <- function (x)
{
    while (x > 180)
        x <- x - 180
    while (x < -180)
        x <- x + 180

    return (x)
}

shifty90 <- function (y)
{
    while (y > 90)
        y <- y - 90
    while (y < -90)
        y <- y + 90

    return (y)
}

make_bbox <- function (start_pt, end_pt, buffer)
{
    buffer <- max (buffer, 0)
    buffer <- buffer / 100

    if (start_pt [2] < -90 | start_pt [2] > 90)
        stop ("Latitude of start_pt is invalid.")
    if (start_pt [1] < -180 | start_pt [1] > 180)
        stop ("Longitude of start_pt is invalid.")
    if (end_pt [2] < -90 | end_pt [2] > 90)
        stop ("Latitude of end_pt is invalid.")
    if (end_pt [1] < -180 | end_pt [1] > 180)
        stop ("Longitude of end_pt is invalid.")

    dx <- abs (start_pt [1] - end_pt [1]) * buffer
    dy <- abs (start_pt [2] - end_pt [2]) * buffer

    minx <- shiftx180 (min (start_pt [1], end_pt [1]) - dx)
    maxx <- shiftx180 (max (start_pt [1], end_pt [1]) + dx)
    miny <- shifty90 (min (start_pt [2], end_pt [2]) - dy)
    maxy <- shifty90 (max (start_pt [2], end_pt [2]) + dy)

    c (minx, miny, maxx, maxy)
}
