#' Download osm data and calculate street lengths
#' 
#' @param bbox Numeric bounding box
#'
#' @return data.frame containing the osm data
#' @export
getData <- function (bbox=c(-0.27,51.47,-0.20,51.50))
{
    #q0 <- osmdata::opq (bbox=bbox)
    #q2 <- osmdata::add_feature (q0, key='highway')
    #h <- osmdata::osmdata_sf (q2)
    h <- 1
    lines <- h$osm_lines
    g <- lines$geometry
    mat <- matrix (0, nrow=length (g), ncol=7)
    for (ln in 1:length (g))
    {
        geom <- g [[ln]]
        n <- rownames (geom)
        mat [ln, 1] <- n [1]
        mat [ln, 2] <- n [length (n)]
        mat [ln, 3] <- geom [,1] [1]
        mat [ln, 4] <- geom [,2] [1]
        mat [ln, 5] <- geom [,1] [length (geom [,1])]
        mat [ln, 6] <- geom [,2] [length (geom [,2])]
        mat [ln, 7] <- getDist (geom [,1], geom [,2])
    }
    osmData <- as.data.frame (mat)
    colnames (osmData) <- c ("from_id", "to_id", "from_lon", "from_lat",
                             "to_lon", "to_lat", "distance")
    return (osmData)
}
