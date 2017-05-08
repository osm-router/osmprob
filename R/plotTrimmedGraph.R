#' Plots the graph network as a Shiny Leaflet app in a browser.
#'
#' @param graph \code{data.frame} containing the street graph to be
#' displayed.
#' @param shortest \code{vector} containing the shortest path.
#'
#' @examples
#' \dontrun{
#' q <- osmdata::opq (bbox = c (11.58, 48.14, 11.585, 48.145))
#' q <- osmdata::add_feature (q, key = 'highway')
#' x <- osmdata::osmdata_sf(q)
#' net <- osmlines_as_network (x)
#' graph <- makeCompactGraph (net)
#' startPt <- graph$from_id [1]
#' endPt <- graph$to_id [100]
#' prob <- getProbability (graph, startPt, endPt)
#' short <- getShortestPath (graph, startPt, endPt)
#' plotMap (prob, short)
#' }
#'
#' @export
plotMap <- function (graph, shortest)
{
    # graph and shortestPath can't be passed as a parameter, so it is passed to
    # the server function via an environment variable
    inputGraph <<- graph
    shortestPath <<- shortest
    shiny::shinyApp(ui, server)
}

inputGraph <- ""
shortestPath <- ""

#' Converts graph stored in \code{data.frame} to \code{sf}
#'
#' @param dat \code{data.frame} containing graph data.
#'
#' @noRd
getGraph <- function (dat)
{
    dat$from_lat %<>% as.character %>% as.numeric
    dat$from_lon %<>% as.character %>% as.numeric
    dat$to_lat %<>% as.character %>% as.numeric
    dat$to_lon %<>% as.character %>% as.numeric

    from <- cbind (dat$from_lon, dat$from_lat)
    to <- cbind (dat$to_lon, dat$to_lat)
    fromTo <- cbind (from, to)
    graphLines <- list ("LINESTRING", dim (fromTo)[1])
    for (i in 1:dim (fromTo)[1])
    {
        pair <- fromTo [i,]
        graphLines [[i]] <- sf::st_linestring (rbind (c (pair[1], pair[2]),
                                                      c (pair[3], pair[4])))
    }

    graph <- sf::st_sfc (graphLines, crs = 4326)
    ltLn <- c ("from_lat", "from_lon", "to_lat", "to_lon")
    dat [ltLn] <- NULL
    graph <- sf::st_sf (graph, dat)

    graph
}

lnColor <- function (x, colorBy) { leaflet::colorQuantile (x, colorBy, n = 9) }

ui <- shiny::bootstrapPage (
  shiny::tags$style (type = "text/css", "html, body {width:100%;height:100%}
              .checkbox, .control-label{color: #FFFFFF}"),
  leaflet::leafletOutput ("map", width = "100%", height = "100%")
)

#' Generates text for edge popup fields on the graph
#'
#' @param fromid OSM ID of the edge beginning.
#' @param fromid OSM ID of the edge end.
#' @param weight \code{numeric} value of the edge weight.
#' @param prob \code{numeric} value of the edge traversal probability.
#'
#' @noRd
popup <- function (fromid, toid, weight, prob)
{
  paste ("<b>From ID: </b>", fromid,
         "</br><b>To ID: </b>", toid,
         "</br><b>Weight: </b>", format (round (weight, 3), nsmall = 3),
         "</br><b>Probability: </b>", format (round (prob, 3), nsmall = 3))
}

#' Generates \code{leaflet} HTML widget containing a web map
#'
#' @param dat \code{sf} object containing street graph data.
#' @param short \code{sf} object containing the shortest path between two
#' points.
#'
#' @noRd
getMap <- function (dat, short)
{
    dat <- subset (dat, !is.na (dat$probability))
    grpPrb <- "Probabilities"
    grpSrt <- "Shortest Path"
    grpSE <- "Start and end points"
    colorscheme <- "YlGnBu"
    print_weights <- dat$d_weighted / sf::st_length (dat) 
    pal <- lnColor (colorscheme, print_weights)
    startPt <- sf::st_coordinates (short [1, ])[1, 2:1]
    endPt <- sf::st_coordinates (utils::tail (short, 1))[1, 2:1]
    bb <- as.vector (sf::st_bbox (dat))

    leaflet::leaflet (data = dat,
                      options = leaflet::leafletOptions ()) %>%
    leaflet::addProviderTiles ('CartoDB.DarkMatter', group = "base") %>%
    leaflet::addPolylines (color = ~pal (print_weights), opacity = 1.0,
                           weight = getWidth (3, 8, dat$probability),
                           popup = popup (dat$from_id, dat$to_id,
                                          dat$d_weighted, dat$probability),
                           group = grpPrb) %>%
    leaflet::addPolylines (data = short, color = "#FF0000", opacity = 1.0,
                           weight = 4, group = grpSrt, dashArray = "20, 20") %>%
    leaflet::addCircleMarkers (stroke = FALSE, startPt [2], startPt [1],
                               group = grpSE, color = "#FFFF00",
                               fillOpacity = 1.0, radius = 10) %>%
    leaflet::addCircleMarkers (stroke = FALSE, endPt [2], endPt [1],
                               group = grpSE, color = "#0066FF",
                               fillOpacity = 1.0, radius = 10) %>%
    leaflet::addLayersControl (overlayGroups = c (grpPrb, grpSE, grpSrt),
                               options = leaflet::layersControlOptions
                               (collapsed = FALSE)) %>%
    leaflet::addLegend (position = "bottomright", pal = pal,
                        values = print_weights,
                        title = "Edge Weight (Quantile)") %>%
    leaflet::fitBounds (bb[1], bb[2], bb[3], bb[4])
}

#' Calculates displayed line width
#'
#' @param base \code{numeric} value of minimum width.
#' @param fac \code{numeric} value of facor to be multiplied width weight.
#' @param weight \code{numeric} value of edge weight.
#'
#' @noRd
getWidth <- function (base, fac, weight) { return (base + fac * weight) }

server <- function (input, output, session)
{
  graph <- getGraph (inputGraph)
  short <- getGraph (shortestPath)

  output$map <- leaflet::renderLeaflet ({
    getMap (graph, short)
  })
}
