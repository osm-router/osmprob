#' Plot the graph network as a Shiny Leaflet app in a browser.
#'
#' @param graph \code{data.frame} containing the street graph to be
#' displayed
#' @param shortest \code{vector} containing the shortest path
#'
#' @export
plotMap <- function (graph, shortest)
{
    ways <- cbind (utils::head (shortest, -1), shortest [-1])
    nms <- names (graph)
    shortest <- data.frame (matrix (ncol = length (nms), nrow = dim (ways)[1]))
    names (shortest) <- nms
    for (i in seq_along (ways [,1]))
    {
        way <- ways [i,]
        shortest [i,] <- graph [graph$from_id == way [1] &
                                graph$to_id == way [2],]
    }
    # graph and shortestPath can't be passed as a parameter, so it is passed to
    # the server function via an environment variable
    inputGraph <<- graph
    shortestPath <<- shortest
    shiny::shinyApp(ui, server)
}

inputGraph <- ""
shortestPath <- ""

charToNum <- function (x) { as.numeric (as.character (x)) }

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
    dat$from_lat <- NULL
    dat$from_lon <- NULL
    dat$to_lat <- NULL
    dat$to_lon <- NULL
    graph <- sf::st_sf (graph, dat)

    graph
}

cRamp <- subset (RColorBrewer::brewer.pal.info, category == "qual")
ui <- shiny::bootstrapPage (
  shiny::tags$style (type = "text/css", "html, body {width:100%;height:100%}
              .checkbox, .control-label{color: #FFFFFF}"),
  leaflet::leafletOutput ("map", width = "100%", height = "100%")
)

server <- function (input, output, session)
{
  graph <- getGraph (inputGraph)
  short <- getGraph (shortestPath)
  startPt <- utils::head (shortestPath, 1)  %>%
      magrittr::extract (c ("from_lat", "from_lon")) %>% as.character %>%
      as.numeric
  endPt <- utils::tail (shortestPath, 1)  %>%
      magrittr::extract (c ("to_lat", "to_lon")) %>% as.character %>%
      as.numeric

  output$map <- leaflet::renderLeaflet ({
    dat <- graph
    bb <- as.vector (sf::st_bbox (dat))
    leaflet::leaflet (data = dat,
                      options = leaflet::leafletOptions (maxZoom = 30)) %>%
    leaflet::addProviderTiles ('CartoDB.DarkMatter', group = "base") %>%
    leaflet::addPolylines (color = "#FFFFFF", opacity = 1.0,
                           weight = dat$probability * 7, group = "prob") %>%
    leaflet::addPolylines (data = short, color = "#FF0000", opacity = 1.0,
                           weight = 3, group = "shortest") %>%
    leaflet::addMarkers (startPt [2], startPt [1], group = "startEnd") %>%
    leaflet::addMarkers (endPt [2], endPt [1], group = "startEnd") %>%
    leaflet::addLayersControl (overlayGroups = c ("prob", "shortest",
                                                  "startEnd"),
                               options = leaflet::layersControlOptions
                               (collapsed = FALSE)) %>%
    leaflet::fitBounds (bb[1], bb[2], bb[3], bb[4])
  })

  getWidth <- function (base, fac, weight) { return (base + fac * weight) }
}
