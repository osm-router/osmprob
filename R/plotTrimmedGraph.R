#' Plot the graph network as a Shiny Leaflet app in a browser.
#'
#' @param osmFile A \code{data.frame} containing the street graph to be
#' displayed
#'
#' @export
plotGraph <- function (osmFile="../tests/compact-ways-munich.Rda")
{
    # osmFile can't be passed as a parameter, so it is passed to the server
    # function via an environment variable
    inputGraph <<- osmFile
    shiny::shinyApp(ui, server)
}

inputGraph <- ""

getGraph <- function (fName)
{
    if (is (fName, "data.frame"))
        dat <- fName
    else
        dat <- readRDS (file = fName)
    dat$from_lat <- sapply (dat$from_lat, function (x) as.numeric (as.character (x)))
    dat$from_lon <- sapply (dat$from_lon, function (x) as.numeric (as.character (x)))
    dat$to_lat <- sapply (dat$to_lat, function (x) as.numeric (as.character (x)))
    dat$to_lon <- sapply (dat$to_lon, function (x) as.numeric (as.character (x)))

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

ui <- shiny::bootstrapPage (
  shiny::tags$style (type = "text/css", "html, body {width:100%;height:100%}
              .checkbox, .control-label{color: #FFFFFF}"),
  leaflet::leafletOutput ("map", width = "100%", height = "100%"),
  shiny::absolutePanel (top = 10, right = 10,
  shiny::selectInput ("colors", "Color Scheme", selected = 'Paired',
  rownames (subset (RColorBrewer::brewer.pal.info, colorblind == TRUE))
  )
))

server <- function (input, output, session)
{
  graph <- getGraph (inputGraph)

  colorpal <- shiny::reactive ({
    leaflet::colorNumeric (input$colors, graph$trimmed)
  })

  output$map <- leaflet::renderLeaflet ({
    bb <- as.vector (sf::st_bbox (graph))
    leaflet::leaflet (graph, options = leaflet::leafletOptions (maxZoom = 30)) %>%
    leaflet::addProviderTiles ('CartoDB.DarkMatter') %>%
    leaflet::fitBounds (bb[1], bb[2], bb[3], bb[4]) %>%
    leaflet::addPolylines (color = "#3399FF", opacity = 0.2, weight = 3)
  })

 lnColor <- shiny::reactive ({
   leaflet::colorNumeric (input$colors, graph$d)
 })

  shiny::observe ({
    pal <- lnColor ()
    dat <- graph$d
    leaflet::leafletProxy ("map", data = graph) %>%
    leaflet::clearControls () %>%
    leaflet::addLegend (position = "bottomright", pal = pal, values = dat)
  })

shiny::observe ({
  dat <- graph
  if (!is.null (dat))
  {
    pal <- lnColor ()
    leaflet::leafletProxy ("map", data = dat) %>%
    leaflet::clearShapes () %>%
    leaflet::addPolylines (color = ~pal (dat$d), opacity = 0.6, weight = 3)
  } else
  {
    leafletProxy ("map", data = dat) %>%
    clearShapes ()
  }
 })
}
