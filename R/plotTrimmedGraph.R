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

charToNum <- function (x) { as.numeric (as.character (x)) }

getGraph <- function (fName)
{
    if (is (fName, "data.frame"))
        dat <- fName
    else
        dat <- readRDS (file = fName)
    dat$from_lat <- vapply (dat$from_lat, charToNum, 0.)
    dat$from_lon <- vapply (dat$from_lon, charToNum, 0.)
    dat$to_lat <- vapply (dat$to_lat, charToNum, 0.)
    dat$to_lon <- vapply (dat$to_lon, charToNum, 0.)

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

    # add mock probability values for testing
    graph$probability <- (sample.int (101,size=dim (graph) [1],
                                      replace=TRUE) -1) / 100
    graph
}

cRamp <- subset (RColorBrewer::brewer.pal.info, category == "qual")
ui <- shiny::bootstrapPage (
  shiny::tags$style (type = "text/css", "html, body {width:100%;height:100%}
              .checkbox, .control-label{color: #FFFFFF}"),
  leaflet::leafletOutput ("map", width = "100%", height = "100%"),
  shiny::absolutePanel (top = 10, right = 10,
  shiny::selectInput ("colors", "Color Scheme", selected = rownames (cRamp) [1],
  rownames (cRamp)
  )
))

server <- function (input, output, session)
{
  graph <- getGraph (inputGraph)

  filtered <- shiny::reactive ({
      n <- 1000
      bounds <- input$map_bounds
      if (is.null (bounds))
          bbx <- sf::st_bbox (graph)
      else
      {
          xmin <- min (bounds$east, bounds$west)
          xmax <- max (bounds$east, bounds$west)
          bbx <- matrix (c (xmin, bounds$south, xmax, bounds$north))
      }
      pol <- matrix (c (bbx [1], bbx [2],
      bbx [3], bbx [2],
      bbx [3], bbx [4],
      bbx [1], bbx [4],
      bbx [1], bbx [2]),
      ncol = 2, byrow = TRUE)
      bbx <- sf::st_polygon (list (pol))
      int <- sf::st_intersects (graph, bbx) == 1
      g <- graph [which (int), ]
      len <- dim (g) [1]
      if (n >= len || len < 1)
          return (graph)
      prt <- len - n
      idxHighest <- which (g$d_weighted > sort (g$d_weighted,
                                                partial = prt) [prt])
      return (g [idxHighest, ])
  })

  output$map <- leaflet::renderLeaflet ({
    dat <- graph
    bb <- as.vector (sf::st_bbox (dat))
    leaflet::leaflet (data = dat,
                      options = leaflet::leafletOptions (maxZoom = 30)) %>%
    leaflet::addProviderTiles ('CartoDB.DarkMatter') %>%
    leaflet::fitBounds (bb[1], bb[2], bb[3], bb[4])
  })

lnColor <- function (x) { leaflet::colorFactor (x, graph$highway) }

  shiny::observe ({
    pal <- lnColor (input$colors)
    dat <- graph$highway
    leaflet::leafletProxy ("map", data = graph) %>%
    leaflet::clearControls () %>%
    leaflet::addLegend (position = "bottomright", pal = pal, values = dat)
  })

getWidth <- function (base, fac, weight) { return (base + fac * weight) }

shiny::observe ({
  dat <- graph
  if (!is.null (dat))
  {
    pal <- lnColor (input$colors)
    leaflet::leafletProxy ("map", data = dat) %>%
    leaflet::clearShapes () %>%
    leaflet::addPolylines (color = ~pal (dat$highway), opacity = 1.0,
                           weight = getWidth (1, 10, dat$probability))
    #leaflet::addPolylines (color = "#FFFFFF", opacity = 1.0,
    #                       weight = getWidth (1, 20, dat$d))
  } else
  {
    leafletProxy ("map", data = dat) %>%
    clearShapes ()
  }
 })
}
