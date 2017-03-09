#' Plot the graph network as a Shiny Leaflet app in a browser.
#'
#' @export
plotGraph <- function ()
{
    shinyApp(ui, server)
}

getGraph <- function (fName)
{
    datRaw <- readRDS (fName)
    datRaw$from_lat <- sapply (datRaw$from_lat, function (x) as.numeric (as.character (x)))
    datRaw$from_lon <- sapply (datRaw$from_lon, function (x) as.numeric (as.character (x)))
    datRaw$to_lat <- sapply (datRaw$to_lat, function (x) as.numeric (as.character (x)))
    datRaw$to_lon <- sapply (datRaw$to_lon, function (x) as.numeric (as.character (x)))

    from <- cbind (datRaw$from_lon, datRaw$from_lat)
    to <- cbind (datRaw$to_lon, datRaw$to_lat)
    fromTo <- cbind (from, to)
    graphLines <- list ("LINESTRING", dim (fromTo)[1])
    for (i in 1:dim (fromTo)[1])
    {
        pair <- fromTo [i,]
        graphLines [[i]] <- st_linestring (rbind (c (pair[1], pair[2]), c (pair[3], pair[4])))
    }

    graph <- st_sfc (graphLines, crs = 4326)
    datRaw$from_lat <- NULL
    datRaw$from_lon <- NULL
    datRaw$to_lat <- NULL
    datRaw$to_lon <- NULL
    graph <- st_sf (graph, datRaw)
    graph
}

ui <- bootstrapPage(
  tags$style (type = "text/css", "html, body {width:100%;height:100%}
              .checkbox, .control-label{color: #FFFFFF}"),
  leafletOutput ("map", width = "100%", height = "100%"),
  absolutePanel (top = 10, right = 10,
    checkboxInput ("trimmed", "Trimmed", FALSE),
    checkboxInput ("rest", "Remaining", FALSE),
    selectInput("colors", "Color Scheme", selected = 'Paired',
    rownames(subset(brewer.pal.info, colorblind == TRUE))
  )
))

server <- function(input, output, session) {

#  graph <- head (getGraph ("../tests/sample_graph_raw.Rda"), 100)
  graph <- getGraph ("../tests/sample_graph_raw.Rda")
  #TODO: replace this with the path to the actual data once it works.

  getCol <- function (trimmed)
  {
      keep <- "#FF0000"
      trim <- "#00FF00"
      sapply (trimmed, function (x) if (x) trim else keep)
  }

  colorpal <- reactive({
    dat <- filtered ()
    colorNumeric(input$colors, graph$trimmed)
  })

 filtered <- reactive ({
     if (!input$trimmed && !input$rest)
         return ()
     graph [graph$trimmed == input$trimmed | graph$trimmed != input$rest,]
 })

  output$map <- renderLeaflet({
    bb <- as.vector (st_bbox (graph))
    leaflet (graph, options = leafletOptions (maxZoom = 30)) %>%
    addProviderTiles (providers$CartoDB.DarkMatter) %>%
    fitBounds(bb[1], bb[2], bb[3], bb[4])
  })

 observe ({
     dat <- filtered ()
     if (!is.null (dat))
     {
         pal <- colorpal ()
         leafletProxy ("map", data = dat) %>%
         clearShapes () %>%
         addPolylines (color=pal (dat$trimmed), opacity = 1, weight = 3,
         popup = paste0 ("<b>From: </b>", graph$from,
                        "<br /><b>To: </b>", graph$to,
                        "<br /><b>Oneway: </b/>", graph$isOneway))
     } else
     {
         leafletProxy ("map", data = dat) %>%
         clearShapes ()
     }
 })
}
