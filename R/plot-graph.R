#' Plot the graph network as a Shiny Leaflet app in a browser.
#'
#' @param graph \code{list} containing the probabilistic routing result of the
#' road graph.
#' @param shortest \code{list} containing the shortest path routing results of
#' the road graph.
#'
#' @examples
#' \dontrun{
#' q <- osmdata::opq (bbox = c (11.58, 48.14, 11.585, 48.145))
#' q <- osmdata::add_feature (q, key = 'highway')
#' x <- osmdata::osmdata_sf(q)
#' net <- osmlines_as_network (x)
#' graph <- make_compact_graph (net)
#' start_pt <- graph$from_id [1]
#' end_pt <- graph$to_id [100]
#' prob <- get_probability (graph, start_pt, end_pt)
#' short <- get_shortest_path (graph, start_pt, end_pt)
#' plot_map (prob, short)
#' }
#'
#' @export
plot_map <- function (graph, shortest)
{
    # graph and shortest_path can't be passed as a parameter, so it is passed to
    # the server function via an environment variable
    input_graph <<- graph [[1]]
    shortest_path <<- shortest [[1]]
    shiny::shinyApp(ui, server)
}

input_graph <- ""
shortest_path <- ""

ln_color <- function (x, color_by) {leaflet::colorQuantile (x, color_by, n = 9)}

ui <- shiny::bootstrapPage (
  shiny::tags$style (type = "text/css", "html, body {width:100%;height:100%}
              .checkbox, .control-label{color: #FFFFFF}"),
  leaflet::leafletOutput ("map", width = "100%", height = "100%")
)

#' Generate text for edge popup fields on the graph
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
         "</br><b>Weight: </b>", format (round (as.numeric (weight), 3), nsmall = 3),
         "</br><b>Probability: </b>", format (round (as.numeric (prob), 3), nsmall = 3))
}

#' Generate \code{leaflet} HTML widget containing a web map
#'
#' @param dat \code{sf} object containing street graph data.
#' @param short \code{sf} object containing the shortest path between two
#' points.
#'
#' @noRd
get_map <- function (dat, short)
{
    dat <- subset (dat, !is.na (dat$prob))
    grp_prb <- "Probabilities"
    grp_srt <- "Shortest Path"
    grp_se <- "Start and end points"
    colorscheme <- "YlGnBu"
    print_weights <- dat$d_weighted / sf::st_length (dat) 
    pal <- ln_color (colorscheme, print_weights)
    start_pt <- sf::st_coordinates (short [1, ])[1, 2:1]
    end_pt <- sf::st_coordinates (utils::tail (short, 1))[1, 2:1]
    bb <- as.vector (sf::st_bbox (dat))

    leaflet::leaflet (data = dat,
                      options = leaflet::leafletOptions ()) %>%
    leaflet::addProviderTiles ('CartoDB.DarkMatter', group = "base") %>%
    leaflet::addPolylines (color = ~pal (print_weights), opacity = 1.0,
                           weight = getWidth (3, 8, dat$dens),
                           popup = popup (dat$from_id, dat$to_id,
                                          dat$d_weighted, dat$dens),
                           group = grp_prb) %>%
    leaflet::addPolylines (data = short, color = "#FF0000", opacity = 1.0,
                           weight = 4, group = grp_srt,
                           dashArray = "20, 20") %>%
    leaflet::addCircleMarkers (stroke = FALSE, start_pt [2], start_pt [1],
                               group = grp_se, color = "#FFFF00",
                               fillOpacity = 1.0, radius = 10) %>%
    leaflet::addCircleMarkers (stroke = FALSE, end_pt [2], end_pt [1],
                               group = grp_se, color = "#0066FF",
                               fillOpacity = 1.0, radius = 10) %>%
    leaflet::addLayersControl (overlayGroups = c (grp_prb, grp_se, grp_srt),
                               options = leaflet::layersControlOptions
                               (collapsed = FALSE)) %>%
    leaflet::addLegend (position = "bottomright", pal = pal,
                        values = print_weights,
                        title = "Edge Weight (Quantile)") %>%
    leaflet::fitBounds (bb[1], bb[2], bb[3], bb[4])
}

#' Calculate displayed line width
#'
#' @param base \code{numeric} value of minimum width.
#' @param fac \code{numeric} value of facor to be multiplied width weight.
#' @param weight \code{numeric} value of edge weight.
#'
#' @noRd
getWidth <- function (base, fac, weight) { return (base + fac * weight) }

server <- function (input, output, session)
{
  graph <- get_graph (input_graph)
  short <- get_graph (shortest_path)

  output$map <- leaflet::renderLeaflet ({
    get_map (graph, short)
  })
}
