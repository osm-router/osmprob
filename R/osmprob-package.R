#' Calculates probabilistic routes on a OSM street graph
#'
#' Downloads and preprocesses OpenStreetMap (OSM) data using \code{osmdata} and
#' performs probabilistic routing on the graph. The result is a
#' \code{data.frame}, where each row represents an edge of the graph that has
#' one column containing the local and one column containing the global
#' traversal probability. This data can either be used for further analysis or
#' displayed in a web broser using a built-in \code{shiny}/\code{leaflet}
#' function.
#'
#' @name osmprob
#' @docType package
#' @importFrom Rcpp evalCpp
#' @importFrom igraph distances E make_directed_graph
#' @importFrom leaflet addPolylines addProviderTiles removeShape colorNumeric
#' @importFrom leaflet fitBounds leaflet leafletOptions leafletProxy 
#' @importFrom leaflet leafletOutput renderLeaflet
#' @importFrom Matrix Diagonal rowSums
#' @importFrom methods as
#' @importFrom shiny absolutePanel bootstrapPage checkboxInput 
#' @importFrom shiny reactive selectInput shinyApp sliderInput
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom stats quantile complete.cases
#' @importFrom utils head tail
#' @importFrom magrittr extract %>% %<>%
#' @importFrom methods is
#' @importFrom sf st_sf st_sfc st_linestring
#' @useDynLib osmprob, .registration = TRUE
NULL
