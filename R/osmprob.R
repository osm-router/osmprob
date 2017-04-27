#' osmprob
#'
#' @name osmprob
#' @docType package
#' @importFrom Rcpp evalCpp
#' @importFrom leaflet addPolylines addProviderTiles removeShape colorNumeric
#' @importFrom leaflet fitBounds leaflet leafletOptions leafletProxy 
#' @importFrom leaflet leafletOutput renderLeaflet
#' @importFrom shiny absolutePanel bootstrapPage checkboxInput 
#' @importFrom shiny reactive selectInput shinyApp sliderInput
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom stats quantile
#' @importFrom utils head tail
#' @importFrom magrittr extract %>% %<>%
#' @importFrom methods is
#' @importFrom sf st_sf st_sfc st_linestring
#' @useDynLib osmprob
NULL
