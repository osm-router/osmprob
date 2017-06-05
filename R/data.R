#' Weighting profiles
#'
#' Collection of weighting profiles used to adjust the routing process to
#' different means of transport. Original data taken from the Routino project.
#'
#' @format \code{data.frame} with profile names, means of transport and
#' weights.
#' @references \url{https://www.routino.org/xml/routino-profiles.xml}
"weighting_profiles"

#' Sample road data
#'
#' Sample data set of road data in Munich, Germany that has been downloaded and
#' preprocessed, so it can be used for routing.
#'
#' @format \code{list} with three \code{data.frame}. One containing the original
#' graph, one the compact graph and one the pairs of edge IDs needed to map the
#' two graphs to one another.
#' @references \url{http://www.openstreetmap.org/}
"road_data_sample"
