#' add_test
#'
#' Test Rcpp
#'
#' @param a A value
#' @param b A value
#'
#' @return a + b
#' @export
add_test <- function (a, b)
{
    res <- addTest (a, b)
    return (res)
}
