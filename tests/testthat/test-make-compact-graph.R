test_that ("make_compact_graph", {
               dat <- sf::st_read ("../osm-ways-munich.osm", layer="lines",
                                   quiet=TRUE)
               nw <- osmprob::osmlines_as_network (dat)
               comp <- make_compact_graph (nw)
               isDf <- is (comp, "list")
               testthat::expect_true (isDf)
               testthat::expect_error (
               make_compact_graph ("not a data.frame"),
               "graph must be of type data.frame")
})
