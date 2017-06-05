test_that ("get_graph", {
    dat <- road_data_sample %>% magrittr::extract2 ("compact")
    graph <- get_graph (dat)
    isSf <- is (graph, "sf")
    testthat::expect_true (isSf)
})
