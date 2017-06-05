test_that ("popup", {
    dat <- popup (1, 2, 3, 4)
    testthat::expect_is (dat, "character")
})

test_that ("get_width", {
    dat <- get_width (1, 2, 3)
    testthat::expect_equal (dat, 7)
})

test_that ("get_map", {
    graph <- road_data_sample
    start_pt <- c (11.603, 48.163)
    end_pt <- c (11.608, 48.167)
    pts <- select_vertices_by_coordinates (graph, start_pt, end_pt)
    route_start <- pts[1]
    route_end <- pts [2]
    prb <- get_probability (graph, route_start, route_end)
    prb <- get_graph (prb$probability)
    testthat::expect_error (get_map (prb, NA))
    testthat::expect_error (get_map (NA, prb))
})
