test_that ("get_probability", {
    graph <- road_data_sample
    start_pt <- c (11.603, 48.163)
    end_pt <- c (11.608, 48.167)
    pts <- select_vertices_by_coordinates (graph, start_pt, end_pt)
    route_start <- pts[1]
    route_end <- pts [2]
    way <- get_probability (graph, route_start, route_end, eta = 1)

    testthat::expect_is (way$probability, "data.frame")
    testthat::expect_error (
       get_probability ("wrong data format", 0, 5, eta = 1.0),
       "graphs must contain data.frames compact, original and map.")
})

test_that ("get_shortest_path", {
    graph <- road_data_sample
    start_pt <- c (11.603, 48.163)
    end_pt <- c (11.608, 48.167)
    pts <- select_vertices_by_coordinates (graph, start_pt, end_pt)
    route_start <- pts[1]
    route_end <- pts [2]
    way <- get_shortest_path (graph, route_start, route_end)
    testthat::expect_is (way$shortest, "data.frame")
    testthat::expect_error (
        get_shortest_path (graph, -1, route_end),
        "start_node is not part of netdf")
    testthat::expect_error (
        get_shortest_path (graph, route_start, -1),
        "end_node is not part of netdf")
})
