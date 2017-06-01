test_that ("osm_router", {
   netdf <- data.frame (
        'xfr' = c (rep (0, 3), rep (1, 3), rep (2, 4),
                   rep (3, 3), rep (4, 2), rep (5, 3)),
        'xto' = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
                   1, 2, 4, 3, 5, 0, 2, 4),
        'd' = c (7., 9., 14., 7., 10., 15., 9., 10., 11., 2.,
                 15., 11., 6., 6., 9., 14., 2., 9.))
    way <- osm_router (netdf, 0, 5, eta = 1.0)
    testthat::expect_is (way, "matrix")
    way <- osm_router (as.matrix (netdf), 0, 5, eta = 1.0)
    testthat::expect_is (way, "matrix")
})

test_that ("get_probability", {
    graph <- readRDS ("../compact-ways-munich.rds")
    start_pt <- c (11.58050,48.13986)
    end_pt <- c (11.58076, 48.14408)
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
    graph <- readRDS ("../compact-ways-munich.rds")
    start_pt <- c (11.58050,48.13986)
    end_pt <- c (11.58076, 48.14408)
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
