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
    testthat::expect_error (
    osm_router ("not a data.frame", 0, 5, eta = 1.0),
    "netdf must be a data.frame")
    testthat::expect_error (
    osm_router (matrix (1, 1), 0, 5, eta = 1.0),
    "netdf must have 3 columns")
})

test_that ("getProbability", {
   netdf <- data.frame (
                        'from_id' = c (rep (0, 3), rep (1, 3), rep (2, 4),
                                   rep (3, 3), rep (4, 2), rep (5, 3)),
                        'to_id' = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
                                   1, 2, 4, 3, 5, 0, 2, 4),
                        'd_weighted' = c (7., 9., 14., 7., 10., 15., 9., 10.,
                                          11., 2., 15., 11., 6., 6., 9., 14.,
                                          2., 9.))
    way <- getProbability (netdf, 0, 5, eta = 1.0)
    testthat::expect_is (way, "data.frame")
    testthat::expect_error (
    getProbability ("not a data.frame", 0, 5, eta = 1.0),
    "netdf must be a data.frame")
    testthat::expect_error (
    getProbability (data.frame (), 0, 5, eta = 1.0),
    "netdf must contain columns from_id, to_id and d_weighted")
    testthat::expect_error (
    getProbability (netdf, 9, 5),
    "start_node is not part of netdf")
    testthat::expect_error (
    getProbability (netdf, 1, 9),
    "end_node is not part of netdf")
})

test_that ("getShortestPath", {
   netdf <- data.frame (
                        'from_id' = c (rep (0, 3), rep (1, 3), rep (2, 4),
                                   rep (3, 3), rep (4, 2), rep (5, 3)),
                        'to_id' = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
                                   1, 2, 4, 3, 5, 0, 2, 4),
                        'd_weighted' = c (7., 9., 14., 7., 10., 15., 9., 10.,
                                          11., 2., 15., 11., 6., 6., 9., 14.,
                                          2., 9.))
    way <- getShortestPath (netdf, 1, 3)
    testthat::expect_is (way, "numeric")
    testthat::expect_error (
    getShortestPath ("not a data.frame", 0, 5),
    "netdf must be a data.frame")
    testthat::expect_error (
    getShortestPath (data.frame (), 0, 5),
    "netdf must contain columns from_id, to_id and d_weighted")
    testthat::expect_error (
    getShortestPath (netdf, 9, 5),
    "start_node is not part of netdf")
    testthat::expect_error (
    getShortestPath (netdf, 1, 9),
    "end_node is not part of netdf")
})
