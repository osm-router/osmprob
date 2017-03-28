test_that ("osm_router", {
    netdf <- cbind (
         'xfr' = c (rep (0, 3), rep (1, 3), rep (2, 4),
                  rep (3, 3), rep (4, 2), rep (5, 3)),
         'xto' = c (1, 2, 5, 0, 2, 3, 0, 1, 3, 5,
                  1, 2, 4, 3, 5, 0, 2, 4),
         'd' = c (7., 9., 14., 7., 10., 15., 9., 10., 11., 2.,
                15., 11., 6., 6., 9., 14., 2., 9.))
    way <- osm_router (netdf, 0, 5, eta = 1.0)
    #isMat <- is (way, "matrix")
    #testthat::expect_true (isMat)
})
