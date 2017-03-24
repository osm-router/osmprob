test_that ("makeCompactGraph", {
               dat <- sf::st_read ("../osm-ways-munich.osm", layer="lines",
                                   quiet=TRUE)
               nw <- osmprob::osmlines_as_network (dat)
               comp <- makeCompactGraph (nw)
               isDf <- is (comp, "data.frame")
               testthat::expect_true (isDf)
})
