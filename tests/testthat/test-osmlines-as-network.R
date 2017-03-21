test_that ("osmlines_as_network", {
               fname <- "../osm-ways.osm"
               dat <- sf::read_sf (fname, layer = "lines", quiet = FALSE)
               graph <- osmlines_as_network (dat)
               isDf <- is (graph, "data.frame")
               expect_true (isDf)
})
