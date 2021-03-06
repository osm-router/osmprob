test_that ("osmlines_as_network", {
               fname <- "../osm-ways-munich.osm"
               dat <- sf::read_sf (fname, layer = "lines", quiet = TRUE)
               graph <- osmlines_as_network (dat)
               isDf <- is (graph, "data.frame")
               testthat::expect_true (isDf)
               datTest <- dat
               datTest$osm_id <- NULL
               testthat::expect_error (osmlines_as_network (datTest))
               datTest <- dat
               datTest$geometry <- NULL
               testthat::expect_error (osmlines_as_network (datTest),
                   "lns must be an 'sf' collection of 'LINESTRING' objects")
               datTest <- dat
               datTest$osm_id <- NULL
               testthat::expect_error (osmlines_as_network (datTest),
                   "sf_lines have no osm_id component")
               datTest <- dat
               rows <- as.numeric (row.names (datTest))
               ow <- vapply (rows, function (x)
                             if (x%%2 == 0) "yes" else "no", "")
               datTest$oneway <- ow
               datTest$oneway.bicycle <- ow
               datTest <- datTest [,c (1:9, 11, 12, 10)]
               graph <- osmlines_as_network (datTest)
               isDf <- is (graph, "data.frame")
               testthat::expect_true (isDf)
})
