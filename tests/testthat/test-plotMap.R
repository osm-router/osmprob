test_that ("popup", {
    dat <- popup (1, 2, 3, 4)
    testthat::expect_is (dat, "character")
})

test_that ("getWidth", {
    dat <- getWidth (1, 2, 3)
    testthat::expect_equal (dat, 7)
})

test_that ("getMap", {
               dat <- readRDS ("../compact-ways-munich.rds") %>% head (10)
               st <- dat$compact$from_id [1]
               en <- dat$compact$to_id [5]
               prb <- get_probability (dat, st, en) %>% get_graph
               testthat::expect_error (get_map (prb, NA))
               testthat::expect_error (get_map (NA, prb))
})
