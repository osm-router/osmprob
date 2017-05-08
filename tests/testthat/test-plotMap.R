test_that ("popup", {
    dat <- popup (1, 2, 3, 4)
    testthat::expect_is (dat, "character")
})

test_that ("getWidth", {
    dat <- getWidth (1, 2, 3)
    testthat::expect_equal (dat, 7)
})

test_that ("getMap", {
               dat <- readRDS ("../compact-ways-munich.Rda") %>% head (10) %>%
                   makeCompactGraph
               st <- dat$compact$from_id [1]
               en <- dat$compact$to_id [3]
               prb <- getProbability (dat, st, en) %>% getGraph
               testthat::expect_error (getMap (prb, prb))
})
