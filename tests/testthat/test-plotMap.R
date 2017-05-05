test_that ("popup", {
    dat <- popup (1, 2, 3, 4)
    testthat::expect_is (dat, "character")
})

test_that ("getWidth", {
    dat <- getWidth (1, 2, 3)
    testthat::expect_equal (dat, 7)
})

# test_that ("getMap", {
#     dat <- readRDS ("../compact-ways-munich.Rda") %>% head (10) %>%
#         makeCompactGraph %>% magrittr::extract2 (1) %>% getGraph
#     prb <- getProbability (dat, dat$from_id [1], dat$to_id [1])
#     map <- getMap (prb, prb)
#     testthat::expect_s3_class (map, c ("leaflet", "htmlwidget"))
# })

# test_that ("pathsequenceToDataframe", {
#     dat <- readRDS ("../compact-ways-munich.Rda") %>% head (10) %>%
#         makeCompactGraph %>% magrittr::extract2 (1)
#     prb <- getProbability (dat, dat$from_id [1], dat$to_id [4])
#     short <- getShortestPath (dat, dat$from_id [1], dat$to_id [4])
#     pathDf <- pathsequenceToDataframe (prb, short)
#     testthat::expect_is (pathDf, "data.frame")
# })
