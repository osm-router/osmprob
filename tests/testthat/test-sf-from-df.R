test_that ("getGraph", {
               dat <- readRDS ("../compact-ways-munich.Rda")
               graph <- getGraph (dat)
               isSf <- is (graph, "sf")
               testthat::expect_true (isSf)
})
