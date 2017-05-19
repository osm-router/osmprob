test_that ("getGraph", {
               dat <- readRDS ("../compact-ways-munich.rds") %>%
                   magrittr::extract2 ("compact")
               graph <- get_graph (dat)
               isSf <- is (graph, "sf")
               testthat::expect_true (isSf)
})
