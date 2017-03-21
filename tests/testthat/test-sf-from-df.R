test_that ("getGraph", {
               fname <- "../sample_graph.Rda"
               graph <- getGraph (fname)
               isSf <- is (graph, "sf")
               testthat::expect_true (isSf)
})
