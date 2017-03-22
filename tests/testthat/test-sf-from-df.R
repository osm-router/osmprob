test_that ("getGraph", {
               fname <- "../compact-ways-munich.Rda"
               graph <- getGraph (fname)
               isSf <- is (graph, "sf")
               testthat::expect_true (isSf)
})
