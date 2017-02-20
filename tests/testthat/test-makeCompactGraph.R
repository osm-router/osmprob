test_that ("makeCompactGraph", {
               inToID <- c (2, 1, 3, 4, 4, 5, 11, 11, 6, 7, 7, 9, 12, 13, 14, 2, 15, 16, 17, 11)
               inFromID <- c (1, 3, 2, 3, 10, 4, 5, 2, 7, 8, 9, 8, 5, 1, 13, 14, 12, 15, 16, 17)
               inCost <- rep (1, length (inToID))
               inOneWay <- c (T, T, F, T, T, T, F, F, T, T, T, F, T, T, T, T, F, F, F, F)
               graph <- data.frame (inToID, inFromID, inCost, inOneWay)
               graph_out <- makeCompactGraph (graph)

               outToID <- c (2, 1, 3, 2, 4, 4, 5, 11, 5, 11, 2, 12, 2, 11, 17, 14, 17, 12)
               outFromID <- c (1, 3, 2, 3, 3, 10, 4, 5, 11, 2, 11, 5, 14, 17, 11, 1, 12, 17)
               outCost <- c (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3)
               graph_test <- data.frame (outToID, outFromID, outCost)
               expect_identical (graph_out, graph_test)
})
