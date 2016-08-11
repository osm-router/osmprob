#library (devtools)
#library (Rcpp)
library(igraph)

#compile
wd <- getwd ()
while (length (grep ('osmprob', getwd ())) > 0) setwd ("..")
#setwd ("~/master_thesis/osmprob/")
#Rcpp::compileAttributes (".", verbose=TRUE)
Rcpp::compileAttributes ("osmprob", verbose=TRUE)

#test
#setwd ("~/master_thesis/")

devtools::document ('osmprob')
#devtools::load_all ("osmprob", export_all=FALSE)
devtools::load_all ("osmprob")
#devtools::check ("osmprob")

unlink ("./osmprob/src/*.gcda")
unlink ("./osmprob/src/*.gcno")
unlink ("./osmprob/src/Makevars")

ls ("package:osmprob")

testGraph <- function ()
{

    # remove vertices with degree < 2
    
    #?igraph
    ?make_graph
    gr <- make_graph (c (6, 3, 1, 2, 2, 3, 3, 1, 3, 4, 2, 4, 1, 5, 5, 4), directed = FALSE)
    degree(gr)
    V(gr)
    grc <- delete.vertices (gr, V (gr)[degree(gr) < 3])
    grc
    plot (gr)
    plot (grc)

    ?delete_vertices


    grDf <- as_data_frame (gr)
    grDf <- makeCompactGraph (grDf)
    grDf
    graph_from_data_frame (grDf)

    plot (gr)
    #tkplot (gr)
    #rglplot (gr)
}

#testGraph ()


makeCompactGraph <- function (graph.in)
{
    vert <- V (graph.in)
    degrees <- degree (graph.in)
    newEdges <- c ()
    verticesToDelete <- c ()
    for (d in 1:length(degrees))
    {
        deg <- degrees[d]
        if (deg == 2)
        {
            n <- neighbors (graph.in, vert[d], mode="total")
            n1 <- n[1]
            n2 <- n[2]
            degN1 <- degrees[n1]
            degN2 <- degrees[n2]
            while (degN1 < 3)
            {
                n <- neighbors (graph.in, vert[n1], mode="total")
                verticesToDelete <- c (verticesToDelete, n1)
                n1 <- n[1]
                degN1 <- degrees[n1]
            }
            while (degN2 < 3)
            {
                n <- neighbors (graph.in, vert[n2], mode="total")
                verticesToDelete <- c (verticesToDelete, n2)
                n2 <- n[1]
                degN2 <- degrees[n2]
            }
            newEdges <- c (newEdges, c (n1, n2))
        }
    }
    graph.in <- delete_vertices (graph.in, verticesToDelete)
    graph.in <- add_edges (graph.in, edges = newEdges)
    plot (graph.in)
}
gr <- make_graph (c (2, 7, 7, 8, 8, 3, 6, 3, 1, 2, 3, 1, 3, 4, 2, 4, 1, 5, 5, 4), directed = FALSE)
plot (gr)
makeCompactGraph (gr)
