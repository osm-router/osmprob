library(igraph)

#compile
compile <- function ()
{
    library (devtools)
    library (Rcpp)

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
}
# compile ()

testGraph <- function ()
{

    # remove vertices with degree < 2
    
    #?igraph
#    ?make_graph
    gr <- make_graph (c (6, 3, 1, 2, 2, 3, 3, 1, 3, 4, 2, 4, 1, 5, 5, 4, 7, 8, 8, 9), directed = FALSE)
    degree(gr)
    V(gr)
    grc <- delete.vertices (gr, V (gr)[degree(gr) < 3])
    grc
#    plot (gr)
#    plot (grc)

    ?delete_vertices


    grDf <- as_data_frame (gr)
    grDf <- makeCompactGraph (grDf)
    grDf
    grDf <- graph_from_data_frame (grDf)
    return (grDf)

#    plot (grDf)
    #tkplot (gr)
    #rglplot (gr)
}

compile ()
grdf <- testGraph ()

#osmG <- graphFromOsmdatar ()
class (osmG)
typeof (osmG)
str (osmG)


bbox <- matrix (c (-0.11, 51.51, -0.10, 51.52), nrow=2, ncol=2)
dat <- get_lines (bbox=bbox, key='highway', value='primary')
datDf <- dfFromOsmdatar ()

summary (dat)
dat@lines[[1]]

summary (datDf)

compile ()
ccc  <- makeCompactGraph (datDf)

#plot (grdf)

dfFromOsmdatar <- function ()
{
    library (osmdatar)
    library (sp)
    bbox <- matrix (c (-0.11, 51.51, -0.10, 51.52), nrow=2, ncol=2)
    #bbox <- matrix (c (-0.15, 51.5, -0.10, 51.6), nrow=2, ncol=2)
    dat <- get_lines (bbox=bbox, key='highway', value='primary')
    dat <- as.SpatialLines.SLDF (dat)

    toID <- c ()
    fromID <- c ()

    for (ln in 1:length (dat))
    {
        datln <- dat[ln]
        lineSlot <- datln@lines
        llines <- lineSlot[[1]]@Lines
        coords <- llines[[1]]@coords
        vert <- attr (coords, "dimnames")[[1]]
        vBeg <- as.numeric (vert[1])
        vEnd <- as.numeric (vert[length (vert)])
        fromID <- c (fromID, vEnd)
        toID <- c (toID, vBeg)
    }
    toFromDF <- data.frame (toID, fromID)
#    osmGraph <- graph.data.frame (d=data.frame (A=from, B=to), directed=FALSE)

    return (toFromDF)
}

makeCompactGraphProto <- function (graph.in)
{
    vert <- V (graph.in)
    degrees <- degree (graph.in)
    newEdges <- c ()
    verticesToDelete <- c ()
    for (d in 1:length (degrees))
    {
        cat ("Making compact graph ", d, "/", length (degrees), "\r")
        deg <- degrees[d]
        if (deg == 2)
        {
            addNew = TRUE
            n <- neighbors (graph.in, vert[d], mode="total")
            n1 <- n[1]
            n2 <- n[2]
            degN1 <- degrees[n1]
            degN2 <- degrees[n2]
            verticesToDelete <- c (verticesToDelete, vert[d])
            n1prev <- vert[d]
            n2prev <- vert[d]
            while (degN1 < 3)
            {
                nn1 <- neighbors (graph.in, vert[n1], mode="total")
                verticesToDelete <- c (verticesToDelete, n1)
                if (length (nn1) < 2 || nn1[1] == nn1[2])
                {
                    addNew = FALSE
                    break
                }
                n1new <- nn1[1]
                if (n1new == n1prev)
                {
                    n1new <- nn1[2]
                }
                if (n1new == vert[d])
                {
                    addNew = FALSE
                    break
                }
                n1prev <- n1
                n1 <- n1new
                degN1 <- degrees[n1new]
                #cat (d, " - neighbor1: ", n1, " deg: ", degN1, "\n")
            }
            while (degN2 < 3)
            {
                nn2 <- neighbors (graph.in, vert[n2], mode="total")
                verticesToDelete <- c (verticesToDelete, n2)
                if (length (nn2) < 2 || nn2[1] == nn2[2])
                {
                    addNew = FALSE
                    break
                }
                n2new <- nn2[1]
                if (n2new == n2prev)
                {
                    n2new <- nn2[2]
                }

                #avoid rings
                if (n1new == vert[d])
                {
                    addNew = FALSE
                    break
                }

                n2prev <- n2
                n2 <- n2new
                degN2 <- degrees[n2]
                #cat (d, " - neighbor2: ", n2, " deg: ", degN2, "\n")
            }
            if (addNew)
            {
                newEdges <- c (newEdges, c (n1, n2))
            }
        }
    }

    graph.in <- add_edges (graph.in, edges = newEdges)

    verticesToDelete <- verticesToDelete[!verticesToDelete %in% newEdges]
    graph.in <- delete_vertices (graph.in, verticesToDelete)
    graph.in <- simplify (graph.in, remove.multiple = TRUE, remove.loops = TRUE)
    verticesToDelete <- V (graph.in)[degree (graph.in) == 1]
    graph.in <- delete_vertices (graph.in, verticesToDelete)
    cat ("\rMaking compact graph done.\n")
    return (graph.in)
}
#osmGraph <- graphFromOsmdatar ()
#system.time (gr.compact <- makeCompactGraph (osmGraph))



#gr <- make_graph (c (2, 7, 7, 8, 8, 9, 9, 3, 6, 3, 1, 2, 3, 1, 3, 4, 2, 4, 1, 5, 5, 4), directed = FALSE)
#plot (osmGraph)

#plot (gr.compact,
#      vertex.size = 3, vertex.color="red", edge.width=2, edge.color="black")
#dev.new ()
#plot (osmGraph,
#      vertex.size = 3, vertex.color="red", edge.width=2, edge.color="black")
