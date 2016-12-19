library (igraph)
library (microbenchmark)
#library (osmdata)
devtools::load_all ("../osmdata", export_all=FALSE)
library (sp)
library (devtools)
library (Rcpp)

#compile
compile <- function ()
{

    #setwd ("~/master_thesis/osmprob/")
    Rcpp::compileAttributes (".", verbose=TRUE)

    #test
    #setwd ("~/master_thesis/")

    #devtools::document ('.')
    devtools::load_all (".", export_all=FALSE)
    devtools::load_all (".")
    #devtools::check (".")

    # These unlink commands shouldn't be needed
    #unlink ("./osmprob/src/*.gcda")
    #unlink ("./osmprob/src/*.gcno")
    #unlink ("./osmprob/src/Makevars")

    #ls ("package:osmprob")
}

dfFromOsmdata <- function (bbox=c (-0.15, 51.5, -0.10, 51.6))
{
#    bbox <- c (-0.11, 51.51, -0.10, 51.52)
    q0 <- opq (bbox=bbox)
    q1 <- add_feature (q0, key='highway', value='primary')
    roads <- osmdata_sp (q1)
    dat <- roads$osm_lines

    oneWay <- slot (dat, "data")$oneway
    oneWay <- tolower (oneWay) == "true"

    dat <- as.SpatialLines.SLDF (dat)
    toID <- c ()
    fromID <- c ()
    cost <- c ()

    for (ln in 1:length (dat))
    {
        datln <- dat[ln]
        lineSlot <- datln@lines
        llines <- lineSlot[[1]]@Lines
        coords <- llines[[1]]@coords
        wayCost <- getCost (coords)
        cost <- c (cost, wayCost)
        vert <- attr (coords, "dimnames")[[1]]
        vBeg <- as.numeric (vert[1])
        vEnd <- as.numeric (vert[length (vert)])
        fromID <- c (fromID, vEnd)
        toID <- c (toID, vBeg)
    }
    toFromDF <- data.frame (toID, fromID, cost, oneWay)
    return (toFromDF)
}

getCost <- function (coordMatrix)
{
    l <- 0
    lastPoint <- c (coordMatrix [1, 1], coordMatrix [1, 2])
    if (dim (coordMatrix) [1] >= 2)
    {
        for (i in 2:dim (coordMatrix) [1])
        {
            diffLon = abs (lastPoint [1] - coordMatrix [i, 1])
            diffLat = abs (lastPoint [2] - coordMatrix [i, 2])
            l <- l + sqrt (diffLon ^ 2 + diffLat ^ 2)
            lastPoint [1] = coordMatrix [i, 1]
            lastPoint [2] = coordMatrix [i, 2]
        }
    }
    return (l)
}

makeCompactGraphProto <- function (graph.in, verbose=FALSE)
{
    vert <- V (graph.in)
    degrees <- degree (graph.in)
    newEdges <- c ()
    verticesToDelete <- c ()
    for (d in 1:length (degrees))
    {
        if (verbose)
            cat ("Making compact graph ", d, "/", length (degrees), "\r")
        
        deg <- degrees[d]
        if (deg == 2)
        {
            addNew <- TRUE
            n <- neighbors (graph.in, vert[d], mode="total")
            n1 <- n[1]
            n2 <- n[2]
            degN1 <- degrees[n1]
            degN2 <- degrees[n2]
            verticesToDelete <- c (verticesToDelete, vert[d])
            n1prev <- vert[d]
            n2prev <- vert[d]
            while (degN1 == 2)
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
            }
            while (degN2 == 2)
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
    if (verbose)
        cat ("\rMaking compact graph done.\n")
    return (graph.in)
}

showGraph <- function (gr)
{
    plot.igraph (gr, vertex.size=3, vertex.color="red", edge.width=2,
          edge.color="black")
}
#igr <- igraph::graph_from_data_frame (gr, directed=FALSE)
#showGraph (igr)
bbox=c (-0.15, 51.5, -0.14, 51.6)
gr <- dfFromOsmdata (bbox=bbox)
#gr <- dfFromOsmdata ()

compile ()
ccc  <- makeCompactGraph (gr)
#microbenchmark::microbenchmark (makeCompactGraph (gr))
