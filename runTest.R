library(igraph)
library(microbenchmark)

compile ()
ccc  <- makeCompactGraph (gr)

gr <- dfFromOsmdatar()
com <- makeCompactGraphProto (gr, TRUE)
# microbenchmark::microbenchmark (makeCompactGraphProto (gr, TRUE), times=10)
showGraph (gr)
dev.new ()
showGraph (com)

ccc  <- makeCompactGraph (gr)
microbenchmark::microbenchmark (makeCompactGraph (gr))
#microbenchmark::microbenchmark (makeCompactGraphProto (grDf, FALSE), times=3)
head (ccc)
ccc

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
    toFromDF <- data.frame (toID, fromID, FALSE)
    osmGraph <- graph.data.frame (d=data.frame (A=fromID, B=toID), directed=FALSE)

#    return (osmGraph)
    return (toFromDF)
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
