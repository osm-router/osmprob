
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/osm-router/osmprob.svg?branch=master)](https://travis-ci.org/osm-router/osmprob) [![Build status](https://ci.appveyor.com/api/projects/status/lw5a4udgpjpaf2if?svg=true)](https://ci.appveyor.com/project/karpfen/osmprob) [![codecov](https://codecov.io/gh/osm-router/osmprob/branch/master/graph/badge.svg)](https://codecov.io/gh/osm-router/osmprob) [![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/osmprob)](http://cran.r-project.org/web/packages/osmprob)

osmprob
=======

R package for probabilistic OpenStreetMap routing

------------------------------------------------------------------------

Install
-------

``` r
devtools::install_github ('osm-router/osmprob')
```

Test road graph
---------------

Downloading and preprocessing the graph is done in the function `download_graph ()`. To prepare the graph for routing, all non-accessible edges are removed and vertices not needed to maintain the graph's topology are removed. The resulting data is a `list` containing the original graph, the compact graph and a map of edge IDs that connect the two graphs to each other. A preprocessed sample graph can be acessed with `road_data_sample`. After the preprocessing and routing is done, `plot_map ()` can be used to display the traversal probabilities and shortest path in a `shiny`/`leaflet` web app.

### From sample data

To load, process and display the sample graph included in the package, run

``` r
graph <- road_data_sample
start_pt <- c (11.603, 48.163)
end_pt <- c (11.608, 48.167)
pts <- select_vertices_by_coordinates (graph, start_pt, end_pt)
route_start <- pts[1]
route_end <- pts [2]
path <- get_shortest_path (graph, route_start, route_end)
prob <- get_probability (graph, route_start, route_end, eta = 0.6)
plot_map (prob, path)
```

### From OSM data

``` r
devtools::load_all (export_all = FALSE)
start_pt <- c (11.603, 48.163)
end_pt <- c (11.608, 48.167)
graph <- download_graph (start_pt, end_pt)
pts <- select_vertices_by_coordinates (graph, start_pt, end_pt)
route_start <- pts[1]
route_end <- pts [2]
path <- get_shortest_path (graph, route_start, route_end)
prob <- get_probability (graph, route_start, route_end, eta = 0.6)
plot_map (prob, path)
```

Code of Conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/osm-router/osmprob/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.
