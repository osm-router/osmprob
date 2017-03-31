<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/osm-router/osmprob.svg?branch=master)](https://travis-ci.org/osm-router/osmprob) [![codecov](https://codecov.io/gh/osm-router/osmprob/branch/master/graph/badge.svg)](https://codecov.io/gh/osm-router/osmprob) [![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/osmprob)](http://cran.r-project.org/web/packages/osmprob)

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

A sample graph with OSM data can be found in `tests/osm-ways-munich`. It was downloaded and saved with [osmdata](https://github.com/osmdatar/osmdata) like this:

``` r
q <- osmdata::opq (bbox = c (11.58, 48.14, 11.585, 48.145))
q <- osmdata::add_feature (q, key = 'highway')
dat <- osmdata::osmdata_xml (q, "osm-ways-munich.osm")
```

To prepare the graph for routing, all non-accessible edges are removed using `makeCompactGraph ()`, so only the largest connected part of the original graph remains. The resulting graph is stored as a `data.frame`. A sample can be found in `tests/compact-ways-munich.Rda`.

After the preprocessing is done, `plotGraph ()` can be used to display the compact sample graph in a `shiny`/`leaflet` web app.

To download, process and display a complete graph from scratch, run

``` r
devtools::load_all ()
q <- osmdata::opq (bbox = c (11.58, 48.14, 11.585, 48.145))
q <- osmdata::add_feature (q, key = 'highway')
dat <- osmdata::osmdata_sf (q)
graph <- osmlines_as_network (dat)
graph <- makeCompactGraph (graph)
plotGraph (graph)
```
