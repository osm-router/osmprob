<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/osm-router/osmprob.svg?branch=master)](https://travis-ci.org/osm-router/osmprob) [![codecov](https://codecov.io/gh/osm-router/osmprob/branch/master/graph/badge.svg)](https://codecov.io/gh/osm-router/osmprob)

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

A sample graph for testing purposes can be found in `tests/sample_graph.Rda`. It can be accessed with `readRDS (file='tests/sample_graph.Rda')`. The graph contains OSM data including node IDs, node coordinates and edge weight in km. It has already been simplified with `makeCompactGraph ()`.
