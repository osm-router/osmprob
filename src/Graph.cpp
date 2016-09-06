#include <Rcpp.h>

#include "boost/graph/adjacency_list.hpp"

struct OsmVertex
{
    long long osmID;
};

// [[Rcpp::export]]
Rcpp::DataFrame makeCompactGraph (Rcpp::DataFrame graph)
{
    Rcpp::NumericVector from = graph[0];
    Rcpp::NumericVector to = graph[1];
    int l = from.length ();

    // define boost vertex pairs
    typedef std::pair<long long, long long> Street;
    Street streets [l];
    for (int i = 0; i < l; i++)
    {
        streets [i] = Street (from [i], to [i]);
        std::cout << "street from: " << from [i] << " to: " << to [i] << std::endl;
    }

    // define road graph type
    typedef boost::adjacency_list<boost::vecS, boost::vecS,
            boost::bidirectionalS, OsmVertex> bGraph;

    // make boost graph
    bGraph roads;
    
    for (int i = 0; i < l; i++)
        boost::add_edge (streets [i].first, streets [i].second, roads);
    /*
    {
        vertex_t vFrom = boost::add_vertex (bGraph)
        vertex_t vTo = boost::add_vertex (bGraph)
        boost::add_edge (vFrom, vTo, bGraph);
    }
    */
/*
    // make DataFrame from boost graph
    boost::graph_traits<boost::adjacency_list<> >::vertex_iterator vi, vi_end, next;
    boost::tie (vi, vi_end) = boost::vertices (bGraph);
    Rcpp::NumericVector vecOut (*vi_end);

    for (next = vi; vi != vi_end; vi = next)
    {
        vecOut (*vi) = *vi;
        next++;
        std::cout << "boost vertex: " << *vi << " / " << boost::num_vertices (bGraph) << std::endl;
    }

//    return Rcpp::DataFrame::create (Rcpp::_["vertex"] = vecOut);
*/
    return graph;
}
