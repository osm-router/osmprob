#include <Rcpp.h>

#include "boost/graph/adjacency_list.hpp"

struct osm_vertex_t
{
    typedef boost::vertex_property_tag osm_vertex;
};


// [[Rcpp::export]]
Rcpp::DataFrame makeCompactGraph (Rcpp::DataFrame graph)
{
    Rcpp::NumericVector from = graph[0];
    Rcpp::NumericVector to = graph[1];
    int l = from.length ();

    // assign vertex ids
    typedef std::map<long long, long long> VertexIdMap;
    VertexIdMap vertexIds;
    long long vertexId = 0;
    for (int i = 0; i < l; i++)
    {
        long long osm = from [i];
        if (vertexIds.count (osm) == 0)
            vertexIds [osm] = vertexId++;
        osm = to [i];
        if (vertexIds.count (osm) == 0)
            vertexIds [osm] = vertexId++;
    }

    // define boost vertex pairs
    typedef std::pair<long long, long long> Street;
    Street streets [l];
    for (int i = 0; i < l; i++)
        streets [i] = Street (from [i], to [i]);

    // define road graph type
    typedef boost::property<osm_vertex_t, long long> OsmIdProperty;
    typedef boost::adjacency_list<boost::vecS, boost::vecS,
            boost::bidirectionalS, OsmIdProperty> bGraph;

    // make boost graph and add edges
    bGraph roads;

    //boost::property_map<bGraph, osm_vertex_t>::type
    //    osm_vertex = boost::get (osm_vertex_t (), roads);

    for (int i = 0; i < l; i++)
    {
        long long idStart = vertexIds.at (streets [i].first);
        long long idEnd = vertexIds.at (streets [i].second);
        boost::add_edge (idStart, idEnd, roads);
    //    boost::put (osmID, idStart, streets [i].first);
    //    boost::put (osmID, idEnd, streets [i].second);
    //    osm_vertex[streets [i].first] = idStart;   
    //    osm_vertex[streets [i].second] = idEnd;
    }



    // PROCESS GRAPH


    // make DataFrame from boost graph
    boost::graph_traits<boost::adjacency_list<> >::vertex_iterator vi, vi_end, next;
    boost::tie (vi, vi_end) = boost::vertices (roads);
    Rcpp::NumericVector vecOut (*vi_end);

    for (next = vi; vi != vi_end; vi = next)
    {
        vecOut (*vi) = *vi;
        next++;
    }

    return Rcpp::DataFrame::create (Rcpp::_["vertex"] = vecOut);
}
