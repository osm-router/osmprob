#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <map>

struct osm_vertex_t
{
    std::vector <osm_vertex_t*> in, out;
    long long osm_id;
    int id;

    int getDegreeIn () { return in.size (); }
    int getDegreeOut () { return out.size (); }
};

struct osm_edge_t
{
    private:
        osm_vertex_t from, to;

    public:
        double weight;
        osm_vertex_t getFromVertex () { return from; }
        osm_vertex_t getToVertex () { return to; }

    osm_edge_t (osm_vertex_t from, osm_vertex_t to)
    {
        this -> from = from;
        this -> to = to;
        from.out.push_back (&to);
        to.in.push_back (&from);
        std::cout << "new edge. ids: " <<
            from.id << " - " <<
            to.id << ", degIn: " <<
            from.getDegreeIn () << ", " << to.getDegreeIn () << " degOut: " <<
            from.getDegreeOut () << ", " << to.getDegreeOut () <<
            std::endl;
    }
};

// [[Rcpp::export]]
Rcpp::DataFrame makeCompactGraph (Rcpp::DataFrame graph)
{
    Rcpp::NumericVector from = graph [0];
    Rcpp::NumericVector to = graph [1];
    Rcpp::NumericVector weight = graph [2];
    Rcpp::LogicalVector isOneway = graph [3];

    int l = from.length ();
    int id = 0;
    std::map <long long, int> idPairs;
    for (int i = 0; i < l; i++)
    {
        if (idPairs.find (from [i]) == idPairs.end ())
        {
            idPairs [from [i]] = id ++;
        }
        if (idPairs.find (to [i]) == idPairs.end ())
        {
            idPairs [to [i]] = id ++;
        }
    }

    std::vector <osm_edge_t> edges;
    std::map <int, osm_vertex_t> allVertexMap;
    for (int i = 0; i < l; i ++)
    {
        int fromId = idPairs.at (from [i]);
        int toId = idPairs.at (to [i]);
        bool insertvFrom = false;
        bool insertvTo = false;

        osm_vertex_t vFrom, vTo;
        if (allVertexMap.find (fromId) != allVertexMap.end ())
            vFrom = allVertexMap.at (fromId);
        else
        {
            vFrom = osm_vertex_t ();
            insertvFrom = true;
        }
        if (allVertexMap.find (toId) != allVertexMap.end ())
            vTo = allVertexMap.at (toId);
        else
        {
            vTo = osm_vertex_t ();
            insertvTo = true;
        }

        /*
        std::cout << "current id - from: " << fromId <<
            " to " << toId <<
            " vFrom " << &vFrom <<
            ", vTo " << &vTo <<
            std::endl;
        */

        vFrom.osm_id = from [i];
        vFrom.id = fromId;
        vTo.osm_id = to [i];
        vTo.id = toId;
        osm_edge_t edge = osm_edge_t (vFrom, vTo);
        edge.weight = weight [i];
        edges.push_back (edge);
        if (!isOneway [i])
        {
            edge = osm_edge_t (vTo, vFrom);
            edge.weight = weight [i];
            edges.push_back (edge);
        }
        if (insertvFrom)
            allVertexMap.insert (std::make_pair (fromId, vFrom));
        if (insertvTo)
            allVertexMap.insert (std::make_pair (toId, vTo));
    }

    /*
    int c = 0;
    std::vector <osm_edge_t>::iterator itEdges = edges.begin ();
    for (; itEdges != edges.end (); itEdges ++)
    {
        osm_edge_t e = *itEdges;
        std::cout << c++ <<
            " FROM - id: " << e.getFromVertex ().id <<
            " deg in: " << e.getFromVertex ().getDegreeIn () <<
            " - deg out: " << e.getFromVertex ().getDegreeOut () <<
            " TO - id: " << e.getToVertex ().id <<
            " deg in: " << e.getToVertex ().getDegreeIn () <<
            " - deg out: " << e.getToVertex ().getDegreeOut () <<
            std::endl;
    }
    */

    /*
    std::map <int, osm_vertex_t>::iterator i = allVertexMap.begin ();
    for (; i != allVertexMap.end (); i ++)
    {
        int key = i -> first;
        osm_vertex_t val = i -> second;
        std::cout << "key: " << key <<
            " id: " << val.id <<
            " deg in: " << val.getDegreeIn () <<
            " deg out: " << val.getDegreeOut () <<
            std::endl;
    }
    */

    //return Rcpp::DataFrame::create (Rcpp::_("from") = f, Rcpp::_("to") = t);
    return NULL;
}
