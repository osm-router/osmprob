#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <map>

typedef long long osm_id_t;

typedef int edge_id_t;

struct osm_vertex_t
{
    private:
        std::vector <osm_id_t> in, out;

    public:
        void addNeighborIn (osm_id_t osm_id) {in.push_back (osm_id); }
        void addNeighborOut (osm_id_t osm_id) {out.push_back (osm_id); }
        int getDegreeIn () { return in.size (); }
        int getDegreeOut () { return out.size (); }
        std::vector <osm_id_t> getNeighborsIn () {return in; }
        std::vector <osm_id_t> getNeighborsOut () {return out; }
        const std::vector <osm_id_t> getAllNeighbors ()
        {
            std::vector <osm_id_t> allNeighbors = in;
            allNeighbors.insert (allNeighbors.end (), out.begin (), out.end ());
            return allNeighbors;
        }
};

struct osm_edge_t
{
    private:
        osm_id_t from, to;

    public:
        double weight;
        edge_id_t id;
        osm_id_t getFromVertex () { return from; }
        osm_id_t getToVertex () { return to; }

        osm_edge_t (osm_id_t fromId, osm_id_t toId, double weight,
                edge_id_t edgeId)
        {
            this -> id = edgeId;
            this -> to = toId;
            this -> from = fromId;
            this -> weight = weight;
        }
};

// [[Rcpp::export]]
Rcpp::DataFrame makeCompactGraph (Rcpp::DataFrame graph)
{
    Rcpp::NumericVector to = graph [0];
    Rcpp::NumericVector from = graph [1];
    Rcpp::NumericVector weight = graph [2];
    Rcpp::LogicalVector isOneway = graph [3];

    std::map <osm_id_t, osm_vertex_t> vertices;
    std::vector <osm_edge_t> edges;
    std::map <osm_id_t, osm_vertex_t>::iterator it;
    int edge_id = 0;
    for (int i = 0; i < to.length (); i ++)
    {
        osm_id_t fromId = from [i];
        osm_id_t toId = to [i];

        if (vertices.find (fromId) == vertices.end ())
            vertices.insert (std::make_pair (fromId, osm_vertex_t ()));
        it = vertices.find (fromId);
        osm_vertex_t fromVtx = it -> second;
        fromVtx.addNeighborOut (toId);
        if (!isOneway [i])
            fromVtx.addNeighborIn (toId);
        vertices [fromId] = fromVtx;

        if (vertices.find (toId) == vertices.end ())
            vertices.insert (std::make_pair (toId, osm_vertex_t ()));
        it = vertices.find (toId);
        osm_vertex_t toVtx = it -> second;
        toVtx.addNeighborIn (fromId);
        if (!isOneway [i])
            toVtx.addNeighborOut (toId);
        vertices [fromId] = toVtx;

        osm_edge_t edge = osm_edge_t (fromId, toId, weight [i], edge_id ++);
        edges.push_back (edge);
        if (!isOneway [i])
        {
            edge = osm_edge_t (toId, fromId, weight [i], edge_id ++);
            edges.push_back (edge);
        }
    }

    // identify largest graph component
    std::map<osm_id_t, int> components;
    int component_number = 0;

    for (auto ii = vertices.begin (); ii != vertices.end(); ++ ii)
    {
        if (components.find (ii -> first) == components.end ())
        {
            components.insert (std::make_pair (ii -> first, component_number));
            for (auto nbi = ii -> second.getNeighborsIn ().begin ();
                    nbi != ii -> second.getNeighborsIn ().end (); ++ nbi)
            {
                if (components.find (*nbi) == components.end ())
                    components.insert (std::make_pair (*nbi, component_number));
                else
                {
                    int currentComponent = components.at (*nbi);
                    for (auto c: components)
                        if (c.second == component_number)
                        {
                            components.at (c.first) = currentComponent;
                        }
                }
            }
            for (auto nbi = ii -> second.getNeighborsOut ().begin ();
                    nbi != ii -> second.getNeighborsOut ().end (); ++ nbi)
            {
                if (components.find (*nbi) == components.end ())
                    components.insert (std::make_pair (*nbi, component_number));
                else
                {
                    int currentComponent = components.at (*nbi);
                    for (auto c: components)
                        if (c.second == component_number)
                            components.at (c.first) = currentComponent;
                }
            }
            component_number ++;
        }
    }

    std::set <int> uniqueComponents;
    for (auto c:components)
        uniqueComponents.insert (c.second);

    int largestComponentValue = -1;
    int largestComponentNumber = -1;
    std::map <int, int> componentSize;
    for (auto uc:uniqueComponents)
    {
        int comSize = 0;
        for (auto c:components)
        {
            if (c.second == uc)
                comSize ++;
        }
        if (comSize > largestComponentValue)
        {
            largestComponentValue = comSize;
            largestComponentNumber = uc;
        }
        componentSize.insert (std::make_pair (uc, comSize));
    }

    // Delete smaller graph components
    int delCount = 0;
    for (auto edge:edges)
    {
        osm_id_t from = edge.getFromVertex ();
        osm_id_t to = edge.getToVertex ();
        if (components.at (from) != largestComponentNumber ||
                components.at (to) != largestComponentNumber)
        {
            vertices.erase (from);
            vertices.erase (to);
            edges.erase (edges.begin () + delCount);
        }
        delCount ++;
    }

    return NULL;
}
