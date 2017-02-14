#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <map>

typedef long long osm_id_t;

typedef int edge_id_t;

struct osm_vertex_t
{
    private:
        std::set <osm_id_t> in, out;

    public:
        void addNeighborIn (osm_id_t osm_id) { in.insert (osm_id); }
        void addNeighborOut (osm_id_t osm_id) { out.insert (osm_id); }
        int getDegreeIn () { return in.size (); }
        int getDegreeOut () { return out.size (); }
        std::set <osm_id_t> getNeighborsIn () {return in; }
        std::set <osm_id_t> getNeighborsOut () {return out; }
        std::set <osm_id_t> getAllNeighbors ()
        {
            std::set <osm_id_t> allNeighbors = in;
            allNeighbors.insert (out.begin (), out.end ());
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
    int edge_id = 0;
    for (int i = 0; i < to.length (); i ++)
    {
        osm_id_t fromId = from [i];
        osm_id_t toId = to [i];

        if (vertices.find (fromId) == vertices.end ())
            vertices.insert (std::make_pair (fromId, osm_vertex_t ()));
        osm_vertex_t fromVtx = vertices.at (fromId);
        fromVtx.addNeighborOut (toId);
        if (!isOneway [i])
            fromVtx.addNeighborIn (toId);
        vertices [fromId] = fromVtx;

        if (vertices.find (toId) == vertices.end ())
            vertices.insert (std::make_pair (toId, osm_vertex_t ()));
        osm_vertex_t toVtx = vertices.at (toId);
        toVtx.addNeighborIn (fromId);
        if (!isOneway [i])
            toVtx.addNeighborOut (fromId);
        vertices [toId] = toVtx;

        osm_edge_t edge = osm_edge_t (fromId, toId, weight [i], edge_id ++);
        edges.push_back (edge);
        if (!isOneway [i])
        {
            edge = osm_edge_t (toId, fromId, weight [i], edge_id ++);
            edges.push_back (edge);
        }
    }

    Rcpp::Rcout << "BEFORE ditching smaller components: vertices: " << vertices.size () << " edges: " << edges.size () << std::endl;

    // identify largest graph component
    std::map <osm_id_t, int> components;
    int component_number = 0;
    // initialize components map
    for (auto it = vertices.begin (); it != vertices.end (); ++ it)
        components.insert (std::make_pair (it -> first, -1));

    for (auto it = vertices.begin (); it != vertices.end (); ++ it)
    {
        components [it -> first] = component_number;
        std::set <osm_id_t> allNeighbors = it -> second.getAllNeighbors ();
        std::set <int> comps;
        for (auto nbi : allNeighbors)
        {
            if (components.at (nbi) =! -1)
                comps.insert (components.at (nbi));
        }
        for (auto cp = components.begin (); cp != components.end (); ++ cp)
        {
            for (auto cn : comps)
            {
                if (components.at (cp -> first) =! -1)
                {
                    if (components.at (cp -> first) == cn)
                        components.at (cp -> first) = *comps.begin ();
                }
            }
        }
        component_number ++;
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

    Rcpp::Rcout << "AFTER ditching smaller components: vertices: " << vertices.size () << " edges: " << edges.size () << std::endl;

    auto v = vertices.begin ();
    while (v != vertices.end ())
    {
        Rcpp::Rcout << "Checking " << v -> first << std::endl;
        osm_id_t id = v -> first;
        osm_vertex_t vt = v -> second;

        std::set <osm_id_t> nIn = vt.getNeighborsIn ();
        std::set <osm_id_t> nOut = vt.getNeighborsOut ();

        int numNeighbors = vt.getDegreeIn () + vt.getDegreeOut ();
        float neighborRatio =  (float) numNeighbors / vt.getAllNeighbors ().size ();
        // TODO properly delete vertices
        if (numNeighbors == 2 && vt.getAllNeighbors ().size () == 2)
        {
            v = vertices.erase (v);
        } else if (numNeighbors == 4 && vt.getAllNeighbors ().size () == 2)
        {
            v = vertices.erase (v);
        } else
            ++ v;
    }

    Rcpp::NumericVector fromOut;
    Rcpp::NumericVector toOut;
    Rcpp::NumericVector weightOut;
    Rcpp::LogicalVector onewayOut;

    osm_id_t lastFrom = -1;
    osm_id_t lastTo = -1;
    int ct = 0;

    for (auto e:edges)
    {
        osm_id_t from = e.getFromVertex ();
        osm_id_t to = e.getToVertex ();

        if ((from != lastTo) || (to != lastFrom))
        {
            fromOut.push_back (from);
            toOut.push_back (to);
            weightOut.push_back (e.weight);
            onewayOut.push_back (false);
            lastFrom = from;
            lastTo = to;
        }
    }

    return Rcpp::DataFrame::create (Rcpp::Named ("from") = fromOut, Rcpp::Named ("to") = toOut, Rcpp::Named ("weight") = weightOut, Rcpp::Named ("oneway") = onewayOut);
    // return NULL;
}

