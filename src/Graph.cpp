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
        void replaceNeighbor (osm_id_t nOld, osm_id_t nNew)
        {
            if (in.find (nOld) != in.end ())
            {
                in.erase (nOld);
                in.insert (nNew);
            }
            if (out.find (nOld) != out.end ())
            {
                out.erase (nOld);
                out.insert (nNew);
            }
        }
        bool isIntermediateSingle ()
        {
            return (in.size () == 1 && out.size () == 1 &&
                    getAllNeighbors ().size () == 2);
        }
        bool isIntermediateDouble ()
        {
            return (in.size () == 2 && out.size () == 2 &&
                    getAllNeighbors ().size () == 2);
        }
};

struct osm_edge_t
{
    private:
        osm_id_t from, to;

    public:
        float weight;
        edge_id_t id;
        osm_id_t getFromVertex () { return from; }
        osm_id_t getToVertex () { return to; }

        osm_edge_t (osm_id_t fromId, osm_id_t toId, float weight,
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

    // identify largest graph component
    std::map <osm_id_t, int> components;

    int component_number = 0;
    // initialize components map
    for (auto it = vertices.begin (); it != vertices.end (); ++ it)
        components.insert (std::make_pair (it -> first, -1));

    for (auto it = vertices.begin (); it != vertices.end (); ++ it)
    {
        std::set <int> comps;
        osm_id_t vtxId = it -> first;
        osm_vertex_t vtx = it -> second;
        std::set <osm_id_t> neighbors = vtx.getAllNeighbors ();
        comps.insert (components.at (vtxId));
        for (auto n:neighbors)
            comps.insert (components.at (n));
        int largestCompNum = *comps.rbegin ();
        if (largestCompNum == -1)
            largestCompNum = component_number ++;
        components.at (vtxId) = largestCompNum;
        for (auto n:neighbors)
            components.at (n) = largestCompNum;
        for (auto c = components.begin (); c != components.end (); ++ c)
        {
            osm_id_t cOsm = c -> first;
            int cNum = c -> second;
            if (comps.find (cNum) != comps.end () && cNum != -1)
                components.at (cOsm) = largestCompNum;
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

    // delete smaller graph components
    for (auto comp = components.begin (); comp != components.end (); comp ++)
        if (comp -> second != largestComponentNumber)
            vertices.erase (comp -> first);
    auto eIt = edges.begin ();
    while (eIt != edges.end ())
    {
        osm_id_t fId = eIt -> getFromVertex ();
        if (vertices.find (fId) == vertices.end ())
            eIt = edges.erase (eIt);
        else
            eIt ++;
    }

    auto v = vertices.begin ();
    while (v != vertices.end ())
    {
        osm_id_t id = v -> first;
        osm_vertex_t vt = v -> second;

        std::set <osm_id_t> nIn = vt.getNeighborsIn ();
        std::set <osm_id_t> nOut = vt.getNeighborsOut ();
        std::set <osm_id_t> nAll = vt.getAllNeighbors ();

        bool isIntermediateSingle = vt.isIntermediateSingle ();
        bool isIntermediateDouble = vt.isIntermediateDouble ();

        bool hasSingleNeighbors = false;
        for (auto nId:nAll)
        {
            osm_vertex_t n = vertices.at (nId);
            if (n.isIntermediateSingle () || n.isIntermediateDouble ())
            {
                hasSingleNeighbors = true;
                break;
            }
        }

        if ((isIntermediateSingle || isIntermediateDouble) && hasSingleNeighbors)
        {
            osm_id_t idFromNew, idToNew;

            for (auto nId:nAll)
            {
                osm_id_t replacementId;
                for (auto repl:nAll)
                    if (repl != nId)
                        replacementId = repl;
                osm_vertex_t nVtx = vertices.at (nId);
                nVtx.replaceNeighbor (id, replacementId);
                if (isIntermediateDouble)
                {
                    idFromNew = nId;
                    idToNew = replacementId;
                }
                vertices.at (nId) = nVtx;
            }
            v = vertices.erase (v);

            // update edges
            float weightNew = 0;
            int numFound = 0;
            int edgesToDelete = 1;
            if (isIntermediateDouble)
                edgesToDelete = 3;
            auto edge = edges.begin ();
            while (edge != edges.end ())
            {
                osm_id_t eFrom = edge -> getFromVertex ();
                osm_id_t eTo = edge -> getToVertex ();
                if (eFrom == id || eTo == id)
                {
                    if (isIntermediateSingle)
                    {
                        if (eFrom == id)
                            idToNew = eTo;
                        if (eTo == id)
                            idFromNew = eFrom;
                    }
                    weightNew += edge -> weight;
                    edge = edges.erase (edge);
                    if (numFound >= edgesToDelete)
                    {
                        if (isIntermediateDouble)
                        {
                            weight = weight / 2;
                            osm_edge_t edgeNew = osm_edge_t (idToNew, idFromNew,
                                    weightNew, edge_id ++);
                            edges.push_back (edgeNew);
                        }
                        osm_edge_t edgeNew = osm_edge_t (idFromNew, idToNew,
                                weightNew, edge_id ++);
                        edges.push_back (edgeNew);
                        break;
                    }
                    numFound ++;
                } else
                    edge ++;
            }
        } else
            v ++;
    }

    Rcpp::NumericVector fromOut;
    Rcpp::NumericVector toOut;
    Rcpp::NumericVector weightOut;
    for (auto e:edges)
    {
        osm_id_t from = e.getFromVertex ();
        osm_id_t to = e.getToVertex ();
        fromOut.push_back (from);
        toOut.push_back (to);
        weightOut.push_back (e.weight);
    }

    return Rcpp::DataFrame::create (Rcpp::Named ("from") = fromOut, Rcpp::Named ("to") = toOut, Rcpp::Named ("weight") = weightOut);
}
