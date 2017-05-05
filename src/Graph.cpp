#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <map>

typedef std::string osm_id_t;
typedef int osm_edge_id_t;

struct osm_vertex_t
{
    private:
        std::set <osm_id_t> in, out;
        double lat, lon;

    public:
        bool inCompactGraph = true;
        void addNeighborIn (osm_id_t osm_id) { in.insert (osm_id); }
        void addNeighborOut (osm_id_t osm_id) { out.insert (osm_id); }
        int getDegreeIn () { return in.size (); }
        int getDegreeOut () { return out.size (); }
        void setLat (double lat) { this -> lat = lat; }
        void setLon (double lon) { this -> lon = lon; }
        double getLat () { return lat; }
        double getLon () { return lon; }
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
        osm_edge_id_t id;
        std::set <int> replacingEdges;
        bool inOriginalGraph;

    public:
        float dist;
        float weight;
        bool inCompactGraph = true;
        std::string highway;
        osm_id_t getFromVertex () { return from; }
        osm_id_t getToVertex () { return to; }
        osm_edge_id_t getID () { return id; }
        std::set <int> isReplacementFor () { return replacingEdges; }
        bool inOriginal () { return inOriginalGraph; }

        osm_edge_t (osm_id_t fromId, osm_id_t toId, float dist, float weight,
                   std::string highway, int id, std::set <int> isRepFor,
                   bool inOriginal)
        {
            this -> to = toId;
            this -> from = fromId;
            this -> dist = dist;
            this -> weight = weight;
            this -> highway = highway;
            this -> id = id;
            this -> replacingEdges.insert (isRepFor.begin (), isRepFor.end ());
            this -> inOriginalGraph = inOriginal;
        }
};

typedef std::map <osm_id_t, osm_vertex_t> vertexMap;
typedef std::vector <osm_edge_t> edgeVector;
typedef std::map <int, std::set <int>> replacementMap;
int edgeIDs = 1;

void graphFromDf (Rcpp::DataFrame gr, vertexMap &vm, edgeVector &e)
{
    edgeIDs = 1;
    Rcpp::StringVector from = gr ["from_id"];
    Rcpp::StringVector to = gr ["to_id"];
    Rcpp::NumericVector from_lon = gr ["from_lon"];
    Rcpp::NumericVector from_lat = gr ["from_lat"];
    Rcpp::NumericVector to_lon = gr ["to_lon"];
    Rcpp::NumericVector to_lat = gr ["to_lat"];
    Rcpp::NumericVector dist = gr ["d"];
    Rcpp::NumericVector weight = gr ["d_weighted"];
    Rcpp::StringVector hw = gr ["highway"];

    for (int i = 0; i < to.length (); i ++)
    {
        osm_id_t fromId = std::string (from [i]);
        osm_id_t toId = std::string (to [i]);

        if (vm.find (fromId) == vm.end ())
        {
            osm_vertex_t fromV = osm_vertex_t ();
            fromV.setLat (from_lat [i]);
            fromV.setLon (from_lon [i]);
            vm.insert (std::make_pair(fromId, fromV));
        }
        osm_vertex_t fromVtx = vm.at (fromId);
        fromVtx.addNeighborOut (toId);
        vm [fromId] = fromVtx;

        if (vm.find (toId) == vm.end ())
        {
            osm_vertex_t toV = osm_vertex_t ();
            toV.setLat (to_lat [i]);
            toV.setLon (to_lon [i]);
            vm.insert (std::make_pair(toId, toV));
        }
        osm_vertex_t toVtx = vm.at (toId);
        toVtx.addNeighborIn (fromId);
        vm [toId] = toVtx;

        std::set <int> replacementEdges;
        osm_edge_t edge = osm_edge_t (fromId, toId, dist [i], weight [i],
                std::string (hw [i]), edgeIDs ++, replacementEdges, true);
        e.push_back (edge);
    }
}

void getLargestGraphComponent (vertexMap &v, std::map <osm_id_t, int> &com,
        int &largestId)
{
    int component_number = 0;
    // initialize components map
    for (auto it = v.begin (); it != v.end (); ++ it)
        com.insert (std::make_pair (it -> first, -1));

    for (auto it = v.begin (); it != v.end (); ++ it)
    {
        std::set <int> comps;
        osm_id_t vtxId = it -> first;
        osm_vertex_t vtx = it -> second;
        std::set <osm_id_t> neighbors = vtx.getAllNeighbors ();
        comps.insert (com.at (vtxId));
        for (auto n:neighbors)
            comps.insert (com.at (n));
        int largestCompNum = *comps.rbegin ();
        if (largestCompNum == -1)
            largestCompNum = component_number ++;
        com.at (vtxId) = largestCompNum;
        for (auto n:neighbors)
            com.at (n) = largestCompNum;
        for (auto c = com.begin (); c != com.end (); ++ c)
        {
            osm_id_t cOsm = c -> first;
            int cNum = c -> second;
            if (comps.find (cNum) != comps.end () && cNum != -1)
                com.at (cOsm) = largestCompNum;
        }
    }

    std::set <int> uniqueComponents;
    for (auto c:com)
        uniqueComponents.insert (c.second);

    int largestComponentValue = -1;
    std::map <int, int> componentSize;
    for (auto uc:uniqueComponents)
    {
        int comSize = 0;
        for (auto c:com)
        {
            if (c.second == uc)
                comSize ++;
        }
        if (comSize > largestComponentValue)
        {
            largestComponentValue = comSize;
            largestId = uc;
        }
        componentSize.insert (std::make_pair (uc, comSize));
    }
}

void removeSmallGraphComponents (vertexMap &v, edgeVector &e,
        std::map <osm_id_t, int> &components, int &largestNum)
{
    for (auto comp = components.begin (); comp != components.end (); comp ++)
        if (comp -> second != largestNum)
            v.erase (comp -> first);
    auto eIt = e.begin ();
    while (eIt != e.end ())
    {
        osm_id_t fId = eIt -> getFromVertex ();
        if (v.find (fId) == v.end ())
            eIt = e.erase (eIt);
        else
            eIt ++;
    }
}

void removeIntermediateVertices (vertexMap &v, edgeVector &e)
{
    auto vert = v.begin ();
    while (vert != v.end ())
    {
        osm_id_t id = vert -> first;
        osm_vertex_t vt = vert -> second;

        std::set <osm_id_t> nIn = vt.getNeighborsIn ();
        std::set <osm_id_t> nOut = vt.getNeighborsOut ();
        std::set <osm_id_t> nAll = vt.getAllNeighbors ();

        bool isIntermediateSingle = vt.isIntermediateSingle ();
        bool isIntermediateDouble = vt.isIntermediateDouble ();

        bool hasSingleNeighbors = false;
        for (auto nId:nAll)
        {
            osm_vertex_t n = v.at (nId);
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
                osm_vertex_t nVtx = v.at (nId);
                nVtx.replaceNeighbor (id, replacementId);
                if (isIntermediateDouble)
                {
                    idFromNew = nId;
                    idToNew = replacementId;
                }
                v.at (nId) = nVtx;
            }
            // vert = v.erase (vert);
            // vert -> inCompactGraph = false;
            vert ++;

            // update edges
            float distNew = 0;
            float weightNew = 0;
            std::string hwNew = "";
            int numFound = 0;
            int edgesToDelete = 1;
            if (isIntermediateDouble)
                edgesToDelete = 3;
            auto edge = e.begin ();
            while (edge != e.end ())
            {
                if (edge -> inCompactGraph)
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
                        hwNew = edge -> highway;
                        distNew += edge -> dist;
                        weightNew += edge -> weight;
                        std::set <int> replacingEdges =
                            edge -> isReplacementFor ();
                        if (numFound >= edgesToDelete)
                        {
                            replacingEdges.insert (edge -> getID ());
                            if (isIntermediateDouble)
                            {
                                distNew = distNew / 2;
                                weightNew = weightNew / 2;
                                osm_edge_t edgeNew = osm_edge_t (idToNew,
                                        idFromNew, distNew, weightNew, hwNew,
                                        edgeIDs ++, replacingEdges, false);
                                e.push_back (edgeNew);
                            }
                            osm_edge_t edgeNew = osm_edge_t (idFromNew,
                                    idToNew, distNew, weightNew, hwNew,
                                    edgeIDs ++, replacingEdges, false);
                            e.push_back (edgeNew);

                            edge -> inCompactGraph = false;
                            edge ++;
                            break;
                        } else
                        {
                            edge -> inCompactGraph = false;
                            edge ++;
                            numFound ++;
                        }
                    } else
                        edge ++;
                } else
                    edge ++;
            }
        } else
            vert ++;
    }
}

//' rcpp_makeCompactGraph
//'
//' Removes nodes and edges from a graph that are not needed for routing
//'
//' @param graph graph to be processed
//' @return \code{Rcpp::List} containing one \code{data.frame} with the compact
//' graph, one \code{data.frame} with the original graph and one
//' \code{data.frame} containing information about the relating edge ids of the
//' original and compact graph.
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::List rcpp_makeCompactGraph (Rcpp::DataFrame graph)
{
    vertexMap vertices;
    edgeVector edges;
    std::map <osm_id_t, int> components;
    int largestComponent;

    graphFromDf (graph, vertices, edges);
    getLargestGraphComponent (vertices, components, largestComponent);
    removeSmallGraphComponents (vertices, edges, components, largestComponent);
    removeIntermediateVertices (vertices, edges);

    Rcpp::StringVector fromCompact, toCompact, highwayCompact, fromOG, toOG,
        highwayOG;
    Rcpp::NumericVector from_latCompact, from_lonCompact, to_latCompact,
    to_lonCompact, distCompact, weightCompact, edgeidCompact, from_latOG,
    from_lonOG, to_latOG, to_lonOG, distOG, weightOG, edgeidOG;

    replacementMap repMap;

    for (auto e:edges)
    {
        osm_id_t from = e.getFromVertex ();
        osm_id_t to = e.getToVertex ();
        osm_vertex_t fromVtx = vertices.at (from);
        osm_vertex_t toVtx = vertices.at (to);
        if (e.inCompactGraph)
        {
            fromCompact.push_back (from);
            toCompact.push_back (to);
            highwayCompact.push_back (e.highway);
            distCompact.push_back (e.dist);
            weightCompact.push_back (e.weight);
            from_latCompact.push_back (fromVtx.getLat ());
            from_lonCompact.push_back (fromVtx.getLon ());
            to_latCompact.push_back (toVtx.getLat ());
            to_lonCompact.push_back (toVtx.getLon ());
            edgeidCompact.push_back (e.getID ());

            std::set <int> repEdge = e.isReplacementFor ();
            if (repEdge.size () == 0)
                repEdge.insert (e.getID ());
            std::set <int> repTotal = repMap [e.getID ()];
            repTotal.insert (repEdge.begin (), repEdge.end ());
            repMap [e.getID ()] = repTotal;
        }
        if (e.inOriginal ())
        {
            fromOG.push_back (from);
            toOG.push_back (to);
            highwayOG.push_back (e.highway);
            distOG.push_back (e.dist);
            weightOG.push_back (e.weight);
            from_latOG.push_back (fromVtx.getLat ());
            from_lonOG.push_back (fromVtx.getLon ());
            to_latOG.push_back (toVtx.getLat ());
            to_lonOG.push_back (toVtx.getLon ());
            edgeidOG.push_back (e.getID ());
        }
    }

    Rcpp::NumericVector rpKey, rpVal;
    for (auto rp = repMap.begin (); rp != repMap.end (); ++ rp)
    {
        int k = rp -> first;
        std::set <int> v = rp -> second;
        for (auto val:v)
        {
            rpKey.push_back (k);
            rpVal.push_back (val);
        }
    }

    Rcpp::DataFrame compact = Rcpp::DataFrame::create (
            Rcpp::Named ("from_id") = fromCompact,
            Rcpp::Named ("to_id") = toCompact,
            Rcpp::Named ("edge_id") = edgeidCompact,
            Rcpp::Named ("d") = distCompact,
            Rcpp::Named ("d_weighted") = weightCompact,
            Rcpp::Named ("from_lat") = from_latCompact,
            Rcpp::Named ("from_lon") = from_lonCompact,
            Rcpp::Named ("to_lat") = to_latCompact,
            Rcpp::Named ("to_lon") = to_lonCompact,
            Rcpp::Named ("highway") = highwayCompact);

    Rcpp::DataFrame og = Rcpp::DataFrame::create (
            Rcpp::Named ("from_id") = fromOG,
            Rcpp::Named ("to_id") = toOG,
            Rcpp::Named ("edge_id") = edgeidOG,
            Rcpp::Named ("d") = distOG,
            Rcpp::Named ("d_weighted") = weightOG,
            Rcpp::Named ("from_lat") = from_latOG,
            Rcpp::Named ("from_lon") = from_lonOG,
            Rcpp::Named ("to_lat") = to_latOG,
            Rcpp::Named ("to_lon") = to_lonOG,
            Rcpp::Named ("highway") = highwayOG);

    Rcpp::DataFrame rel = Rcpp::DataFrame::create (
            Rcpp::Named ("id_compact") = rpKey,
            Rcpp::Named ("id_original") = rpVal);

    return Rcpp::List::create (
            Rcpp::Named ("compact") = compact,
            Rcpp::Named ("original") = og,
            Rcpp::Named ("map") = rel);
}
