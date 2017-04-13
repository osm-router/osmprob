#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <map>

typedef std::string osm_id_t;

struct osm_vertex_t
{
    private:
        std::set <osm_id_t> in, out;
        double lat, lon;

    public:
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

    public:
        float dist;
        float weight;
        std::string highway;
        osm_id_t getFromVertex () { return from; }
        osm_id_t getToVertex () { return to; }

        osm_edge_t (osm_id_t fromId, osm_id_t toId, float dist, float weight,
                   std::string highway)
        {
            this -> to = toId;
            this -> from = fromId;
            this -> dist = dist;
            this -> weight = weight;
            this -> highway = highway;
        }
};

typedef std::map <osm_id_t, osm_vertex_t> vertexMap;
typedef std::vector <osm_edge_t> edgeVector;

void graphFromDf (Rcpp::DataFrame gr, vertexMap &vm, edgeVector &e)
{
    Rcpp::StringVector from = gr [0];
    Rcpp::StringVector to = gr [3];
    Rcpp::NumericVector from_lon = gr [1];
    Rcpp::NumericVector from_lat = gr [2];
    Rcpp::NumericVector to_lon = gr [4];
    Rcpp::NumericVector to_lat = gr [5];
    Rcpp::NumericVector dist = gr [6];
    Rcpp::NumericVector weight = gr [7];
    Rcpp::StringVector hw = gr [8];

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

        osm_edge_t edge = osm_edge_t (fromId, toId, dist [i], weight [i],
                std::string (hw [i]));
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
            vert = v.erase (vert);

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
                    edge = e.erase (edge);
                    if (numFound >= edgesToDelete)
                    {
                        if (isIntermediateDouble)
                        {
                            distNew = distNew / 2;
                            weightNew = weightNew / 2;
                            osm_edge_t edgeNew = osm_edge_t (idToNew, idFromNew,
                                    distNew, weightNew, hwNew);
                            e.push_back (edgeNew);
                        }
                        osm_edge_t edgeNew = osm_edge_t (idFromNew, idToNew,
                                distNew, weightNew, hwNew);
                        e.push_back (edgeNew);
                        break;
                    }
                    numFound ++;
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
//' @return Rcpp::DataFrame containing the output graph
// [[Rcpp::export]]
Rcpp::DataFrame rcpp_makeCompactGraph (Rcpp::DataFrame graph)
{
    vertexMap vertices;
    edgeVector edges;
    std::map <osm_id_t, int> components;
    int largestComponent;

    graphFromDf (graph, vertices, edges);
    getLargestGraphComponent (vertices, components, largestComponent);
    removeSmallGraphComponents (vertices, edges, components, largestComponent);
    removeIntermediateVertices (vertices, edges);

    Rcpp::StringVector fromOut;
    Rcpp::StringVector toOut;
    Rcpp::StringVector highwayOut;
    Rcpp::NumericVector from_latOut;
    Rcpp::NumericVector from_lonOut;
    Rcpp::NumericVector to_latOut;
    Rcpp::NumericVector to_lonOut;
    Rcpp::NumericVector distOut;
    Rcpp::NumericVector weightOut;
    for (auto e:edges)
    {
        osm_id_t from = e.getFromVertex ();
        osm_id_t to = e.getToVertex ();
        osm_vertex_t fromVtx = vertices.at (from);
        osm_vertex_t toVtx = vertices.at (to);
        fromOut.push_back (from);
        toOut.push_back (to);
        highwayOut.push_back (e.highway);
        distOut.push_back (e.dist);
        weightOut.push_back (e.weight);
        from_latOut.push_back (fromVtx.getLat ());
        from_lonOut.push_back (fromVtx.getLon ());
        to_latOut.push_back (toVtx.getLat ());
        to_lonOut.push_back (toVtx.getLon ());
    }

    return Rcpp::DataFrame::create (
            Rcpp::Named ("from_id") = fromOut,
            Rcpp::Named ("to_id") = toOut,
            Rcpp::Named ("d") = distOut,
            Rcpp::Named ("d_weighted") = weightOut,
            Rcpp::Named ("from_lat") = from_latOut,
            Rcpp::Named ("from_lon") = from_lonOut,
            Rcpp::Named ("to_lat") = to_latOut,
            Rcpp::Named ("to_lon") = to_lonOut,
            Rcpp::Named ("highway") = highwayOut);
}
