#include <Rcpp.h>

typedef long long osm_id_t;

struct Graph
{
    private:
        std::unordered_map <osm_id_t, std::unordered_map <osm_id_t, float>>
            vertices;

    public:
        void add_vertex (osm_id_t from, const std::unordered_map <osm_id_t,
                float>& to)
        {
            vertices.insert (std::unordered_map <osm_id_t, const
                    std::unordered_map <osm_id_t, float>>::value_type
                    (from,to));
        }

        std::vector <osm_id_t> getPath (osm_id_t start, osm_id_t end)
        {
            std::unordered_map <osm_id_t, float> distances;
            std::unordered_map <osm_id_t, osm_id_t> previous;
            std::vector <osm_id_t> nodes;
            std::vector <osm_id_t> path;

            auto comparator = [&] (osm_id_t left, osm_id_t right) { return distances [left] > distances [right]; };

            for (auto& vertex : vertices)
            {
                if (vertex.first == start)
                    distances [vertex.first] = 0;
                else
                    distances [vertex.first] = std::numeric_limits <float>::max ();

                nodes.push_back (vertex.first);
                push_heap (begin (nodes), std::end (nodes), comparator);
            }

            while (!nodes.empty ())
            {
                pop_heap (begin (nodes), std::end (nodes), comparator);
                osm_id_t smallest = nodes.back ();
                nodes.pop_back ();

                if (smallest == end)
                {
                    while (previous.find (smallest) != std::end (previous))
                    {
                        path.push_back (smallest);
                        smallest = previous [smallest];
                    }
                    break;
                }

                if (distances [smallest] == std::numeric_limits <float>::max ())
                    break;

                for (auto& neighbour : vertices [smallest])
                {
                    float alt = distances [smallest] + neighbour.second;
                    if (alt < distances [neighbour.first])
                    {
                        distances [neighbour.first] = alt;
                        previous [neighbour.first] = smallest;
                        make_heap (begin (nodes), std::end (nodes), comparator);
                    }
                }
            }
            return path;
        }
};

//' dijkstra
//'
//' Calculates the shortest path between two points on a graph using Dijkstra's
//' algorithm
//'
//' @param graph matrix containing the graph
//' @param start id of the start point
//' @param end id of the end point
//' @return a list of edges that are part of the shortest path
// [[Rcpp::export]]
void dijkstra (Rcpp::DataFrame graph, long long start, long long end)
{
    Graph g;
    Rcpp::NumericVector from = graph [0];
    Rcpp::NumericVector to = graph [1];
    Rcpp::NumericVector weight = graph [2];
    for (int i = 0; i < to.length (); i ++)
        g.add_vertex (from [i], {{to [i], weight [i]}});

    for (long long vtx : g.getPath (start,end))
        Rcpp::Rcout << vtx << std::endl;
}
