#include <Rcpp.h>
#include <algorithm>
#include <vector>

struct osm_vertex_t
{
    private:
        void addNeighbourIn (long long id_in)
        {
            if (std::find (ids_in.begin (), ids_in.end (), id_in) != ids_in.end ())
                ids_in.push_back (id_in);
        }

    public:
        long long id, osm_id;
        std::vector <long long> ids_in, ids_out;
        int getDegreeIn ()
        {
            return ids_in.size ();
        }

        int getDegreeOut ()
        {
            return ids_out.size ();
        }

        void addNeighbourOut (long long id_out, long long id_in)
        {
            if (std::find (ids_out.begin (), ids_out.end (),
                        id_out) != ids_out.end ())
                ids_out.push_back (id_out);
            addNeighbourIn (id_in);
        }
};

struct osm_vertex_list_t
{
    private:
        std::vector <osm_vertex_t> vertices;

    public:
        void add_vertex (long long osm_id, long long to_osm_id, long long *id)
        {
            bool addNew = true;
            std::vector <osm_vertex_t>::iterator it;
            for (it = vertices.begin (); it < vertices.end (); it ++)
            {
                if (osm_id == (*it).osm_id)
                {
                    addNew = false;
                    (*it).addNeighbourOut (to_osm_id, osm_id);
                    break;
                }
            }
            if (addNew)
            {
                osm_vertex_t vertex_new = osm_vertex_t ();
                vertex_new.id = *id ++;
                vertex_new.osm_id = osm_id;
                vertex_new.addNeighbourOut (to_osm_id, osm_id);
                vertices.push_back (vertex_new);
            }
        }

        int getSize () { return vertices.size (); }
};

// [[Rcpp::export]]
Rcpp::DataFrame makeCompactGraph (Rcpp::DataFrame graph)
{
    Rcpp::NumericVector from = graph [0];
    Rcpp::NumericVector to = graph [1];
    Rcpp::LogicalVector isOneway = graph [2];
    int l = from.length ();
    long long id = 0;

    osm_vertex_list_t vertices = osm_vertex_list_t ();

    for (int i = 0; i < l; i++)
    {
        vertices.add_vertex (from [i], to [i], &id);
        if (!isOneway [i])
            vertices.add_vertex (to [i], from [i], &id);
    }

    std::cout << "size: " << vertices.getSize () << std::endl;

    //return Rcpp::DataFrame::create (Rcpp::_["vertex"] = vecOut);
    return NULL;
}
