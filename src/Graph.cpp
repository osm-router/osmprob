#include <Rcpp.h>
#include <algorithm>
#include <vector>

struct osm_vertex_t
{
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
            if (!(std::find (ids_out.begin (), ids_out.end (),
                        id_out) != ids_out.end ()))
            {
                ids_out.push_back (id_out);
                ids_in.push_back (id_in);
            }
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
                vertex_new.id = (*id) ++;
                vertex_new.osm_id = osm_id;
                vertex_new.addNeighbourOut (to_osm_id, osm_id);
                vertices.push_back (vertex_new);
            }
        }

        osm_vertex_t getVertexFromId (long long id)
        {
            std::vector <osm_vertex_t>::iterator it;
            for (it = vertices.begin (); it < vertices.end (); it ++)
            {
                if (id == (*it).id)
                {
                    return (*it);
                }
            }
        }

        osm_vertex_t getVertexFromOsmId (long long osmId)
        {
            std::vector <osm_vertex_t>::iterator it;
            for (it = vertices.begin (); it < vertices.end (); it ++)
            {
                if (osmId == (*it).osm_id)
                {
                    return (*it);
                }
            }
        }

        std::vector <osm_vertex_t> getVertices () { return vertices; }

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

    //PROCESS GRAPH
    
    osm_vertex_list_t vertices_compact = osm_vertex_list_t ();
    id = 0;
    
    std::vector <osm_vertex_t> allVertices = vertices.getVertices ();
    std::vector <osm_vertex_t>::iterator it;
    for (it = allVertices.begin (); it < allVertices.end (); it ++)
    {
        if ((*it).getDegreeIn () != 2 || (*it).getDegreeOut () != 2)
        {
            osm_vertex_t temp_vertex = (*it);
            long long osm_id_new = temp_vertex.osm_id;

            std::vector <long long>::iterator vertexIt;
            for (vertexIt = temp_vertex.ids_out.begin ();
                    vertexIt < temp_vertex.ids_out.end (); vertexIt ++)
            {
                long long neighbourOsmId = *vertexIt;
                osm_vertex_t neighbourVertex = vertices.getVertexFromOsmId (neighbourOsmId);
                long long idPrev = temp_vertex.id;

                while (neighbourVertex.getDegreeOut () == 2)
                {
                    neighbourVertex = vertices.getVertexFromOsmId 
                        (neighbourVertex.ids_out.front ());
                    if (neighbourVertex.id == idPrev)
                    {
                        neighbourVertex = vertices.getVertexFromOsmId 
                            (neighbourVertex.ids_out.back ());
                    }

                    if (temp_vertex.id == idPrev)
                    {
                        break;
                    }
                    idPrev = neighbourVertex.id;
                }
                vertices_compact.add_vertex (osm_id_new,
                        neighbourVertex.osm_id, &id);

                neighbourOsmId = *vertexIt;
                neighbourVertex = vertices.getVertexFromOsmId (neighbourOsmId);
                idPrev = temp_vertex.id;

                while (neighbourVertex.getDegreeIn () == 2)
                {
                    neighbourVertex = vertices.getVertexFromOsmId 
                        (neighbourVertex.ids_in.front ());
                    if (neighbourVertex.id == idPrev)
                    {
                        neighbourVertex = vertices.getVertexFromOsmId 
                            (neighbourVertex.ids_in.back ());
                    }

                    if (temp_vertex.id == idPrev)
                    {
                        break;
                    }
                    idPrev = neighbourVertex.id;
                }
                vertices_compact.add_vertex (osm_id_new,
                        neighbourVertex.osm_id, &id);
            }
        }
    }
    //return Rcpp::DataFrame::create (Rcpp::_["vertex"] = vecOut);
    return NULL;
}
