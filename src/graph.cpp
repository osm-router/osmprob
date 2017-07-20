#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <map>

typedef std::string osm_id_t;
typedef int osm_edge_id_t;

struct osm_vertex_t
{
    private:
        std::unordered_set <osm_id_t> in, out;
        double lat, lon;

    public:
        void add_neighbour_in (osm_id_t osm_id) { in.insert (osm_id); }
        void add_neighbour_out (osm_id_t osm_id) { out.insert (osm_id); }
        int get_degree_in () { return in.size (); }
        int get_degree_out () { return out.size (); }
        void set_lat (double lat) { this -> lat = lat; }
        void set_lon (double lon) { this -> lon = lon; }
        double getLat () { return lat; }
        double getLon () { return lon; }
        std::unordered_set <osm_id_t> get_all_neighbours ()
        {
            std::unordered_set <osm_id_t> all_neighbours = in;
            all_neighbours.insert (out.begin (), out.end ());
            return all_neighbours;
        }
        void replace_neighbour (osm_id_t n_old, osm_id_t n_new)
        {
            if (in.find (n_old) != in.end ())
            {
                in.erase (n_old);
                in.insert (n_new);
            }
            if (out.find (n_old) != out.end ())
            {
                out.erase (n_old);
                out.insert (n_new);
            }
        }
        bool is_intermediate_single ()
        {
            return (in.size () == 1 && out.size () == 1 &&
                    get_all_neighbours ().size () == 2);
        }
        bool is_intermediate_double ()
        {
            return (in.size () == 2 && out.size () == 2 &&
                    get_all_neighbours ().size () == 2);
        }
};

struct osm_edge_t
{
    private:
        osm_id_t from, to;
        osm_edge_id_t id;
        std::set <int> replacing_edges;
        bool in_original_graph;

    public:
        float dist;
        float weight;
        bool replaced_by_compact = false;
        std::string highway;
        osm_id_t get_from_vertex () { return from; }
        osm_id_t get_to_vertex () { return to; }
        osm_edge_id_t getID () { return id; }
        std::set <int> is_replacement_for () { return replacing_edges; }
        bool in_original () { return in_original_graph; }

        osm_edge_t (osm_id_t from_id, osm_id_t to_id, float dist, float weight,
                   std::string highway, int id, std::set <int> is_rep_for,
                   bool in_original)
        {
            this -> to = to_id;
            this -> from = from_id;
            this -> dist = dist;
            this -> weight = weight;
            this -> highway = highway;
            this -> id = id;
            this -> replacing_edges.insert (is_rep_for.begin (),
                    is_rep_for.end ());
            this -> in_original_graph = in_original;
        }
};

typedef std::unordered_map <osm_id_t, osm_vertex_t> vertex_map_t;
typedef std::vector <osm_edge_t> edge_vector_t;
typedef std::unordered_map <int, osm_edge_t> edge_map_t;
typedef std::unordered_map <osm_id_t, std::set <int>> vert2edge_map_t;
typedef std::map <int, std::set <int>> replacement_map_t;

void add_to_edge_map (vert2edge_map_t &vert2edge_map, osm_id_t vid, int eid)
{
    std::set <int> edge_ids;
    if (vert2edge_map.find (vid) == vert2edge_map.end ())
    {
        edge_ids.insert (eid);
        vert2edge_map.emplace (vid, edge_ids);
    } else
    {
        edge_ids = vert2edge_map [vid];
        edge_ids.insert (eid);
        vert2edge_map [vid] = edge_ids;
    }
}

void erase_from_edge_map (vert2edge_map_t &vert2edge_map, osm_id_t vid, int eid)
{
    std::set <int> edge_ids = vert2edge_map [vid];
    if (edge_ids.find (eid) != edge_ids.end ())
    {
        edge_ids.erase (eid);
        vert2edge_map [vid] = edge_ids;
    }
}

void graph_from_df (Rcpp::DataFrame gr, vertex_map_t &vm, edge_vector_t &e,
        edge_map_t &edge_map, vert2edge_map_t &vert2edge_map)
{
    Rcpp::StringVector from = gr ["from_id"];
    Rcpp::StringVector to = gr ["to_id"];
    Rcpp::NumericVector from_lon = gr ["from_lon"];
    Rcpp::NumericVector from_lat = gr ["from_lat"];
    Rcpp::NumericVector to_lon = gr ["to_lon"];
    Rcpp::NumericVector to_lat = gr ["to_lat"];
    Rcpp::NumericVector edge_id = gr ["edge_id"];
    Rcpp::NumericVector dist = gr ["d"];
    Rcpp::NumericVector weight = gr ["d_weighted"];
    Rcpp::StringVector hw = gr ["highway"];

    for (int i = 0; i < to.length (); i ++)
    {
        osm_id_t from_id = std::string (from [i]);
        osm_id_t to_id = std::string (to [i]);

        if (vm.find (from_id) == vm.end ())
        {
            osm_vertex_t fromV = osm_vertex_t ();
            fromV.set_lat (from_lat [i]);
            fromV.set_lon (from_lon [i]);
            vm.emplace (from_id, fromV);
        }
        osm_vertex_t from_vtx = vm.at (from_id);
        from_vtx.add_neighbour_out (to_id);
        vm [from_id] = from_vtx;

        if (vm.find (to_id) == vm.end ())
        {
            osm_vertex_t toV = osm_vertex_t ();
            toV.set_lat (to_lat [i]);
            toV.set_lon (to_lon [i]);
            vm.emplace (to_id, toV);
        }
        osm_vertex_t to_vtx = vm.at (to_id);
        to_vtx.add_neighbour_in (from_id);
        vm [to_id] = to_vtx;

        std::set <int> replacementEdges;
        osm_edge_t edge = osm_edge_t (from_id, to_id, dist [i], weight [i],
                std::string (hw [i]), edge_id [i], replacementEdges, true);
        e.push_back (edge);
        edge_map.emplace (edge_id [i], edge);
        add_to_edge_map (vert2edge_map, from_id, edge_id [i]);
        add_to_edge_map (vert2edge_map, to_id, edge_id [i]);
    }
}

void get_largest_graph_component (vertex_map_t &v,
        std::unordered_map <osm_id_t, int> &com,
        int &largest_id)
{
    // initialize components map
    for (auto it = v.begin (); it != v.end (); ++ it)
        com.insert (std::make_pair (it -> first, -1));

    std::unordered_set <osm_id_t> all_verts, component, nbs_todo, nbs_done;
    for (auto it = v.begin (); it != v.end (); ++ it)
        all_verts.insert (it -> first);
    osm_id_t vt = (*all_verts.begin ());
    nbs_todo.insert (vt);
    int compnum = 0;
    while (all_verts.size () > 0)
    {
        vt = (*nbs_todo.begin ());
        component.insert (vt);
        com.at (vt) = compnum;
        all_verts.erase (vt);

        osm_vertex_t vtx = v.find (vt)->second;
        std::unordered_set <osm_id_t> nbs = vtx.get_all_neighbours ();
        for (auto n: nbs)
        {
            component.insert (n);
            com.at (vt) = compnum;
            if (nbs_done.find (n) == nbs_done.end ())
                nbs_todo.insert (n);
        }
        nbs_todo.erase (vt);
        nbs_done.insert (vt);

        if (nbs_todo.size () == 0 && all_verts.size () > 0)
        {
            nbs_todo.insert (*all_verts.begin ());
            compnum++;
        }
    }

    std::vector <int> comp_sizes (compnum, 0);
    for (auto c: com)
        comp_sizes [c.second]++;
    auto maxi = std::max_element (comp_sizes.begin (), comp_sizes.end ());
    largest_id = std::distance (comp_sizes.begin (), maxi);
    //int maxsize = comp_sizes [largest_id];
}

void remove_small_graph_components (vertex_map_t &vertex_map,
        edge_vector_t &edge_map,
        std::unordered_map <osm_id_t, int> &components, int &largest_num)
{
    for (auto comp = components.begin (); comp != components.end (); comp ++)
        if (comp -> second != largest_num)
            vertex_map.erase (comp -> first);
    auto eIt = edge_map.begin ();
    while (eIt != edge_map.end ())
    {
        osm_id_t fId = eIt -> get_from_vertex ();
        if (vertex_map.find (fId) == vertex_map.end ())
            eIt = edge_map.erase (eIt);
        else
            eIt ++;
    }
}

void remove_intermediate_vertices (vertex_map_t &v, edge_vector_t &e, replacement_map_t &reps)
{
    int max_edge_id = 0;
    for (auto i: e)
        if (i.getID () > max_edge_id)
            max_edge_id = i.getID ();

    auto vert = v.begin ();
    while (vert != v.end ())
    {
        osm_id_t id = vert -> first;
        osm_vertex_t vt = vert -> second;

        std::unordered_set <osm_id_t> n_all = vt.get_all_neighbours ();
        bool is_intermediate_single = vt.is_intermediate_single ();
        bool is_intermediate_double = vt.is_intermediate_double ();

        if (is_intermediate_single || is_intermediate_double)
        {
            osm_id_t id_from_new, id_to_new;

            for (auto n_id: n_all)
            {
                osm_id_t replacement_id;
                for (auto repl: n_all)
                    if (repl != n_id)
                        replacement_id = repl;
                osm_vertex_t nVtx = v.at (n_id);
                nVtx.replace_neighbour (id, replacement_id);
                if (is_intermediate_double)
                {
                    id_from_new = n_id;
                    id_to_new = replacement_id;
                }
                v.at (n_id) = nVtx;
            }

            float dist_new = 0;
            float weight_new = 0;
            std::string hw_new = "";
            int num_found = 0;
            int edges_to_delete = 1;
            if (is_intermediate_double)
                edges_to_delete = 3;
            auto edge = e.begin ();
            while (edge != e.end ())
            {
                if (!edge -> replaced_by_compact)
                {
                    osm_id_t e_from = edge -> get_from_vertex ();
                    osm_id_t e_to = edge -> get_to_vertex ();
                    if (e_from == id || e_to == id)
                    {
                        std::set <int> comp_replacements =
                            reps [edge -> getID ()];
                        comp_replacements.insert (max_edge_id);
                        reps [edge -> getID ()] = comp_replacements;

                        for (int k:comp_replacements)
                        {
                            std::set <int> cascade_repl = reps [k];
                            cascade_repl.insert (edge -> getID ());
                            cascade_repl.insert (comp_replacements.begin (),
                                    comp_replacements.end ());
                            reps [k] = cascade_repl;
                        }

                        edge -> replaced_by_compact = true;
                        if (is_intermediate_single)
                        {
                            if (e_from == id)
                                id_to_new = e_to;
                            if (e_to == id)
                                id_from_new = e_from;
                        }
                        hw_new = edge -> highway;
                        dist_new += edge -> dist;
                        weight_new += edge -> weight;
                        std::set <int> replacing_edges =
                            edge -> is_replacement_for ();
                        if (num_found >= edges_to_delete)
                        {
                            replacing_edges.insert (edge -> getID ());
                            if (is_intermediate_double)
                            {
                                dist_new = dist_new / 2;
                                weight_new = weight_new / 2;
                                osm_edge_t edge_new = osm_edge_t (id_to_new,
                                        id_from_new, dist_new, weight_new,
                                        hw_new, max_edge_id ++, replacing_edges,
                                        false);
                                e.push_back (edge_new);
                            }
                            osm_edge_t edge_new = osm_edge_t (id_from_new,
                                    id_to_new, dist_new, weight_new, hw_new,
                                    max_edge_id ++, replacing_edges, false);
                            e.push_back (edge_new);
                            break;
                        }
                        num_found ++;
                    }
                }
                edge ++;
            }
        }
        vert ++;
    }
}


void contract_graph (vertex_map_t &vertex_map, edge_map_t &edge_map,
        vert2edge_map_t &vert2edge_map)
{
    std::unordered_set <osm_id_t> verts;
    for (auto v: vertex_map)
        verts.insert (v.first);

    int max_edge_id = 0;
    for (auto e: edge_map)
        if (e.second.getID () > max_edge_id)
            max_edge_id = e.second.getID ();
    max_edge_id++;

    std::set <int> edges_to_erase;

    while (verts.size () > 0)
    {
        std::unordered_set <osm_id_t>::iterator vid = verts.begin ();
        osm_id_t vtx_id = vertex_map.find (*vid)->first;
        osm_vertex_t vtx = vertex_map.find (*vid)->second;
        std::set <int> edges = vert2edge_map [vtx_id];

        if ((vtx.is_intermediate_single () || vtx.is_intermediate_double ()) &&
                (edges.size () == 2 || edges.size () == 4))
        {
            // remove intervening vertex:
            auto nbs = vtx.get_all_neighbours (); // unordered_set <osm_id_t>
            std::vector <osm_id_t> two_nbs;
            for (osm_id_t nb: nbs)
                two_nbs.push_back (nb);

            osm_vertex_t vt_from = vertex_map [two_nbs [0]],
                vt_to = vertex_map [two_nbs [1]];

            vt_from.replace_neighbour (vtx_id, two_nbs [1]);
            vt_to.replace_neighbour (vtx_id, two_nbs [0]);
            vertex_map [two_nbs [0]] = vt_from;
            vertex_map [two_nbs [1]] = vt_to;

            vertex_map.erase (vtx_id);

            // construct new edge and remove old ones
            float d_to = 0.0, d_from = 0.0, wt_to = 0.0, wt_from = 0.0;
            std::set <int> replacement_edges;
            replacement_edges.clear ();
            std::string hw;
            for (int e: edges)
            {
                replacement_edges.insert (e);
                osm_edge_t ei = edge_map.find (e)->second;
                // NOTE: There is no check that types of highways are consistent!
                hw = ei.highway;
                if (ei.get_from_vertex () == two_nbs [0] ||
                        ei.get_to_vertex () == two_nbs [1])
                {
                    d_to += ei.dist;
                    wt_to += ei.weight;
                } else if (ei.get_from_vertex () == two_nbs [1] ||
                        ei.get_to_vertex () == two_nbs [0])
                {
                    d_from += ei.dist;
                    wt_from += ei.weight;
                }
                edges_to_erase.insert (e);
                erase_from_edge_map (vert2edge_map, two_nbs [0], e);
                erase_from_edge_map (vert2edge_map, two_nbs [1], e);
            }

            if (d_to > 0.0)
            {
                osm_edge_t new_edge = osm_edge_t (two_nbs [0], two_nbs [1],
                        d_to, wt_to, hw, max_edge_id, replacement_edges, false);
                add_to_edge_map (vert2edge_map, two_nbs [0], max_edge_id);
                add_to_edge_map (vert2edge_map, two_nbs [1], max_edge_id);
                edge_map.emplace (max_edge_id++, new_edge);
            }
            if (d_from > 0.0)
            {
                osm_edge_t new_edge = osm_edge_t (two_nbs [1], two_nbs [0],
                        d_from, wt_from, hw, max_edge_id, replacement_edges,
                        false);
                add_to_edge_map (vert2edge_map, two_nbs [0], max_edge_id);
                add_to_edge_map (vert2edge_map, two_nbs [1], max_edge_id);
                edge_map.emplace (max_edge_id++, new_edge);
            }
            vert2edge_map.erase (vtx_id);
        }
        verts.erase (vtx_id);
    }

    for (int e: edges_to_erase)
        edge_map.erase (e);
}

//' rcpp_make_compact_graph
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
Rcpp::List rcpp_make_compact_graph (Rcpp::DataFrame graph)
{
    vertex_map_t vertices;
    edge_vector_t edges;
    edge_map_t edge_map;
    replacement_map_t rep_map;
    std::unordered_map <osm_id_t, int> components;
    int largest_component;
    vert2edge_map_t vert2edge_map;

    graph_from_df (graph, vertices, edges, edge_map, vert2edge_map);
    get_largest_graph_component (vertices, components, largest_component);
    //remove_small_graph_components (vertices, edges, components,
    //        largest_component);
    vertex_map_t vertices2 = vertices;
    edge_map_t edge_map2 = edge_map;
    contract_graph (vertices2, edge_map2, vert2edge_map);
    //remove_intermediate_vertices (vertices, edges, rep_map);

    int nedges = edge_map2.size ();

    Rcpp::StringVector from_compact (nedges), to_compact (nedges),
        highway_compact (nedges), from_og (nedges),
        to_og (nedges), highway_og (nedges);
    Rcpp::NumericVector from_lat_compact (nedges), from_lon_compact (nedges),
        to_lat_compact (nedges), to_lon_compact (nedges), dist_compact (nedges),
        weight_compact (nedges), edgeid_compact (nedges), from_lat_og (nedges),
        from_lon_og (nedges), to_lat_og (nedges), to_lon_og (nedges),
        dist_og (nedges), weight_og (nedges), edgeid_og (nedges);

    int map_size = 0; // size of original -> contracted map
    for (auto e = edge_map2.begin (); e != edge_map2.end (); ++e)
    {
        osm_id_t from = e->second.get_from_vertex ();
        osm_id_t to = e->second.get_to_vertex ();
        osm_vertex_t from_vtx = vertices2.at (from);
        osm_vertex_t to_vtx = vertices2.at (to);

        int en = std::distance (edge_map2.begin (), e);

        from_compact (en) = from;
        to_compact (en) = to;
        highway_compact (en) = e->second.highway;
        dist_compact (en) = e->second.dist;
        weight_compact (en) = e->second.weight;
        from_lat_compact (en) = from_vtx.getLat ();
        from_lon_compact (en) = from_vtx.getLon ();
        to_lat_compact (en) = to_vtx.getLat ();
        to_lon_compact (en) = to_vtx.getLon ();
        edgeid_compact (en) = e->second.getID ();

        map_size += e->second.is_replacement_for ().size ();
    }

    Rcpp::NumericVector rp_orig (map_size), rp_comp (map_size);
    int pos = 0;
    for (auto e = edge_map2.begin (); e != edge_map2.end (); ++e)
    {
        int eid = e->second.getID ();
        std::set <int> edges = e->second.is_replacement_for ();
        for (auto ei: edges)
        {
            rp_comp (pos) = eid;
            rp_orig (pos++) = ei;
        }
    }
    
    Rcpp::DataFrame compact = Rcpp::DataFrame::create (
            Rcpp::Named ("from_id") = from_compact,
            Rcpp::Named ("to_id") = to_compact,
            Rcpp::Named ("edge_id") = edgeid_compact,
            Rcpp::Named ("d") = dist_compact,
            Rcpp::Named ("d_weighted") = weight_compact,
            Rcpp::Named ("from_lat") = from_lat_compact,
            Rcpp::Named ("from_lon") = from_lon_compact,
            Rcpp::Named ("to_lat") = to_lat_compact,
            Rcpp::Named ("to_lon") = to_lon_compact,
            Rcpp::Named ("highway") = highway_compact);

    Rcpp::DataFrame rel = Rcpp::DataFrame::create (
            Rcpp::Named ("id_compact") = rp_comp,
            Rcpp::Named ("id_original") = rp_orig);

    return Rcpp::List::create (
            Rcpp::Named ("compact") = compact,
            Rcpp::Named ("original") = graph,
            Rcpp::Named ("map") = rel);
}
