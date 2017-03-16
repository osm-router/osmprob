/***************************************************************************
 *  Project:    osmprob
 *  File:       router.h
 *  Language:   C++
 *
 *  osmprob is free software: you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the Free
 *  Software Foundation, either version 3 of the License, or (at your option)
 *  any later version.
 *
 *  osmprob is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 *  more details.
 *
 *  You should have received a copy of the GNU General Public License along with
 *  osm-router.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  Author:     Mark Padgham 
 *  E-Mail:     mark.padgham@email.com 
 *
 *  Description:    Current a skeleton Dijkstra mostly adapted from 
 *                  https://rosettacode.org/wiki/Dijkstra%27s_algorithm#C.2B.2B
 *
 *  Limitations:
 *
 *  Dependencies:       
 *
 *  Compiler Options:   -std=c++11 
 ***************************************************************************/


#include <iostream>
#include <iomanip> // TODO: Delete those headers
#include <vector>
#include <string>
#include <limits> // for numeric_limits
#include <set>

#include <RcppArmadillo.h> // automatically loads Rcpp.h

typedef unsigned vertex_t;
typedef double weight_t;

const weight_t max_weight = std::numeric_limits <weight_t>::infinity();

const double eta = 1.0; // The entropy parameter

struct neighbor {
    vertex_t target;
    weight_t weight;
    neighbor (vertex_t arg_target, weight_t arg_weight)
        : target (arg_target), weight (arg_weight) { }
};

typedef std::vector <std::vector <neighbor> > adjacency_list_t;

class Graph
{
    protected:
        unsigned _start_node, _end_node, _num_vertices;
        const std::vector <vertex_t> _idfrom, _idto;
        const std::vector <weight_t> _d;

    public:
        adjacency_list_t adjlist; // the graph data
        arma::mat cost_mat, d_mat, p_mat, q_mat, n_mat;
        arma::vec h_vec, x_vec, v_vec;
        // d_mat is cost_mat minus last row and last column
        // q_mat is p_mat minus last row and last column
        // n_mat = (1 - q_mat) ^ -1
        // h_vec is Eq.(8) from Saerens
        // x_vec is from Eq.(14) Saerens
        // v_vec is from Eq.(7) Saerens

        Graph (std::vector <vertex_t> idfrom, std::vector <vertex_t> idto,
                std::vector <weight_t> d, unsigned start_node, unsigned end_node)
            : _idfrom (idfrom), _idto (idto), _d (d),
                _start_node (start_node), _end_node (end_node)
        {
            _num_vertices = fillGraph (); // fills adjlist with (idfrom, idto, d)
            make_cost_mat ();
            initialise_pq_mats ();
        }
        ~Graph ()
        {
            for (int i=0; i<adjlist.size (); i++)
                adjlist [i].clear ();
            adjlist.clear ();
            cost_mat.resize (0, 0);
            d_mat.resize (0, 0);
            p_mat.resize (0, 0);
            q_mat.resize (0, 0);
            h_vec.resize (0);
            x_vec.resize (0);
        }

        unsigned return_num_vertices() { return _num_vertices;   }
        unsigned return_start_node() { return _start_node;   }
        unsigned return_end_node() { return _end_node;   }
        std::vector <vertex_t> return_idfrom() { return _idfrom; }
        std::vector <vertex_t> return_idto() { return _idto; }
        std::vector <weight_t> return_d() { return _d; }

        unsigned fillGraph ();
        void Dijkstra (vertex_t source, 
                std::vector <weight_t> &min_distance,
                std::vector <vertex_t> &previous);
        std::vector <vertex_t> GetShortestPathTo (vertex_t vertex, 
                const std::vector <vertex_t> &previous);

        void make_cost_mat ();
        void initialise_pq_mats ();
        void calc_h_vec ();
        void calc_n_mat ();
        void iterate_pq_mats ();
};


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             FILLGRAPH                              **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

unsigned Graph::fillGraph ()
{
    std::vector <vertex_t> idfrom = return_idfrom ();
    std::vector <vertex_t> idto = return_idto ();
    std::vector <weight_t> d = return_d ();
    int ss = idfrom.size ();
    int from_here = idfrom.front ();
    std::vector <neighbor> nblist;
    for (int i=0; i<idfrom.size (); i++)
    {
        int idfromi = idfrom [i];
        if (idfromi == from_here)
        {
            nblist.push_back (neighbor (idto [i], d [i]));
        } else
        {
            from_here = idfromi;
            adjlist.push_back (nblist);
            nblist.clear ();
            nblist.push_back (neighbor (idto [i], d [i]));
        }
    }
    // final nblist also has to be added
    adjlist.push_back (nblist);
    nblist.clear ();

    unsigned num_vertices = adjlist.size ();

    return num_vertices;
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                            DIJKSTRA                                **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graph::Dijkstra (vertex_t source,
        std::vector <weight_t> &min_distance,
        std::vector <vertex_t> &previous)
{
    int n = adjlist.size();
    min_distance.clear();
    min_distance.resize (n, max_weight);
    min_distance [source] = 0;
    previous.clear();
    previous.resize (n, -1);
    std::set <std::pair <weight_t, vertex_t> > vertex_queue;
    vertex_queue.insert (std::make_pair (min_distance [source], source));

    while (!vertex_queue.empty()) 
    {
        weight_t dist = vertex_queue.begin()->first;
        vertex_t u = vertex_queue.begin()->second;
        vertex_queue.erase (vertex_queue.begin());

        // Visit each edge exiting u
        const std::vector <neighbor> &neighbors = adjlist [u];
        for (std::vector <neighbor>::const_iterator neighbor_iter = neighbors.begin();
                neighbor_iter != neighbors.end(); neighbor_iter++)
        {
            vertex_t v = neighbor_iter->target;
            weight_t weight = neighbor_iter->weight;
            weight_t distance_through_u = dist + weight;
            if (distance_through_u < min_distance [v]) {
                vertex_queue.erase (std::make_pair (min_distance [v], v));

                min_distance [v] = distance_through_u;
                previous [v] = u;
                vertex_queue.insert (std::make_pair (min_distance [v], v));

            }

        }
    }
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                         GETSHORTESTPATHTO                          **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

std::vector <vertex_t> Graph::GetShortestPathTo (vertex_t vertex, 
        const std::vector <vertex_t> &previous)
{
    std::vector <vertex_t> path;
    for ( ; vertex != -1; vertex = previous [vertex])
        path.push_back (vertex);
    std::reverse (path.begin(), path.end());
    return path;
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                            MAKECOSTMAT                             **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graph::make_cost_mat ()
{
    /* the diagonal of cost_mat is 0, otherwise the first row contains only one
     * finite entry for escape from start_node. The last column similarly
     * contains only one finite entry for absorption by end_node. */
    const unsigned n = return_num_vertices ();
    const unsigned start_node = return_start_node ();
    const unsigned end_node = return_end_node ();

    cost_mat.resize (n + 2, n + 2);
    cost_mat.fill (max_weight);
    cost_mat.diag().zeros();
    // set weights from start_node and to end_node:
    cost_mat (0, start_node + 1) = 0.0;
    cost_mat (end_node + 1, cost_mat.n_rows - 1) = 0.0;

    for (int i=0; i<n; i++)
    {
        const std::vector <neighbor> &nbs = adjlist [i];
        for (std::vector <neighbor>::const_iterator nb_iter = nbs.begin ();
                nb_iter != nbs.end (); nb_iter++)
        {
            vertex_t v = nb_iter->target;
            cost_mat (i + 1, v + 1) = nb_iter->weight;
        }
    }
    d_mat = cost_mat;
    d_mat.resize (n + 1, n + 1);
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                              MAKEPMAT                              **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graph::initialise_pq_mats ()
{
    const unsigned n = return_num_vertices ();
    const unsigned start_node = return_start_node ();
    const unsigned end_node = return_end_node ();
    double p;

    p_mat.zeros (n + 2, n + 2);
    q_mat.zeros (n + 1, n + 1);
    // set probabilities from start_node and to end_node:
    q_mat (0, start_node + 1) = 1.0;
    p_mat (0, start_node + 1) = 1.0;

    for (int i=0; i<n; i++)
    {
        const std::vector <neighbor> &nbs = adjlist [i];
        const unsigned nverts = nbs.size ();
        for (std::vector <neighbor>::const_iterator nb_iter = nbs.begin ();
                nb_iter != nbs.end (); nb_iter++)
        {
            vertex_t v = nb_iter->target;
            p = 1.0 / (double) nbs.size ();
            if (i == end_node) // absorbing node is not in adjlist
            {
                p = 1.0 / (1.0 + (double) nbs.size ());
                p_mat (i + 1, p_mat.n_cols - 1) = p;
            }
            q_mat (i + 1, v + 1) = p;
            p_mat (i + 1, v + 1) = p;
        }
    }
    p_mat (p_mat.n_rows - 1, p_mat.n_cols - 1) = 1.0;
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             CALC_H_VEC                             **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graph::calc_h_vec ()
{
    const unsigned n = return_num_vertices ();

    h_vec.zeros (n + 1);

    for (int i=0; i<(n + 1); i++)
        for (int j=0; j<(n + 1); j++)
            if (q_mat (i, j) > 0.0)
                h_vec (i) -= q_mat (i, j) * log (q_mat (i, j));
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             CALC_N_MAT                             **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graph::calc_n_mat ()
{
    // N = (1 - Q) ^ (-1) (see below Eq.5 in Saerens et al 2009).
    const unsigned n = return_num_vertices ();

    n_mat.eye (n + 1, n + 1);
    n_mat = (n_mat - q_mat).i();
    x_vec = n_mat * h_vec;
    // d_mat has non-finite values which are set to zero here so they don't
    // contribute to the resultant sum
    arma::mat temp_mat = d_mat;
    temp_mat.elem (arma::find_nonfinite (temp_mat)).zeros ();
    temp_mat = (q_mat * (temp_mat.t ()));
    v_vec = n_mat * temp_mat.diag ();

    // pad x and v with 0 (see Algorithm#1 of Saerens)
    x_vec.resize (x_vec.n_elem + 1);
    x_vec (x_vec.n_elem - 1) = 0.0;
    v_vec.resize (v_vec.n_elem + 1);
    v_vec (v_vec.n_elem - 1) = 0.0;
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                           ITERATE_PQ_MATS                          **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graph::iterate_pq_mats ()
{
    for (arma::uword r=0; r < cost_mat.n_rows; ++r)
    {
        arma::vec c_vec = cost_mat.row (r).t();
        // again here again the non-finite costs are simply set to zero so they
        // don't contribute to the resultant probability sums
        arma::uvec nf = arma::find_nonfinite (c_vec);
        c_vec.elem (nf).zeros ();
        c_vec = (c_vec + v_vec) / eta + x_vec;
        c_vec = (c_vec / arma::accu (c_vec));
        // Then the non-finite values have to be reset to probs of zero
        c_vec.elem (nf).zeros ();
        p_mat.row (r) = c_vec.t();
        //p_mat.row (r) = (c_vec / arma::accu (c_vec)).t();
    }
}
