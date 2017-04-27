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
#include <vector>
#include <string>
#include <list>
#include <limits> // for numeric_limits
#include <set>
#include <utility> // for pair
#include <algorithm>
#include <iterator>

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

void R_init_osmprob(DllInfo* info) {
    R_registerRoutines(info, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

typedef long long vertex_t;
typedef double weight_t;

const weight_t max_weight = std::numeric_limits <weight_t>::infinity();

struct neighbor {
    vertex_t target;
    weight_t weight;
    neighbor (vertex_t arg_target, weight_t arg_weight)
        : target (arg_target), weight (arg_weight) { }
};

typedef std::map <vertex_t, std::vector <neighbor> > adjacency_list_t;

class Graphmp
{
    protected:
        const vertex_t _start_node, _end_node;
        unsigned _num_vertices;
        const double _eta; // The entropy parameter
        const std::vector <vertex_t> _idfrom, _idto;
        const std::vector <weight_t> _d;

    public:
        std::set <vertex_t> all_nodes;
        adjacency_list_t adjlist; // the graph data
        arma::mat d_mat, q_mat, n_mat; // <double>
        arma::vec h_vec, x_vec, v_vec; // also <double>

        Graphmp (std::vector <vertex_t> idfrom, std::vector <vertex_t> idto,
                std::vector <weight_t> d, vertex_t start_node,
                vertex_t end_node, double eta)
            : _idfrom (idfrom), _idto (idto), _d (d),
                _start_node (start_node), _end_node (end_node), _eta (eta)
        {
            _num_vertices = fillGraph (); // fills adjlist with (idfrom, idto, d)
            make_dq_mats ();
            make_n_mat ();
        }

        Graphmp (std::vector <vertex_t> idfrom, std::vector <vertex_t> idto,
                std::vector <weight_t> d, unsigned start_node,
                unsigned end_node)
            : _idfrom (idfrom), _idto (idto), _d (d),
                _start_node (start_node), _end_node (end_node), _eta (1)
        {
            fillGraph ();
        }

        ~Graphmp ()
        {
            // no destructors necessary
        }

        unsigned return_num_vertices() { return _num_vertices;   }
        vertex_t return_start_node() { return _start_node;   }
        vertex_t return_end_node() { return _end_node;   }
        std::vector <vertex_t> return_idfrom() { return _idfrom; }
        std::vector <vertex_t> return_idto() { return _idto; }
        std::vector <weight_t> return_d() { return _d; }
        double return_eta() { return _eta;  }

        unsigned fillGraph ();
        void dumpGraph ();
        void dumpMat (arma::mat mat, std::string mat_name,
                std::vector <std::string> cnames);
        void Dijkstra (vertex_t source, 
                std::vector <weight_t> &min_distance,
                std::vector <vertex_t> &previous);
        std::vector <vertex_t> GetShortestPathTo (vertex_t vertex, 
                const std::vector <vertex_t> &previous);

        void make_dq_mats ();
        void make_n_mat ();
        void make_hxv_vecs ();
        void iterate_q_mat ();
        int calculate_q_mat (double tol, unsigned max_iter);
};


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             FILLGRAPH                              **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

unsigned Graphmp::fillGraph ()
{
    std::vector <vertex_t> idfrom = return_idfrom ();
    std::vector <vertex_t> idto = return_idto ();
    std::vector <weight_t> d = return_d ();
    std::vector <neighbor> nblist;

    for (int i=0; i<idfrom.size (); i++)
    {
        all_nodes.insert (idfrom [i]);
        if (adjlist.find (idfrom [i]) != adjlist.end ())
        {
            nblist = adjlist [idfrom [i]];
            adjlist.erase (idfrom [i]);
        }
        nblist.push_back (neighbor (idto [i], d [i]));
        adjlist.insert (std::make_pair (idfrom [i], nblist));
        all_nodes.insert (idto [i]);
        nblist.clear ();
    }

    return all_nodes.size ();
}

void Graphmp::dumpGraph ()
{
    for (auto const &it1 : adjlist)
        for (auto const &it2 : it1.second)
            Rcpp::Rcout << "[" << it1.first << "] (" <<
                it2.target << ", " << it2.weight << ")" << std::endl;
}

void Graphmp::dumpMat (arma::mat mat, std::string mat_name,
        std::vector <std::string> cnames)
{
    Rcpp::Rcout << "------  " << mat_name << "_MAT  ------" << std::endl;
    Rcpp::Rcout << "        ";
    for (auto i : cnames)
        Rcpp::Rcout << i << "       ";
    Rcpp::Rcout << std::endl << mat << std::endl;
}


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                            DIJKSTRA                                **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graphmp::Dijkstra (vertex_t source,
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

std::vector <vertex_t> Graphmp::GetShortestPathTo (vertex_t vertex, 
        const std::vector <vertex_t> &previous)
{
    std::vector <vertex_t> path;
    for ( ; vertex != -1; vertex = previous [vertex])
        path.push_back (vertex);
    std::reverse (path.begin(), path.end());
    return path;
}

