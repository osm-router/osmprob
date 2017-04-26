/***************************************************************************
 *  Project:    osmprob
 *  File:       router.cpp
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

#include "router-mp.h"

// TODO: Move all these back into header file

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             MAKE_DQ_MATS                           **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graphmp::make_dq_mats ()
{
    /* the diagonal of d_mat is 0, otherwise the first row contains only one
     * finite entry for escape from start_node. The last column similarly
     * contains only one finite entry for absorption by end_node. */
    const unsigned num_vertices = return_num_vertices ();
    //const unsigned start_node = return_start_node ();
    //const unsigned end_node = return_end_node ();
    const unsigned dstart_node = std::distance (all_nodes.begin (),
            all_nodes.find (return_start_node ()));
    const unsigned dend_node = std::distance (all_nodes.begin (),
            all_nodes.find (return_end_node ()));

    d_mat = arma::mat (num_vertices + 1, num_vertices + 1);
    d_mat.fill (max_weight);
    d_mat.diag (0.0);
    q_mat.zeros (num_vertices + 1, num_vertices + 1);

    unsigned q_sums [num_vertices] = {0}; // Note: 1 shorter than q_mat

    for (auto const &it1 : adjlist)
    {
        const unsigned di = std::distance (all_nodes.begin (), 
                all_nodes.find (it1.first));
        for (auto const &it2 : it1.second)
        {
            const unsigned dj = std::distance (all_nodes.begin (),
                    all_nodes.find (it2.target));
            d_mat (di + 1, dj + 1) = it2.weight;
            q_mat (di + 1, dj + 1) = 1.0;
            q_sums [di]++;
        }
    }

    d_mat (0, dstart_node + 1) = 1.0;

    // Standardise q_mat, which is the top-left of the probability matrix
    for (arma::uword r=1; r<q_mat.n_rows; ++r)
        if (q_sums [r - 1] > 0) // == 0 if links to TO and not FROM
            q_mat.row (r) = q_mat.row (r) / (double) q_sums [r - 1];
    // Then add links to dstart_node and to absorbing end_node
    q_mat (0, dstart_node + 1) = 1.0;
    q_mat.row (dend_node + 1) = q_mat.row (dend_node + 1) * 
        q_sums [dend_node] / (q_sums [dend_node] + 1.0);
}


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             MAKE_N_MAT                             **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graphmp::make_n_mat ()
{
    // The most computationally expensive part of all, and the only place
    // requiring matrix inversion. This is, however, only required once, and is
    // not repeated within the convergence loop.
    const unsigned n = return_num_vertices ();

    arma::mat unit_mat (n + 1, n + 1, arma::fill::eye);
    n_mat = (unit_mat - q_mat).i();
}


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                           MAKE_HXV_VECS                            **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graphmp::make_hxv_vecs ()
{
    arma::mat lq = -arma::log (q_mat.t ());
    // q_mat has zeros, so log (q) has non-finite values which are here reset to
    // zero so they don't contribute to the resultant sums.
    lq.elem (arma::find_nonfinite (lq)).zeros ();
    arma::mat temp_mat = q_mat * lq; // arma requires this intermediate stage
    h_vec = temp_mat.diag ();
    x_vec = n_mat * h_vec;

    arma::mat dtemp = d_mat;
    dtemp.elem (arma::find_nonfinite (dtemp)).zeros ();
    temp_mat = q_mat * dtemp.t ();
    v_vec = n_mat * temp_mat.diag ();
}


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                           ITERATE_Q_MAT                            **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graphmp::iterate_q_mat ()
{
    const double eta_inv = 1.0 / return_eta ();
    const arma::rowvec x_row = arma::conv_to <arma::rowvec>::from (x_vec),
          v_row = arma::conv_to <arma::rowvec>::from (v_vec);

    // TODO: Use arma::sum to get row sums and avoid looping over rows?
    // - this would require making matrices of x_vec and v_vec, so may not be
    // any quicker?
    q_mat.replace (0.0, max_weight);
    for (arma::uword r=0; r<q_mat.n_rows; ++r)
    {
        //arma::rowvec temp_row = q_mat.row (r);
        //arma::rowvec temp_row = q_mat.row (r);
        //temp_row.replace (0.0, max_weight);
        //temp_row = arma::exp (-eta_inv * (temp_row + v_row) + x_row);
        arma::rowvec temp_row = arma::exp (-eta_inv * (q_mat.row (r) + 
                    v_row) + x_row);
        const double rsum = arma::sum (temp_row);
        if (rsum > 0.0)
            q_mat.row (r) = temp_row / rsum;
        else
            q_mat.row (r) = temp_row.zeros ();
    }
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                         CALCULATE_Q_MAT                            **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

int Graphmp::calculate_q_mat (double tol, unsigned max_iter)
{
    int nloops = 0; 

    arma::mat q_mat_old;

    double delta = 1.0;
    while (delta > tol & nloops < max_iter)
    {
        q_mat_old = q_mat;
        make_hxv_vecs ();
        iterate_q_mat ();
        delta = arma::accu (arma::abs (q_mat_old - q_mat));
        nloops++;
    }

    return nloops;
}


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                          RCPP_ROUTER                               **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

//' rcpp_router
//'
//' Return OSM data in Simple Features format
//'
//' @param netdf A \code{matrix} containing network connections
//' @param start_node Starting node for shortest path route
//' @param end_node Ending node for shortest path route
//' @param eta The entropy parameter
//'
//' @return Rcpp::List objects of OSM data
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_router (Rcpp::DataFrame netdf, 
        int start_nodei, int end_nodei, double eta)
{
    // Extract vectors from netmat and convert to std:: types
    Rcpp::NumericVector idfrom_rcpp = netdf ["xfr"];
    std::vector <vertex_t> idfrom = 
        Rcpp::as <std::vector <vertex_t> > (idfrom_rcpp);

    Rcpp::NumericVector idto_rcpp = netdf ["xto"];
    std::vector <vertex_t> idto = 
        Rcpp::as <std::vector <vertex_t> > (idto_rcpp);

    Rcpp::NumericVector d_rcpp = netdf ["d"];
    std::vector <weight_t> d = Rcpp::as <std::vector <weight_t> > (d_rcpp);

    const unsigned start_node = (unsigned) start_nodei;
    const unsigned end_node = (unsigned) end_nodei;

    Graphmp g (idfrom, idto, d, start_node, end_node, eta);

    int nloops = g.calculate_q_mat (1.0e-6, 1000000);
    Rcpp::Rcout << "---converged in " << nloops << " loops" << std::endl;
    std::vector <std::string> cnames = {"S", "0", "1", "2", "3", "4", "5", "E"};
    g.dumpMat (g.q_mat, "Q1", cnames);

    std::vector <weight_t> min_distance;
    std::vector <vertex_t> previous;
    g.Dijkstra (start_node, min_distance, previous);

    std::vector <vertex_t> path = g.GetShortestPathTo (end_node, previous);
    // Then fill distances from start to end nodes
    std::vector <weight_t> dout;
    dout.reserve (path.size ());
    for (int i=0; i<path.size (); i++)
        dout.push_back (min_distance [i + 1]);

    Rcpp::NumericMatrix res (path.size (), 2);
    std::copy (path.begin (), path.end (), res.begin ());
    std::copy (dout.begin (), dout.end (), res.begin () + path.size ());

    return res;
}

//' rcpp_router_prob
//'
//' Return a vector of traversing probabilities
//'
//' @param netdf A \code{matrix} containing network connections
//' @param start_node Starting node for shortest path route
//' @param end_node Ending node for shortest path route
//' @param eta The entropy parameter
//'
//' @return Rcpp::NumericVector of traversing probabilities
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_router_prob (Rcpp::DataFrame netdf,
        long long start_node, long long end_node, double eta)
{
    // Extract vectors from netmat and convert to std:: types
    Rcpp::NumericVector idfrom_rcpp = netdf ["xfr"];
    std::vector <vertex_t> idfrom = 
        Rcpp::as <std::vector <vertex_t> > (idfrom_rcpp);

    Rcpp::NumericVector idto_rcpp = netdf ["xto"];
    std::vector <vertex_t> idto = 
        Rcpp::as <std::vector <vertex_t> > (idto_rcpp);

    Rcpp::NumericVector d_rcpp = netdf ["d"];
    std::vector <weight_t> d = Rcpp::as <std::vector <weight_t> > (d_rcpp);

    Graphmp g (idfrom, idto, d, start_node, end_node, eta);

    int nloops = g.calculate_q_mat (1.0e-6, 1000000);
    // q_mat then has to have first row and first col removed
    const int s = g.q_mat.n_rows;
    g.q_mat = g.q_mat.submat (1, 1, s - 1, s - 1);

    // The OSM IDs are then inserted as row and col names, requiring conversion
    // of arma::mat to Rcpp::NumericMatrix
    Rcpp::NumericMatrix q_mat_out = 
        Rcpp::as <Rcpp::NumericMatrix> (Rcpp::wrap (g.q_mat));
    Rcpp::CharacterVector names;
    for (auto i=g.all_nodes.begin (); i != g.all_nodes.end (); ++i)
        names.push_back (std::to_string (*i));
    Rcpp::rownames (q_mat_out) = names;
    Rcpp::colnames (q_mat_out) = names;

    // Finally, convert matrix to single vector matching the pairs of xfr,xto
    Rcpp::NumericVector q_vec;
    for (unsigned i=0; i<idfrom.size (); i++)
    {
        unsigned di = std::distance (g.all_nodes.begin (), 
                g.all_nodes.find (idfrom [i]));
        unsigned dj = std::distance (g.all_nodes.begin (), 
                g.all_nodes.find (idto [i]));
        q_vec.push_back (q_mat_out (di, dj));
    }
    return q_vec;
}

//' rcpp_router_dijkstra
//'
//' Return a vector containing the shortest path between two nodes on a graph
//'
//' @param netdf A \code{data.frame} containing network connections
//' @param start_node Starting node for shortest path route
//' @param end_node Ending node for shortest path route
//'
//' @return \code{Rcpp::NumericVector} with node IDs
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_router_dijkstra (Rcpp::DataFrame netdf, 
        int start_node, int end_node)
{
    // Extract vectors from netmat and convert to std:: types
    Rcpp::NumericVector idfrom_rcpp = netdf ["from_id"];
    std::vector <vertex_t> idfrom = 
        Rcpp::as <std::vector <vertex_t> > (idfrom_rcpp);

    Rcpp::NumericVector idto_rcpp = netdf ["to_id"];
    std::vector <vertex_t> idto = 
        Rcpp::as <std::vector <vertex_t> > (idto_rcpp);

    Rcpp::NumericVector d_rcpp = netdf ["d_weighted"];
    std::vector <weight_t> d = Rcpp::as <std::vector <weight_t> > (d_rcpp);

    const unsigned start_nodei = (unsigned) start_node;
    const unsigned end_nodei = (unsigned) end_node;

    Graphmp g (idfrom, idto, d, start_nodei, end_nodei);

    std::vector <weight_t> min_distance;
    std::vector <vertex_t> previous;
    g.Dijkstra (start_nodei, min_distance, previous);

    std::vector <vertex_t> path = g.GetShortestPathTo (end_nodei, previous);
    return Rcpp::wrap (path);
}
