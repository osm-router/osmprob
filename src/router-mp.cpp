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

void Graph::make_dq_mats ()
{
    /* the diagonal of d_mat is 0, otherwise the first row contains only one
     * finite entry for escape from start_node. The last column similarly
     * contains only one finite entry for absorption by end_node. */
    const unsigned num_vertices = return_num_vertices ();
    const unsigned start_node = return_start_node ();
    const unsigned end_node = return_end_node ();

    d_mat = arma::mat (num_vertices + 1, num_vertices + 1);
    d_mat.fill (max_weight);
    d_mat.diag (0.0);
    q_mat.zeros (num_vertices + 1, num_vertices + 1);

    unsigned q_sums [num_vertices]; // Note: 1 shorter than q_mat

    for (int i=0; i<num_vertices; i++)
    {
        q_sums [i] = 0;
        const std::vector <neighbor> &nbs = adjlist [i];
        for (std::vector <neighbor>::const_iterator nb_iter = nbs.begin ();
                nb_iter != nbs.end (); nb_iter++)
        {
            d_mat (i + 1, nb_iter->target + 1) = nb_iter->weight;
            q_mat (i + 1, nb_iter->target + 1) = 1.0;
            q_sums [i]++;
        }
    }
    d_mat (0, start_node + 1) = 1.0;

    // Standardise q_mat, which is the top-left of the probability matrix
    for (arma::uword r=1; r<q_mat.n_rows; ++r)
        q_mat.row (r) = q_mat.row (r) / (double) q_sums [r - 1];
    // Then add links to start_node and to absorbing end_node
    q_mat (0, start_node + 1) = 1.0;
    q_mat.row (end_node + 1) = q_mat.row (end_node + 1) * 
        q_sums [end_node] / (q_sums [end_node] + 1.0);
}


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             MAKE_N_MAT                             **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graph::make_n_mat ()
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

void Graph::make_hxv_vecs ()
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

void Graph::iterate_q_mat ()
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
        q_mat.row (r) = temp_row / arma::sum (temp_row);
    }
}

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                         CALCULATE_Q_MAT                            **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

int Graph::calculate_q_mat (double tol, unsigned max_iter)
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
//' @param netmat A \code{matrix} containing network connections
//' @param start_node Starting node for shortest path route
//' @param end_node Ending node for shortest path route
//' @param eta The entropy parameter
//'
//' @return Rcpp::List objects of OSM data
//'
//' @noRd
// [[Rcpp::export]]
void rcpp_router (Rcpp::DataFrame netdf, 
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

    Graph g (idfrom, idto, d, start_node, end_node, eta);

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

    //return res;
}
