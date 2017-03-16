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

// [[Rcpp::depends(RcppArmadillo)]]

#include "router-mp.h"
#include <RcppArmadillo.h> // automatically loads Rcpp.h


void dump_mat (std::vector <std::string> strs, unsigned nspaces, arma::mat * mat,
        double null_value, unsigned precision)
{
    if (mat->n_rows != mat->n_cols)
        throw std::runtime_error ("matrix is not square");
    if (strs.size () != mat->n_rows)
        throw std::runtime_error ("row/col labels not same size as matrix");

    Rcpp::Rcout << "     ";
    for (auto i : strs)
    {
        Rcpp::Rcout << i;
        for (int j=0; j<nspaces; j++) Rcpp::Rcout << " ";
    }
    Rcpp::Rcout << std::endl << "    ";
    for (int i=0; i<(strs.size () * (nspaces + 1)); i++)
        Rcpp::Rcout << "-";
    Rcpp::Rcout << std::endl;
    for (arma::uword r=0; r < mat->n_rows; ++r)
    {
        Rcpp::Rcout << " " << strs [r] << "| ";
        for (auto it = mat->begin_row (r); 
                it != mat->end_row (r); ++it)
        {
            if ((*it) == null_value)
            {
                Rcpp::Rcout << " -";
                for (int i=0; i<(nspaces - 1); i++) Rcpp::Rcout << " ";
            } else
            {
                if (precision > 0)
                    Rcpp::Rcout << std::fixed << std::setprecision (2) << 
                        (*it) << " ";
                else
                    Rcpp::Rcout << std::setw (2) << (*it) << " ";
            }
        }
        Rcpp::Rcout << std::endl;
    }
    Rcpp::Rcout << std::endl;
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
//' @param netdf \code{data.frame} containing network connections
//' @param start_node Starting node for shortest path route
//' @param end_node Ending node for shortest path route
//'
//' @return Rcpp::List objects of OSM data
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_router (Rcpp::DataFrame netdf, 
        int start_node, int end_node)
{
    // Extract vectors from netdf and convert to std:: types
    Rcpp::NumericVector idfrom_rcpp = netdf ["xfr"];
    std::vector <vertex_t> idfrom = 
        Rcpp::as <std::vector <vertex_t> > (idfrom_rcpp);

    Rcpp::NumericVector idto_rcpp = netdf ["xto"];
    std::vector <vertex_t> idto = 
        Rcpp::as <std::vector <vertex_t> > (idto_rcpp);

    Rcpp::NumericVector d_rcpp = netdf ["d"];
    std::vector <weight_t> d = Rcpp::as <std::vector <weight_t> > (d_rcpp);

    Graph g (idfrom, idto, d, start_node, end_node);

    Rcpp::Rcout << "----- prob matrix#0 -----" << std::endl;
    std::vector <std::string> rnames = {"S", "0", "1", "2", "3", "4", "5", "E"};
    dump_mat (rnames, 4, &g.p_mat, 0.0, 2);

    for (int i=0; i<3; i++)
    {
        g.calc_h_vec ();
        g.calc_n_mat ();
        g.iterate_pq_mats ();

        rnames = {"S", "0", "1", "2", "3", "4", "5", "E"};
        Rcpp::Rcout << "----- prob matrix#" << i + 1 << " -----" << std::endl;
        dump_mat (rnames, 4, &g.p_mat, 0.0, 2);
    }

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
    std::copy (dout.begin (), dout.end (), path.size () + res.begin ());

    return res;
}
