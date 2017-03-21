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

#include <Rcpp.h>


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
