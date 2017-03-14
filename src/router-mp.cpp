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

#include <string>
#include <Rcpp.h>

/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             FILLGRAPH                              **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

void Graph::fillGraph ()
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
        }
    }
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

    Graph g (idfrom, idto, d);

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
