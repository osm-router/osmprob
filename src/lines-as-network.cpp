/***************************************************************************
 *  Project:    osmdata
 *  File:       lines-as-network.cpp
 *  Language:   C++
 *
 *  osmdata is free software: you can redistribute it and/or modify it under
 *  the terms of the GNU General Public License as published by the Free
 *  Software Foundation, either version 3 of the License, or (at your option)
 *  any later version.
 *
 *  osmdata is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *  details.
 *
 *  You should have received a copy of the GNU General Public License along with
 *  osm-router.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  Author:     Mark Padgham 
 *  E-Mail:     mark.padgham@email.com 
 *
 *  Description:    Convert sf linestring collection to data.frame of network
 *                  connections
 *
 *  Limitations:
 *
 *  Dependencies:       none (rapidXML header included in osmdatar)
 *
 *  Compiler Options:   -std=c++11
 ***************************************************************************/

#include <string>
#include <cmath>

#include <Rcpp.h>

// Haversine great circle distance between two points
float haversine (float x1, float y1, float x2, float y2)
{
    float xd = (x2 - x1) * M_PI / 180.0;
    float yd = (y2 - y1) * M_PI / 180.0;
    float d = sin (yd / 2.0) * sin (yd / 2.0) + cos (y2 * M_PI / 180.0) *
        cos (y1 * M_PI / 180.0) * sin (xd / 2.0) * sin (xd / 2.0);
    d = 2.0 * 3671.0 * asin (sqrt (d));
    return (d);
}

//' rcpp_lines_as_network
//'
//' Return OSM data in Simple Features format
//'
//' @param sf_lists An sf collection of LINESTRING objects
//'
//' @return Rcpp::List objects of OSM data
// [[Rcpp::export]]
Rcpp::List rcpp_lines_as_network (const Rcpp::List &sf_lines)
{

    size_t n = sf_lines.size ();
    Rcpp::CharacterVector nms = sf_lines.attr ("names");
    if (nms [nms.size () - 1] != "geometry")
        throw std::runtime_error ("sf_lines have no geometry component");
    if (nms [0] != "osm_id")
        throw std::runtime_error ("sf_lines have no osm_id component");

    Rcpp::List geoms = sf_lines [nms.size () - 1];
    // First get dimension of matrix, and store vector of sizes of each
    // linestring object
    std::vector <int> sizes;
    size_t nrows = 0;
    for (auto g = geoms.begin (); g != geoms.end (); ++g)
    {
        // Rcpp uses an internal proxy iterator here, NOT a direct copy
        Rcpp::NumericMatrix gi = (*g);
        nrows += gi.nrow () - 1;
        sizes.push_back (gi.nrow ());
    }

    Rcpp::NumericMatrix nmat = Rcpp::NumericMatrix (Rcpp::Dimension (nrows, 5));
    Rcpp::CharacterMatrix idmat = Rcpp::CharacterMatrix (Rcpp::Dimension (nrows, 2));
    Rcpp::CharacterVector colnames (nrows);
    nrows = 0;
    for (auto g = geoms.begin (); g != geoms.end (); ++g)
    {
        Rcpp::NumericMatrix gi = (*g);

        Rcpp::List ginames = gi.attr ("dimnames");
        Rcpp::CharacterVector rnms = ginames [0];
        if (rnms.size () != gi.nrow ())
            throw std::runtime_error ("geom size differs from rownames");

        for (int i=1; i<gi.nrow (); i++)
        {
            nmat (nrows, 0) = gi (i-1, 0);
            nmat (nrows, 1) = gi (i-1, 1);
            nmat (nrows, 2) = gi (i, 0);
            nmat (nrows, 3) = gi (i, 1);
            nmat (nrows, 4) = haversine (gi (i-1, 0), gi (i-1, 1),
                    gi (i, 0), gi (i, 1));
            idmat (nrows, 0) = rnms (i-1);
            idmat (nrows, 1) = rnms (i);
            nrows++;
        }
    }

    Rcpp::List res (2);
    res [0] = nmat;
    res [1] = idmat;

    return res;
}