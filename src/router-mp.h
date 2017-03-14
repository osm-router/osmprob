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



#include <unordered_map> // TODO: Delete!


#include <iostream>
#include <vector>
#include <string>
#include <list>

#include <limits> // for numeric_limits

#include <set>
#include <utility> // for pair
#include <algorithm>
#include <iterator>

const float FLOAT_MAX = std::numeric_limits <float>::max ();

typedef int vertex_t;
typedef double weight_t;

struct neighbor {
    vertex_t target;
    weight_t weight;
    neighbor (vertex_t arg_target, weight_t arg_weight)
        : target (arg_target), weight (arg_weight) { }
};

typedef std::vector <std::vector <neighbor> > adjacency_list_t;

const weight_t max_weight = std::numeric_limits <double>::infinity();

class Graph
{
    std::unordered_map <char, const std::unordered_map <char, float> > vertices;

    protected:
        const std::vector <vertex_t> _idfrom, _idto;
        const std::vector <weight_t> _d;

    public:
        adjacency_list_t adjlist; // the graph data

        Graph (std::vector <vertex_t> idfrom, std::vector <vertex_t> idto,
                std::vector <weight_t> d)
            : _idfrom (idfrom), _idto (idto), _d (d)
        {
            fillGraph (); // fills adjlist with (idfrom, idto, d)
        }
        ~Graph ()
        {
            for (int i=0; i<adjlist.size (); i++)
                adjlist [i].clear ();
            adjlist.clear ();
        }

        std::vector <vertex_t> return_idfrom() { return _idfrom; }
        std::vector <vertex_t> return_idto() { return _idto; }
        std::vector <weight_t> return_d() { return _d; }

        void fillGraph ();
        void Dijkstra (vertex_t source, 
                std::vector <weight_t> &min_distance,
                std::vector <vertex_t> &previous);
        std::vector <vertex_t> GetShortestPathTo (vertex_t vertex, 
                const std::vector <vertex_t> &previous);
};


