library (osmdatar)

addTest (2, 4)

get_routes <- function (bbox, from, to)
{
    roads <- osmdatar::get_lines (bbox, key='highway')
}
