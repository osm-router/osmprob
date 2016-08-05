get_routes <- function (bbox, from, to)
{
    roads <- osmdatar::get_lines (bbox, key='highway')
}
