#include <Rcpp.h>
#include <cmath>

//' getDist
//'
//' Calculates the total distance in km between all sequential points in xAll
//' and yAll
//'
//' @param xAll vector containing all latitudes sequentially
//' @param yAll vector containing all longitudes sequentially
//' @return the total distance between a and b in km
// [[Rcpp::export]]
double getDist (Rcpp::NumericVector xAll, Rcpp::NumericVector yAll)
{
    double x, y, xa, xb, ya, yb, dist, total = 0;
    for (int i = 0; i < xAll.length () - 1; i ++)
    {
        xa = xAll [i];
        xb = xAll [i + 1];
        ya = yAll [i];
        yb = yAll [i + 1];
        x = (xa - xb) * PI / 180.0;
        y = (ya - yb) * PI / 180.0;
        dist = std::sin (y / 2.0) * std::sin (y / 2.0) + std::cos (ya * PI / 180.0) *
            std::cos (yb * PI / 180.0) * sin (x / 2.0) * std::sin (x / 2.0);
        dist = 2.0 * std::atan2 (sqrt (dist), sqrt (1.0 - dist));
        total += dist;
    }
    total *= 6371.0;
    return total;
};
