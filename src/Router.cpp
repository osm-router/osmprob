#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int addTest (int a, int b)
{
    int res = a + b;
    return res;
}
