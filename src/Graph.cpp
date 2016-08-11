#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::DataFrame makeCompactGraph (Rcpp::DataFrame graph)
{
    Rcpp::NumericVector vec = graph[0];
    int l = vec.length ();
    for (int i = 0; i < l; i++)
    {
        std::cout << "element: " << vec[i] << std::endl;
    }
    std::cout << "length: " << l << std::endl;
    return graph;
}
