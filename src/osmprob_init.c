#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _osmprob_rcpp_lines_as_network(SEXP, SEXP);
extern SEXP _osmprob_rcpp_make_compact_graph(SEXP, SEXP);
extern SEXP _osmprob_rcpp_router(SEXP, SEXP, SEXP, SEXP);
extern SEXP _osmprob_rcpp_router_dijkstra(SEXP, SEXP, SEXP);
extern SEXP _osmprob_rcpp_router_prob(SEXP, SEXP, SEXP, SEXP);


static const R_CallMethodDef CallEntries[] = {
    {"_osmprob_rcpp_lines_as_network",   (DL_FUNC) &_osmprob_rcpp_lines_as_network,   2},
    {"_osmprob_rcpp_make_compact_graph", (DL_FUNC) &_osmprob_rcpp_make_compact_graph, 2},
    {"_osmprob_rcpp_router",             (DL_FUNC) &_osmprob_rcpp_router,             4},
    {"_osmprob_rcpp_router_dijkstra",    (DL_FUNC) &_osmprob_rcpp_router_dijkstra,    3},
    {"_osmprob_rcpp_router_prob",        (DL_FUNC) &_osmprob_rcpp_router_prob,        4},
    {NULL, NULL, 0}
};

void R_init_osmprob(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
