library (devtools)
library (Rcpp)

#compile
setwd ("~/master_thesis/osmprob/")
Rcpp::compileAttributes (".", verbose=TRUE)

#test
setwd ("~/master_thesis/")

devtools::check ("osmprob")
devtools::load_all ("osmprob")
