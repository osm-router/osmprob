#library (devtools)
#library (Rcpp)

#compile
wd <- getwd ()
while (length (grep ('osmprob', getwd ())) > 0) setwd ("..")
#setwd ("~/master_thesis/osmprob/")
#Rcpp::compileAttributes (".", verbose=TRUE)
Rcpp::compileAttributes ("osmprob", verbose=TRUE)

#test
#setwd ("~/master_thesis/")

devtools::document ('osmprob')
#devtools::load_all ("osmprob", export_all=FALSE)
devtools::load_all ("osmprob")
#devtools::check ("osmprob")

ls ("package:osmprob")
?add_test
add_test (1, 2)
