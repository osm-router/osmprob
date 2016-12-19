RFILE = README
SFILE = runTest

all: knith open 

knith: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).html')" | R --no-save -q

knitr: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',rmarkdown::md_document(variant='markdown_github'))" | R --no-save -q

open: $(LFILE).html
	xdg-open $(LFILE).html &

clean:
	rm -rf *.html *.png README_cache man/rcpp_get_points.Rd \
		man/rcpp_get_lines.Rd man/rcpp_get_polygons.Rd

test: $(SFILE).R
	Rscript -e "source ('$(SFILE).R')"
