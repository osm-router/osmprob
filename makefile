RFILE = aaa

all: knith open 

knith: $(RFILE).Rmd
	echo "rmarkdown::render('$(RFILE).Rmd',output_file='$(RFILE).html')" | R --no-save -q

knitr: $(RFILE).Rmd
	echo "rmarkdown::render('$(RFILE).Rmd',rmarkdown::md_document(variant='markdown_github'))" | R --no-save -q

open: $(RFILE).html
	xdg-open $(RFILE).html &

clean:
	rm -rf *.html *.png README_cache man/rcpp_get_points.Rd \
		man/rcpp_get_lines.Rd man/rcpp_get_polygons.Rd
