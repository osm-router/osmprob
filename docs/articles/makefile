RFILE = osmprob

all: knith open 

knith: $(RFILE).Rmd
	echo "rmarkdown::render('$(RFILE).Rmd',output_file='$(RFILE).html')" | R --no-save -q

open: $(RFILE).html
	xdg-open $(RFILE).html &

clean:
	rm -rf *.html
