# Makefile to use knitr for package vignettes

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.toc *.log *.spl *tikzDictionary *.md figure/

%.html: %.Rmd
	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2html('$*.Rmd')"
