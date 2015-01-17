# Makefile to use knitr for package vignettes

# put all PDF targets here, separated by spaces
PDFS= datatable-faq.pdf datatable-intro.pdf datatable-intro-vignette.html

all: $(PDFS) 

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.toc *.log *.spl *tikzDictionary *.md figure/

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "tools::texi2pdf('$*.tex')"

%.html: %.Rmd
	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2html('$*.Rmd')"
