MD_SOURCE = $(wildcard engine/R/*.md)

all: 01.grammer.html

01.gramar.html: .R $(MD_SOURCE)
	Rscript -e "\
    parser('.R')"
