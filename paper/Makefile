MAIN=lc_main
ENGINE=pdflatex

all:
	latexmk -pdf -interaction=nonstopmode -file-line-error $(MAIN).tex

clean:
	latexmk -c

distclean:
	latexmk -C
	rm -f $(MAIN).pdf

