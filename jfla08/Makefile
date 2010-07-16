FIG = transitions.tex transitions-out.tex transitions-out-in.tex
FIGPDF = $(FIG:.tex=.pdf)

pdf: baelde.pdf

baelde.pdf: baelde.tex $(FIGPDF)
#	if [ -f /usr/bin/rubber ]; then rubber -d $^; else pdflatex $^; fi
	pdflatex $^

figures: $(FIGPDF)

clean:
	rubber -d --clean baelde.tex
	rm -f $(FIGPDF)

figures: $(FIGPDF)

%.pdf: %.tex
	pdflatex $^
	pdfcrop $@ $@

dist: pdf
	mkdir -p baelde
	cp Makefile actes.sty baelde.tex baelde.bib baelde.bbl baelde.pdf $(FIG) $(FIGPDF) baelde
	tar zcvf baelde.tar.gz baelde
	rm -rf baelde

.PHONY: baelde.pdf
