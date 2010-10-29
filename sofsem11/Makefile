pdf: savonet.pdf

savonet.pdf: $(wildcard *tex)
	if [ -f /usr/bin/rubber ]; then rubber -d -W refs savonet; else pdflatex savonet; fi

dist:
	mkdir sofsem11
	cp biblio.bib clocks.tex hetero.tex llncs.cls savonet.tex stream.pdf sofsem11/
	tar zcvf sofsem11.tar.gz sofsem11
	rm -rf sofsem11

clean:
	rubber -d --clean savonet.tex
