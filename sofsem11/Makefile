pdf: savonet.pdf

savonet.pdf: $(wildcard *tex)
	if [ -f /usr/bin/rubber ]; then rubber -d -W refs savonet; else pdflatex savonet; fi

dist:
	mkdir sofsem11
	cp biblio.bib clocks.tex hetero.tex llncs.cls savonet.tex stream.pdf sofsem11/
	cd sofsem11 && zip -r ../sofsem11.zip *
	rm -rf sofsem11

clean:
	rubber -d --clean savonet.tex
