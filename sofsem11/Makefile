pdf: savonet.pdf talk.pdf

dist:
	mkdir sofsem11
	cp biblio.bib clocks.tex hetero.tex llncs.cls savonet.tex stream.pdf sofsem11/
	cd sofsem11 && zip -r ../sofsem11.zip *
	rm -rf sofsem11

clean:
	rubber -d --clean savonet.tex

%.pdf: %.tex
	rubber -d $<
