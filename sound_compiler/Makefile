TEX = $(wildcard *.tex)
PDF = $(TEX:.tex=.pdf)

all: $(PDF)

clean:
	for i in $(TEX); do rubber --clean $$i; done
	rm -f $(PDF)

%.pdf: %.tex
	rubber -d $^
