pdf: savonet.pdf

savonet.pdf: $(wildcard *tex)
	if [ -f /usr/bin/rubber ]; then rubber -d savonet; else pdflatex savonet; fi

clean:
	rubber -d --clean savonet.tex
