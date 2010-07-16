pdf: savonet.pdf

savonet.pdf: savonet.tex
	if [ -f /usr/bin/rubber ]; then rubber -d $^; else pdflatex $^; fi

clean:
	rubber -d --clean savonet.tex
