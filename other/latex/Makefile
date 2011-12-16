FILEPREFIX = put_the_file_prefix_here

all:latexopen

latex:
	pdflatex $(FILEPREFIX).tex
	pdflatex $(FILEPREFIX).tex

latexopen: latex open

pdf: nonstopmodeopen

open:
	open -a Skim $(FILEPREFIX).pdf

nonstopmode:
	pdflatex -interaction nonstopmode $(FILEPREFIX).tex
	pdflatex -interaction nonstopmode $(FILEPREFIX).tex

nonstopmodeopen: nonstopmode open

clean:
	-rm -f *.blg *.lof *.log *.lot *.pdf *.toc *.bbl *.aux *glg *.glo *.gls *.ist *.acn *.acr *.alg *.out
