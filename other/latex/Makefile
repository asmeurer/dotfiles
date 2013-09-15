FILEPREFIX = put_the_file_prefix_here

.PHONY : all latex latexopen pdf open nonstopmode nonstopmodeopen clean

all:latex

latex:
	pdflatex $(FILEPREFIX).tex
	pdflatex $(FILEPREFIX).tex

latexopen: latex open

latexopenopen: latex open openiterm

pdf: nonstopmodeopen

open:
	open -a Skim $(FILEPREFIX).pdf

openiterm:
	open -a iTerm\ 2

nonstopmode:
	pdflatex -interaction nonstopmode $(FILEPREFIX).tex
	pdflatex -interaction nonstopmode $(FILEPREFIX).tex

nonstopmodeopen: nonstopmode open

clean:
	-rm -f *.blg *.lof *.log *.lot *.pdf *.toc *.bbl *.aux *glg *.glo *.gls *.ist *.acn *.acr *.alg *.out
