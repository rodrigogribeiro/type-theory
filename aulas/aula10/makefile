default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula10.tex
	pdflatex aula10.tex
	pdflatex aula10.tex

clean:
	rm aula10.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
