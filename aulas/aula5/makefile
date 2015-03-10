default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula5.tex
	pdflatex aula5.tex
	pdflatex aula5.tex

clean:
	rm aula5.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
