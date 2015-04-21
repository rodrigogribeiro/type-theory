default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula9.tex
	pdflatex aula9.tex
	pdflatex aula9.tex

clean:
	rm aula9.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
