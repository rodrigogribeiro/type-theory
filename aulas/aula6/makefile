default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula6.tex
	pdflatex aula6.tex
	pdflatex aula6.tex

clean:
	rm aula6.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
