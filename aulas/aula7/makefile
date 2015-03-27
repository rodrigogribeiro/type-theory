default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula7.tex
	pdflatex aula7.tex
	pdflatex aula7.tex

clean:
	rm aula7.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
