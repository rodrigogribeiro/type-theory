default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula4.tex
	pdflatex aula4.tex
	pdflatex aula4.tex

clean:
	rm aula4.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
