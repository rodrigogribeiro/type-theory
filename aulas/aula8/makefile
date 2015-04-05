default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula8.tex
	pdflatex aula8.tex
	pdflatex aula8.tex

clean:
	rm aula8.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
