default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula2.tex
	pdflatex aula2.tex
	pdflatex aula2.tex

clean:
	rm aula2.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
