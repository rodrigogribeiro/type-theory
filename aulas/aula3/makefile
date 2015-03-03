default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula3.tex
	pdflatex aula3.tex
	pdflatex aula3.tex

clean:
	rm aula3.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
