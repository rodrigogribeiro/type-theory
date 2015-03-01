default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula2.tex
	pdflatex aula2.tex
	pdflatex aula2.tex

clear:
	rm *.tex
	rm *.pdf
	rm *.log
