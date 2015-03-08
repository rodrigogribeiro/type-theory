default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o ts-lista1.tex
	pdflatex ts-lista1.tex
	pdflatex ts-lista1.tex

clean:
	rm *.log
	rm *.aux
	rm *.ptb
	rm *.pdf
	rm *.tex
