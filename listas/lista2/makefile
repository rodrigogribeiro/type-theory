default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o ts-lista2.tex
	pdflatex ts-lista2.tex
	pdflatex ts-lista2.tex

clean:
	rm *.log
	rm *.aux
	rm *.ptb
	rm *.pdf
	rm *.tex
