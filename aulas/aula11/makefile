default: pdf

pdf:
	lhs2TeX --haskell Main.lhs -o aula11.tex
	pdflatex aula11.tex
	pdflatex aula11.tex

clean:
	rm aula11.tex
	rm *.pdf
	rm *.log
	rm *.nav
	rm *.aux
	rm *.snm
	rm *.toc
	rm *.out
	rm *.ptb
