
.PHONY : sdist clean

html/logbook.md : logbook.lagda.md
	agda --html --html-highlight=auto logbook.lagda.md

html/logbook.html : html/logbook.md
	pandoc -s html/logbook.md -o html/logbook.html --from markdown+tex_math_dollars+tex_math_double_backslash+latex_macros+citations --citeproc

html : html/logbook.html

latex/report.tex : report.lagda.tex
	agda --latex report.lagda.tex


latex/report.pdf : latex/report.tex
	pdflatex -output-directory=latex latex/report.tex

pdf : latex/report.pdf

clean : 
	rm -rf html
	rm -rf latex

# EOF
