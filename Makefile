.PHONY : install test clean

install :
	cabal install --overwrite-policy=always

test :
	cabal run test

test-clean :
	rm test/*.err

run : 
	agda-llvm --llvm agda-programs/DownFrom.agda

html : html/logbook.html

pdf : latex/report.pdf

bib : latex/report.tex
	cd latex && biber report && lualatex report

clean : 
	rm -rf html
	rm -rf latex

html/logbook.md : logbook.lagda.md
	agda-llvm --html --html-highlight=auto logbook.lagda.md

html/logbook.html : html/logbook.md
	pandoc -s html/logbook.md -o html/logbook.html --from markdown+tex_math_dollars+tex_math_double_backslash+latex_macros+citations+implicit_header_references --citeproc

latex/report.tex : report.lagda.tex
	agda-llvm --latex report.lagda.tex

latex/report.pdf : latex/report.tex
	cd latex && lualatex report

# EOF
