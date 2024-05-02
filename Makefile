.PHONY : install gen pdf bib html 

AGDA2HS = agda2hs

install :
	cabal install --overwrite-policy=always

gen : 
	$(AGDA2HS) --config=rewrite-rules.yml -o gen src/Formalize/Test.agda

pdf : latex/report.pdf

bib : latex/report.tex
	cd latex && biber report && lualatex report

html : html/logbook.html

html/logbook.md : logbook.lagda.md
	agda-llvm --html --html-highlight=auto logbook.lagda.md

html/logbook.html : html/logbook.md
	pandoc -s html/logbook.md -o html/logbook.html --from markdown+tex_math_dollars+tex_math_double_backslash+latex_macros+citations+implicit_header_references --citeproc

latex/report.tex : report.lagda.tex
	agda-llvm --latex report.lagda.tex

latex/report.pdf : latex/report.tex
	cd latex && lualatex report

# EOF
