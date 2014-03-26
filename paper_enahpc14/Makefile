SRC 	= paper
REP 	= ./Figs
LATEX 	= latex
BIBTEX 	= bibtex -terse

.PHONY: all figs $(REP) clean realclean dvi ps pdf svn commit tarball

all: realclean $(REP) dvi ps pdf

$(REP):
	@$(MAKE) -C $@ $(MAKECMDGOALS)

clean: $(REP)
	@echo "LaTeX compilation files deletion"
	@rm -vf *.bak *.idx *.out *~ *.log *.aux *.dvi *.toc *.lot *.lof
	@echo "BibTeX files deletion"
	@rm -vf *.bbl *.blg

realclean: clean
	@echo "Target files deletion"
	@rm -vf *.eps *.ps *.pdf

dvi: $(SRC).dvi

ps : $(SRC).ps 

pdf : $(SRC).pdf

svn:
	svn commit

commit: all clean svn

%.dvi : %.tex
	@echo "LaTeX compilation"
	@${LATEX} $<
	@if [ -e ${SRC}.bib ]; then ${BIBTEX} $* ; fi
	@${LATEX} $<
	@${LATEX} $<

%.ps : %.dvi
	dvips -q -t a4 $< -o $(<:.dvi=.ps)

%.pdf : %.ps
	ps2pdf -sPAPERSIZE=a4 -dPDFSETTINGS=/prepress $<

