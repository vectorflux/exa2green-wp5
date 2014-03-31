#
# LaTeX Makefile Invocation
# -------------------------
#
# You can use the following commands:
#
# - make help: 		Prints an overview of the available commands
# - make all:		Generates the DVI, PS and PDF files
# - make dvi:		Generates the DVI file
# - make ps:		Generates the PS file
# - make pdf:		Generates the PDF file
# - make clean:		Removes all generated output excluding EPS, PS and PDF files
# - make realclean: 	Removes all generated output including EPS, PS and PDF files

SRC 	= paper
FIGDIR	= ./Figs
LATEX 	= latex
BIBTEX 	= bibtex -terse

.PHONY: all $(FIGDIR) clean realclean dvi ps pdf

all: realclean $(FIGDIR) dvi ps pdf

$(FIGDIR):
	@$(MAKE) -C $@ $(MAKECMDGOALS)

help:
	@echo ""
	@echo "LaTeX Makefile Options"
	@echo "----------------------"
	@echo "Environment variables:"
	@echo "  SRC		Filename to make (without extension)"
	@echo "  FIGDIR	Directory to store figures that have to be compiled"
	@echo "  LATEX		Implementation of TeX/LaTeX"	
	@echo "  BIBTEX	Reference management software"	
	@echo ""
	@echo "Targets:"
	@echo "  help		Prints this overview"
	@echo "  all		Generates the DVI, PS and PDF files"
	@echo "  dvi		Generates the DVI file"
	@echo "  ps		Generates the PS file"
	@echo "  pdf		Generates the PDF file"
	@echo "  clean		Removes all generated output excluding EPS, PS and PDF files"
	@echo "  realclean	Removes all generated output including EPS, PS and PDF files"
	@echo ""

clean: $(FIGDIR)
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

