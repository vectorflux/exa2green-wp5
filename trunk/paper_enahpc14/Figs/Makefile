FICHIERS_GNUPLOT = $(wildcard *.plt)
FICHIERS_PDF = $(patsubst %.plt,%.pdf,  $(FICHIERS_GNUPLOT))

.PHONY: all clean realclean

all: $(FICHIERS_PDF)

clean: 	
	@echo "PDF files deletion"
	@rm -vf *.pdf *~

realclean: clean

%.eps: %.plt
	@echo "compilation of "$<
	@gnuplot $<;

%.pdf: %.eps
	@echo "conversion of "$*".eps"
	@epstopdf $*.eps 
