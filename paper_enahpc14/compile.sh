#/bin/bash -l
./clean.sh
latex draft.tex
bibtex draft.aux
latex draft.tex
latex draft.tex
dvips -o draft.ps draft.dvi
ps2pdf -sPAPERSIZE=a4 draft.ps
acroread draft.pdf
