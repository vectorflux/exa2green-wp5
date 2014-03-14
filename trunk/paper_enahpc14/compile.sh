#/bin/bash -l
./clean.sh
latex template.tex
dvips -o template.ps template.dvi
ps2pdf -sPAPERSIZE=a4 template.ps
acroread template.pdf
