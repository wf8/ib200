file="lab09"
pdflatex $file.tex
bibtex $file
pdflatex $file.tex
pdflatex $file.tex
rm $file.bib
rm $file.aux
rm $file.bcf
rm $file.log
rm $file.out
rm $file.run.xml
rm $file.bbl
rm $file.blg
