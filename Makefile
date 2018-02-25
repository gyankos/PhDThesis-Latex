all:
	java -cp jbTeX3.jar it.giacomobergami.jbTeX3.Main
pics:
	./compileimgs.py
world:	pics all
	java -cp jbTeX3.jar it.giacomobergami.jbTeX3.Main
missing:
	texlua checkcites.lua --undefined main.aux
clean:
	rm -rf rm *.aux *.blg *.log *.mtc* *.toc *.maf *.bbl *.out *.gz *.nav *.snm
cleanall: clean
	./clean_pics.py
slides:
	pandoc -f markdown+raw_tex+grid_tables -t beamer -o presentation.pdf thesis_presentation.md -V theme:metropolis -H thesis_presentation.conf
	pdfnup presentation.pdf --nup 1x2 --no-landscape --keepinfo --paper letterpaper --frame true --scale 0.9 --suffix "handout"
present:
	pdfpc presentation.pdf
