# Taken from https://github.com/yzyzsun/CP-next/blob/main/ott/Makefile

OTTFILES = fiplus.ott compilation.ott
DOCNAME  = doc

$(DOCNAME).pdf: $(DOCNAME).mng $(OTTFILES)
	ott $(OTTFILES) -o ott.tex \
		-tex_wrap false -tex_show_meta false \
		-tex_filter $(DOCNAME).mng $(DOCNAME).tex
	
.PHONY: clean
clean:
	latexmk -c
	rm -f *.tex