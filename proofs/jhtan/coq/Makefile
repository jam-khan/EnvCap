COQMKFILENAME=CoqSrc.mk

coq: $(COQMKFILENAME) $(shell ls *.v)
	@$(MAKE) -f CoqSrc.mk -

%.vo: %.v
	@$(MAKE) -f CoqSrc.mk $*.vo

$(COQMKFILENAME): Makefile $(shell ls *.v)
	coq_makefile -arg '-w -variable-collision,-meta-collision,-require-in-module' -f _CoqProject -o $(COQMKFILENAME)

coqclean:
	rm -f .*.d *.conf .*.aux *.v-e *.v.d *.vo *.vos *.vio *.vok *.glob .lia.cache $(COQMKFILENAME)

clean: coqclean
