
all: strings test notest

strings: strings.cmo
	ocamlc $< -o $@

test: file.cmo test.cmo
	ocamlc unix.cma file.cmo test.cmo -o $@

notest: file.cmo $(wildcard notest*.ml)
	for i in notest*.ml ; do \
	  if ocamlc -c $$i 2> /dev/null ; then \
	    echo "$$i: ERREUR" ; exit 1 ; \
	  else \
	    echo "$$i: OK" ; \
	  fi \
	done ;
	touch notest

-include .depend
.depend: $(wildcard *.ml) $(wildcard *.mli)
	ocamldep $(wildcard *.ml) $(wildcard *.mli) > .depend

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

clean:
	rm -f *.cmi *.cmo

tarball:
	cd .. && tar zcvf phantom/phantom.tar.gz \
	  phantom/Makefile \
	  phantom/file.ml phantom/file.mli phantom/test.ml \
	  phantom/notest1.ml phantom/notest2.ml \
	  phantom/notest3.ml phantom/notest4.ml \
	  phantom/strings.ml phantom/strings.mli
