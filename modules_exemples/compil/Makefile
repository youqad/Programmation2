default: monprog.byte monprog.opt

ML = a.ml b.ml
BYTE = $(ML:.ml=.cmo)
OPT = $(ML:.ml=.cmx)

monprog.byte: $(BYTE)
	ocamlc $(BYTE) -o $@

monprog.opt: $(OPT)
	ocamlopt $(OPT) -o $@

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

%.cmx: %.ml
	ocamlopt -c $<

-include .depend
.depend: $(wildcard *.ml *.mli)
	ocamldep $(wildcard *.ml *.mli) > .depend

.PHONY: clean
clean:
	rm -f monprog.* *.cmo *.cmi *.cmx *.o .depend
