all:
	ocamlbuild TP2.native
	mv TP2.native TP2
clean:
	ocamlbuild -clean
