all:
	ocamlbuild TP1.native
	mv TP1.native TP1
clean:
	ocamlbuild -clean
