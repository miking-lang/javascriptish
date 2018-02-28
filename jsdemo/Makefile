

all:
	ocamlbuild -pkgs js_of_ocaml code.byte
	js_of_ocaml code.byte
	node main.js
clean:
	rm -rf _build code.js code.byte
