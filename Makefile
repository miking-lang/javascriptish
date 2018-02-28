

all:
	ocamlbuild -pkgs js_of_ocaml ext2.byte && js_of_ocaml ext2.byte && node test.js
