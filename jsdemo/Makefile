

all:
	ocamlbuild -pkgs js_of_ocaml code.byte
	js_of_ocaml code.byte
	node main.js
# browserify mainweb.js -o mainweb_bf.js

clean:
	rm -rf _build code.js code.byte mainweb_bf.js
