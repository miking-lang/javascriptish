

.PHONY : all test clean


all:
	ocamlbuild src/jsh.byte
	js_of_ocaml jsh.byte

clean:
	rm -rf _build jsh.js jsh.byte
