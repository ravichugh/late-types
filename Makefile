NAME=late-types

all: build

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure 

build: setup.data setup.ml
	ocaml setup.ml -build

install: setup.data setup.ml
	ocaml setup.ml -install

test: setup.ml build
	_build/test/Test.byte inline-test-runner netkat

reinstall: setup.ml
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log

distclean:
	ocaml setup.ml -distclean
	rm -f setup.data setup.log
