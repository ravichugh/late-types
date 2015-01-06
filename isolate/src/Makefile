
#LIBS=-libs unix,str
LIBS=
PARSE=-use-menhir -yaccflag --explain # set to empty for ocamlyacc

all:
	#([ -d out ] && echo "out/ exists") || (mkdir out && echo "out/ created")
	rm -f _build/langParser.conflicts
	ocamlbuild $(LIBS) $(PARSE) -no-links main.native
	ln -f -s _build/main.native isolate

debug:
	ocamlbuild $(LIBS) $(PARSE) -no-links main.d.byte

clean:
	rm -rf _build out isolate

