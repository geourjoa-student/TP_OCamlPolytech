# ENVIRONMENT
INCLUDES=-I +lablGL
LABLGLCMAS=lablgl.cma lablglut.cma unix.cma

# BUILD RULES
all: testdessin koch peano

# Les règles pour compiler koch puis les autres fractales sont à rajouter

testdessin: aff.cmo geo.cmo testdessin.cmo
	ocamlc $(INCLUDES) $(LABLGLCMAS) geo.cmo aff.cmo testdessin.cmo -o testdessin

testdessin.cmo: testdessin.ml aff.cmo geo.cmo
	ocamlc -c testdessin.ml

geo.cmi: geo.mli
	ocamlc -c geo.mli
geo.cmo: geo.ml geo.cmi
	ocamlc -c geo.ml

aff.cmi: aff.mli geo.cmi
	ocamlc -c aff.mli
aff.cmo: aff.cmi geo.cmo
	ocamlc $(INCLUDES) -c aff.ml

koch : koch.cmo geo.cmo aff.cmo
	ocamlc $(INCLUDES) $(LABLGLCMAS) geo.cmo aff.cmo koch.cmo -o koch


koch.cmo : koch.ml aff.cmo geo.cmo
	ocamlc -c koch.ml

peano : peano.cmo geo.cmo aff.cmo
	ocamlc $(INCLUDES) $(LABLGLCMAS) geo.cmo aff.cmo peano.cmo -o peano

peano.cmo : peano.ml aff.cmo geo.cmo
	ocamlc -c peano.ml
clean:
	rm -f *.cmi *.cmo *~ peano koch testdessin
