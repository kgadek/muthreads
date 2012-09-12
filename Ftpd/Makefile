# FTP server with muthreads

OCAMLC=ocamlc
OCAMLOPT=ocamlopt

LIB=/usr/local/lib/ocaml

# utiles dans une install debian classique ?
PAMDIR=/usr/lib/ocaml/pam
PAMLIB=/usr/lib/ocaml/stublibs
#PAMDIR=/usr/local/godi/lib/ocaml/std-lib/pam
#PAMLIB=/usr/local/godi/lib/ocaml/std-lib/stublibs

# euid
COMPFLAGS=-warn-error A
MKLIB=ocamlmklib
#CAMLH_DIR=/usr/local/godi/lib/ocaml/std-lib/caml/
CAMLH_DIR=/usr/lib/ocaml/caml/
CFLAGS=-I$(CAMLH_DIR) -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)

clean:
	rm -f ftpd ftpd.opt *.aux *.log *.dvi *.ps
	rm -f *.cm* *.o *.a *.so

parse.mli: data.cmi parse.ml
	$(OCAMLC) -i parse.ml > parse.mli

aux.mli: data.cmi parse.cmi aux.ml
	$(OCAMLC) -i -I $(LIB) -I $(PAMDIR) aux.ml > aux.mli

# exécuter en root
# ocamlrun -I . ftpd
# ./ftpd.opt

CMI=data.cmi parse.cmi aux.cmi

ftpd: libeuid.a euid.cma ftpd.ml $(CMI) aux.cmo parse.cmo
	$(OCAMLC) -I $(LIB) -I . -I $(PAMDIR) -I $(PAMLIB) -o ftpd unix.cma pam.cma euid.cma muthr.cma parse.cmo aux.cmo ftpd.ml

ftpd.opt: libeuid.a euid.cmxa ftpd.ml $(CMI) aux.cmx parse.cmx
	$(OCAMLOPT) -I $(LIB) -I . -I $(PAMDIR) -I $(PAMLIB) -o ftpd.opt unix.cmxa euid.cmxa pam.cmxa muthr.cmxa parse.cmx aux.cmx ftpd.ml

parse.cmi: data.cmi

aux.cmo: aux.ml
	$(OCAMLC) -I $(LIB) -I $(PAMDIR) -c aux.ml

aux.cmx: aux.ml aux.cmi parse.cmi
	$(OCAMLOPT) -I $(LIB) -I $(PAMDIR) -c aux.ml

euid.cmo: euid.ml euid.cmi
	$(OCAMLC) -c $(COMPFLAGS) -labels $<

euid.cmx: euid.ml euid.cmi
	$(OCAMLOPT) -c $(COMPFLAGS) -labels $<

euid.cma: euid.cmo
	$(MKLIB) -o euid -ocamlc '$(OCAMLC)' -linkall euid.cmo


euid.cmxa: euid.cmx
	$(MKLIB) -o euid -ocamlopt '$(OCAMLOPT)' -linkall euid.cmx

# PIC necessary for amd64 only?
euid.o: euid.c
	gcc $(CFLAGS) -fPIC -c $<

libeuid.a: euid.o
	$(MKLIB) -o euid euid.o

# ...

%.cmo : %.ml %.cmi
	$(OCAMLC) -c $< -I $(LIB)

%.cmi : %.mli
	$(OCAMLC) -c $<  -I $(LIB)

%.cmx : %.ml %.cmi
	$(OCAMLOPT) -c $<


%.tex : %.ml
	ocamlweb --header -s --no-index --latex-option novisiblespaces $< > $@

%.dvi : %.tex
	latex $<

%.ps : %.dvi
	dvips $< -o $@



client: client.ml muthr.cmo
	$(OCAMLC) unix.cma muthr.cmo client.ml  -o client