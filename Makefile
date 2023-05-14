main:
	ocamlopt -o main src/parser/dimacs.ml src/main.ml
	rm *.c* *.o

parser:
	ocamlc -o src/parser/horn_converter.ml
	rm *.c* *.o

rapport:
	pdflatex src/rapport.tex 
	rm *.log *.aux
	firefox rapport.pdf

clean:
	rm main

.PHONY: main