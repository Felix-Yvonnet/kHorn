NAME			= main

TESTSRC			= $(shell cd tests/satisfiable/ && find -name '*.cnf')
TESTSRCNOEXT	= $(SRC:.cnf=)

$(NAME):
	ocamlc -o $(NAME) src/$(NAME).ml
	rm src/*.c*

converter:
	ocamlc -o src/horn_converter.ml
	rm src/*.c* src/*.o

rapport:
	pdflatex src/rapport.tex 
	rm *.log *.aux
	firefox rapport.pdf


all_tests: $(TESTSRC)

$(TESTSRC): $(NAME)
	./$< tests/satisfiable/$@


clean:
	rm main rapport.pdf

.PHONY: $(NAME) clean rapport