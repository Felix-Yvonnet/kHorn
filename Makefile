NAME			= main

TESTSRCSAT			= $(shell cd tests/satisfiable/ && find -name '*.cnf')

TESTSRCUNSAT			= $(shell cd tests/unsatisfiable/ && find -name '*.cnf')

$(NAME):
	ocamlc -o $(NAME) src/$(NAME).ml
	rm src/*.c*

converter:
	ocamlc -o converter src/horn_converter.ml
	rm src/*.c* src/*.o

rapport:
	pdflatex src/rapport.tex 
	rm *.log *.aux
	firefox rapport.pdf


all_tests: $(TESTSRCSAT) $(TESTSRCUNSAT)

sat: 
	$(TESTSRCSAT) 
unsat: 
	$(TESTSRCUNSAT)


$(TESTSRCSAT): $(NAME)
	./$< tests/satisfiable/$@

$(TESTSRCUNSAT): $(NAME)
	./$< tests/unsatisfiable/$@

clean:
	rm main rapport.pdf converter

.PHONY: $(NAME) clean rapport 