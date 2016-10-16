all: paper/paper.pdf

paper/paper.pdf: paper/paper.scrbl paper/notation.scrbl paper/evolution.scrbl paper/editing.scrbl paper/mj-examples.rkt paper/minijava.scrbl paper/racket.scrbl paper/conclusion.scrbl paper/setup.rkt paper/intro.scrbl paper/bib.rkt
	(cd paper; racket check-fonts.rkt && raco make -v paper.scrbl && scribble --pdf paper.scrbl; cd ..)
