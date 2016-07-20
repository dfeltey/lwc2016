all: the-paper

the-paper: paper/paper.scrbl paper/notation.scrbl paper/evolution.scrbl paper/editing.scrbl paper/mj-examples.rkt
	(cd paper; raco make -v paper.scrbl && scribble --pdf paper.scrbl; cd ..)
