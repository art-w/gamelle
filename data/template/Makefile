.PHONY: run html

run:
	dune build --root=.
	gamelle hotreload _build/default/src/mygame.cmxs &
	dune build --root=. --watch

html:
	dune build --root=. --profile=release
	xdg-open _build/default/mygame.html
