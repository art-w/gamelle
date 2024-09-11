.PHONY: run html

run:
	dune build
	gamelle hotreload _build/default/src/ui.cmxs &
	dune build --watch

html:
	dune build --root=. --profile=release ui.html
	xdg-open _build/default/ui.html
