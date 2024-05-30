.PHONY: run html

run:
	dune build
	gamelle hotreload _build/default/src/ui.cmxs &
	dune build --watch

html:
	dune build --profile=release
	xdg-open _build/default/ui.html
