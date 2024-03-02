.PHONY: run html

run:
	dune build
	gamelle hotreload _build/default/game/mygame.cmxs &
	dune build --watch

html:
	dune build
	xdg-open _build/default/bin/index.html
