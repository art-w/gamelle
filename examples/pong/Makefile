.PHONY: run html

run:
	dune build --root=.
	gamelle hotreload _build/default/game/pong.cmxs &
	dune build --root=. --watch

html:
	dune build --root=.
	xdg-open _build/default/bin/index.html
