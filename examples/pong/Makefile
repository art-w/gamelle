.PHONY: run html

run:
	dune build
	gamelle hotreload _build/default/src/pong.cmxs &
	dune build --watch

html:
	dune build --profile=release
	xdg-open _build/default/pong.html
