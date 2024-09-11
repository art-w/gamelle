.PHONY: run html

run:
	dune build --root=.
	gamelle hotreload _build/default/src/snake.cmxs &
	dune build --root=. --watch

html:
	dune build --root=. --profile=release snake.html
	xdg-open _build/default/snake.html
