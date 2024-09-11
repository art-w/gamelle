.PHONY: run html

run:
	dune build --root=. --watch _build/default/src/mygame.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/mygame.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release mygame.html
	xdg-open _build/default/mygame.html
