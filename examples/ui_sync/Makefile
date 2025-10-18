.PHONY: run html

run:
	dune build --root=. --watch _build/default/src/ui_sync.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/ui_sync.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release ui_sync.html
	xdg-open _build/default/ui_sync.html
