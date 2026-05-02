.PHONY: run html

run:
	dune build --root=. --watch _build/default/src/trigonometry_sprint.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/trigonometry_sprint.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release trigonometry_sprint.html
	xdg-open _build/default/trigonometry_sprint.html
