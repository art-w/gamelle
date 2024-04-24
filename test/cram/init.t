  $ gamelle init funny_game
  $ ls
  funny_game
  $ cd funny_game
  $ ls
  Makefile
  assets
  dune
  dune-project
  funny_game.template.html
  src
TODO : make this test succeed
  $ dune build
  File "/home/emile/Projects/ocaml/gamelle/_build/install/default/lib/gamelle/dune-package", line 506, characters 6-171:
  506 |       (alias
  507 |        (obj_name gamelle__Ui__)
  508 |        (visibility public)
  509 |        (kind (alias (Ui)))
  510 |        (source (path Gamelle__Ui__) (impl (path gamelle__Ui__.ml-gen))))
  Error: Atom or quoted string expected
  -> required by _build/default/funny_game_bin.exe
  [1]
